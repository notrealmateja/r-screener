# The nightly workflow embeds R inline in YAML, where nothing exercises it until
# the job runs for real. That is how `object 'csv_files' not found` reached
# production: the allowlist change removed the variable and left one reference
# behind, so the step aborted after doing all of its work. These tests run the
# static check locally, and — more importantly — confirm the check still detects
# that exact mistake, so the guard cannot silently rot into a no-op.

lint <- repo_path("tools", "lint_workflow_r.R")
wf   <- repo_path(".github", "workflows")

run_lint <- function(dir) {
  out <- suppressWarnings(system2("Rscript", c(shQuote(lint), shQuote(dir)),
                                  stdout = TRUE, stderr = TRUE))
  list(status = attr(out, "status") %||% 0L, text = paste(out, collapse = "\n"))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

test_that("every inline R block in the workflows parses and resolves", {
  skip_if_not(file.exists(lint), "linter not present")
  skip_if_not(dir.exists(wf), "no workflows directory")
  r <- run_lint(wf)
  expect_equal(r$status, 0L, info = r$text)
})

test_that("the linter still catches a dangling variable reference", {
  skip_if_not(file.exists(lint), "linter not present")
  skip_if_not(dir.exists(wf), "no workflows directory")
  src <- list.files(wf, pattern = "\\.ya?ml$", full.names = TRUE)[1]
  skip_if(is.na(src), "no workflow file")

  tmp <- file.path(tempdir(), "wflint"); dir.create(tmp, showWarnings = FALSE)
  dst <- file.path(tmp, basename(src))
  txt <- readLines(src, warn = FALSE)

  # Inject a read of a name that is never assigned, inside an existing block.
  hit <- grep("^\\s*message\\(", txt)[1]
  skip_if(is.na(hit), "no message() line to amend")
  txt[hit] <- sub("message\\(", "message(a_name_that_is_never_assigned, ", txt[hit])
  writeLines(txt, dst)

  r <- run_lint(tmp)
  expect_equal(r$status, 1L, info = r$text)
  expect_true(grepl("a_name_that_is_never_assigned", r$text, fixed = TRUE))
})

# Found by probing the linter itself during the audit. It closed a block on the
# next line whose only content is a quote — but a one-liner closes on its OWN
# line, so the scan ran past it and swallowed everything up to the following
# block's terminator. The report came out as a syntax error quoting raw YAML,
# and the swallowed block was never checked, so a genuine dangling reference
# inside it went unreported.
test_that("a one-line Rscript -e block does not swallow the next one", {
  skip_if_not(file.exists(lint), "linter not present")
  tmp <- file.path(tempdir(), "wflint_oneline")
  dir.create(tmp, showWarnings = FALSE)
  writeLines(c(
    "jobs:", "  x:", "    steps:",
    "      - name: one-liner",
    "        run: Rscript -e 'message(undefined_one)'",
    "      - name: later block",
    "        run: |",
    "          Rscript -e '",
    "            message(undefined_two)",
    "          '"
  ), file.path(tmp, "a.yml"))

  r <- run_lint(tmp)
  expect_equal(r$status, 1L, info = r$text)
  expect_true(grepl("undefined_one", r$text, fixed = TRUE))
  expect_true(grepl("undefined_two", r$text, fixed = TRUE))  # was never reached
  expect_false(grepl("syntax", r$text, fixed = TRUE))        # not a YAML parse error
})

test_that("an unterminated block is named rather than mis-sliced", {
  skip_if_not(file.exists(lint), "linter not present")
  tmp <- file.path(tempdir(), "wflint_unterminated")
  dir.create(tmp, showWarnings = FALSE)
  writeLines(c(
    "jobs:", "  x:", "    steps:", "      - run: |",
    "          Rscript -e '",
    "            message(\"never closed\")"
  ), file.path(tmp, "a.yml"))

  r <- run_lint(tmp)
  expect_equal(r$status, 1L, info = r$text)
  expect_true(grepl("unterminated", r$text, fixed = TRUE))
})

# The R in this workflow is embedded in the shell as Rscript -e '...', so the
# file has to be valid SHELL before any of it is valid R. Parsing the YAML, as
# the linter did, cannot see that distinction: the R is fine, bash is what
# objects.
#
# The word "day's" in a comment closed that quote and took the nightly down,
# with an error naming a line that was perfectly correct:
#     line 86: syntax error near unexpected token `"data/price_history.csv"'
# Three nights failed before anyone read the log. `bash -n` reproduces it in
# milliseconds, so it runs here.
test_that("every run: block in the workflow is valid shell", {
  wf <- repo_path(".github", "workflows")
  skip_if_not(dir.exists(wf), "no workflows directory")
  skip_if(Sys.which("bash") == "", "bash unavailable")

  for (f in list.files(wf, pattern = "\\.ya?ml$", full.names = TRUE)) {
    lines <- readLines(f, warn = FALSE)
    cur <- NULL; indent <- NA_integer_; blocks <- list(); starts <- integer(0)
    for (i in seq_along(lines)) {
      l <- lines[i]
      if (grepl("^\\s*run: \\|\\s*$", l)) {
        if (!is.null(cur)) { blocks[[length(blocks) + 1]] <- cur }
        cur <- character(0); indent <- NA_integer_
        starts <- c(starts, i); next
      }
      if (is.null(cur)) next
      if (!nzchar(trimws(l))) { cur <- c(cur, ""); next }
      ind <- nchar(sub("[^ ].*$", "", l))
      if (is.na(indent)) indent <- ind
      if (ind < indent) { blocks[[length(blocks) + 1]] <- cur; cur <- NULL; next }
      cur <- c(cur, substring(l, indent + 1))
    }
    if (!is.null(cur)) blocks[[length(blocks) + 1]] <- cur

    for (k in seq_along(blocks)) {
      tmp <- tempfile(fileext = ".sh")
      writeLines(blocks[[k]], tmp)
      out <- suppressWarnings(system2("bash", c("-n", shQuote(tmp)),
                                      stdout = TRUE, stderr = TRUE))
      st <- attr(out, "status")
      unlink(tmp)
      expect_true(is.null(st) || st == 0L,
                  info = sprintf("%s run: block starting line %d is not valid shell:\n%s",
                                 basename(f), starts[k], paste(out, collapse = "\n")))
    }
  }
})

# The Python linter is a separate tool because the embedded Python lives in shell
# heredocs rather than `Rscript -e` blocks. Same reasoning as above: those blocks
# run only when a nightly has ALREADY failed, and the step that runs them ends in
# `|| true; exit 0` so that reporting a failure can never fail the job. A syntax
# error there is therefore invisible — the alert just never arrives.

py_lint <- repo_path("tools", "lint_workflow_py.py")

run_py_lint <- function(dir) {
  out <- suppressWarnings(system2("python3", c("-I", shQuote(py_lint), shQuote(dir)),
                                  stdout = TRUE, stderr = TRUE))
  list(status = attr(out, "status") %||% 0L, text = paste(out, collapse = "\n"))
}

test_that("every embedded Python heredoc in the workflows compiles", {
  skip_if_not(file.exists(py_lint), "python linter not present")
  skip_if_not(dir.exists(wf), "no workflows directory")
  r <- run_py_lint(wf)
  expect_equal(r$status, 0L, info = r$text)
})

test_that("the Python linter finds the blocks rather than reporting a vacuous pass", {
  skip_if_not(file.exists(py_lint), "python linter not present")
  skip_if_not(dir.exists(wf), "no workflows directory")
  r <- run_py_lint(wf)
  # A linter that matches nothing exits 0 too. Insist it actually saw the
  # reporters, so the guard cannot quietly become a no-op if the heredoc style
  # changes.
  expect_match(r$text, "report_failure\\.py")
  expect_match(r$text, "resolve_failure\\.py")
})

test_that("the Python linter still catches a syntax error", {
  skip_if_not(file.exists(py_lint), "python linter not present")
  src <- list.files(wf, pattern = "\\.ya?ml$", full.names = TRUE)[1]
  skip_if(is.na(src), "no workflow file")
  tmp <- file.path(tempdir(), "pylint-check")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  lines <- readLines(src, warn = FALSE)
  # Break the first reporter the way a careless edit would: an unclosed paren.
  at <- grep("^\\s*token = os\\.environ\\[\"GH_TOKEN\"\\]\\s*$", lines)
  skip_if(length(at) == 0L, "reporter body not found")
  lines[at[1]] <- sub("os\\.environ\\[\"GH_TOKEN\"\\]",
                      "os.environ[\"GH_TOKEN\"", lines[at[1]])
  writeLines(lines, file.path(tmp, "broken.yml"))

  r <- run_py_lint(tmp)
  expect_equal(r$status, 1L, info = r$text)
  expect_match(r$text, "FAIL")
})
