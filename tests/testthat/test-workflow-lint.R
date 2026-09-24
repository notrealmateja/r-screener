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
