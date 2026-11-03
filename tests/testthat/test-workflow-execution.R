# Does the workflow's embedded R actually RUN?
#
# Every other test in this suite asks what the nightly's inline R *says*. None
# asked whether R would agree to execute it, and that gap cost three nights.
#
# R caps an -e expression at 10000 bytes. Past the limit it prints
#     WARNING: '-e <mangled expression>'
# discards the expression, and EXITS 0. Nothing runs and the step reports
# success. Parsing cannot see this: the R is valid R, and only R's own argument
# handling refuses it, at invocation time.
#
# The "Verify pipeline output" step crossed the limit at commit 856380c on
# 2026-10-31 - the commit whose stated purpose was to stop the nightly blessing
# data it never generated. For three nights the step silently ran nothing while
# reporting success: no core-file checks, no provenance check, no upstream feed
# health check, and no dark_feeds output, so "Fail on a dark upstream feed" was
# skipped every night while both Alpha Vantage feeds sat 28 days dark. The data
# was real, but nothing had checked it.
#
# test-pipeline-provenance.R and test-workflow-lint.R both passed throughout.
# They asserted the text was present and executed extracted fragments in
# isolation, which works regardless of whether the whole block ever runs.

lint <- repo_path("tools", "lint_workflow_r.R")
wf_dir <- repo_path(".github", "workflows")
wf_file <- repo_path(".github", "workflows", "daily-update.yml")
vp_file <- repo_path("tools", "verify_pipeline.R")

`%||%` <- function(a, b) if (is.null(a)) b else a

run_lint <- function(dir) {
  out <- suppressWarnings(system2("Rscript", c(shQuote(lint), shQuote(dir)),
                                  stdout = TRUE, stderr = TRUE,
                                  stdin = "/dev/null"))
  list(status = attr(out, "status") %||% 0L, text = paste(out, collapse = "\n"))
}

# R's documented -e ceiling. The linter refuses well below it; see E_LIMIT_FAIL.
E_LIMIT <- 10000L

# Every Rscript -e block in a workflow, sized as bash receives it. Deliberately
# a second implementation, independent of the linter's: the one thing that has
# already slipped through is exactly the thing a shared helper would hide.
e_blocks <- function(lines) {
  out <- list()
  i <- 1L
  while (i <= length(lines)) {
    if (grepl("Rscript -e '", lines[i], fixed = TRUE)) {
      after <- sub("^.*Rscript -e '", "", lines[i])
      if (grepl("'", after, fixed = TRUE)) {
        i <- i + 1L
        next                                    # one-liner, closes itself
      }
      j <- i + 1L
      while (j <= length(lines) && !grepl("^\\s*'\\s*$", lines[j])) j <- j + 1L
      body <- if (j > i + 1L) lines[(i + 1L):(j - 1L)] else character(0)
      keep <- nzchar(trimws(body))
      if (any(keep)) {
        indent <- min(nchar(sub("[^ ].*$", "", body[keep])))
        body <- substring(body, indent + 1L)
      }
      nbytes <- sum(nchar(body, type = "bytes")) + max(0L, length(body) - 1L)
      out[[length(out) + 1L]] <- list(line = i, bytes = nbytes, n = length(body))
      i <- j + 1L
    } else i <- i + 1L
  }
  out
}

# ── The hazard itself ───────────────────────────────────────────────────────

test_that("R silently discards an oversized -e expression", {
  # Pins the mechanism every other test here defends against, and reports it if
  # a future R changes the limit or starts failing loudly instead.
  expr <- function(nbytes, marker) {
    pad <- paste(rep(paste0("# ", strrep("x", 76)), ceiling(nbytes / 79)),
                 collapse = "\n")
    paste0(substr(pad, 1, max(1, nbytes - 40)), "\n",
           sprintf('cat("%s\\n")', marker))
  }
  # stdin must be /dev/null, and that detail is the rest of the explanation for
  # why the nightly went green. Having discarded the expression R has nothing
  # to evaluate, so it falls through to reading a program from stdin. On a
  # terminal or a pipe it waits there forever; on the runner stdin is
  # /dev/null, so it reads EOF, finds nothing to do, and exits 0.
  run_e <- function(e)
    suppressWarnings(system2("Rscript", c("-e", shQuote(e)),
                             stdout = TRUE, stderr = TRUE, stdin = "/dev/null"))
  small <- run_e(expr(4000,  "RAN-SMALL"))
  big   <- run_e(expr(14000, "RAN-BIG"))

  # Under the limit: the expression runs.
  expect_true(any(grepl("RAN-SMALL", small, fixed = TRUE)),
              info = paste(utils::head(small, 3), collapse = " | "))

  # Over it: nothing runs, R warns, and — the dangerous part — exit is 0.
  expect_false(any(grepl("RAN-BIG", big, fixed = TRUE)),
               info = "an oversized -e expression unexpectedly executed")
  expect_true(any(grepl("WARNING", big, fixed = TRUE)),
              info = paste(utils::head(big, 3), collapse = " | "))
  expect_equal(attr(big, "status") %||% 0L, 0L,
               info = "R now fails loudly on an oversized -e; the guards can be relaxed")
})

test_that("no workflow hands R an expression it will discard", {
  skip_if_not(dir.exists(wf_dir), "no workflows directory")
  files <- list.files(wf_dir, pattern = "\\.ya?ml$", full.names = TRUE)
  expect_gt(length(files), 0)
  for (f in files) {
    for (b in e_blocks(readLines(f, warn = FALSE))) {
      expect_lt(b$bytes, E_LIMIT,
                label = sprintf("%s:%d (%d lines) is %d bytes; R discards it and exits 0",
                                basename(f), b$line, b$n, b$bytes))
    }
  }
})

# ── The verify program ──────────────────────────────────────────────────────

test_that("the verify program is a script file, not an -e argument", {
  skip_if_not(file.exists(wf_file), "workflow file not present")
  expect_true(file.exists(vp_file))
  expect_silent(invisible(parse(vp_file)))
  # A script file is immune: no length limit, and a syntax error exits
  # non-zero naming a line instead of being discarded in silence.
  wf <- readLines(wf_file, warn = FALSE)
  at <- grep("^\\s*- name:\\s*Verify pipeline output\\s*$", wf)
  expect_length(at, 1L)
  j <- at + 1L
  body <- character(0)
  while (j <= length(wf) && !grepl("^\\s*- name:", wf[j])) {
    body <- c(body, wf[j]); j <- j + 1L
  }
  code <- grep("^\\s*#", body, invert = TRUE, value = TRUE)
  expect_match(paste(code, collapse = " "), "Rscript +tools/verify_pipeline\\.R")
  expect_false(grepl("Rscript -e", paste(code, collapse = " "), fixed = TRUE))
})

test_that("the verify program records that it reached a verdict", {
  skip_if_not(file.exists(vp_file), "tools/verify_pipeline.R not present")
  # Executed, not grepped. On a bare tree every check fails, which is fine —
  # what matters is that the program ran at all and said so. Under the old
  # inline form there was nothing on disk to run.
  d <- file.path(tempdir(), paste0("vp-", basename(tempfile(""))))
  dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)
  gh <- file.path(d, "gh_out")
  file.create(gh)
  old <- setwd(d); on.exit(setwd(old), add = TRUE)
  st <- suppressWarnings(system2("Rscript", shQuote(vp_file),
                                 stdout = NULL, stderr = NULL,
                                 stdin = "/dev/null",
                                 env = c(sprintf("GITHUB_OUTPUT=%s", gh),
                                         "GITHUB_STEP_SUMMARY=",
                                         "GITHUB_RUN_ID=7")))
  wrote <- readLines(gh, warn = FALSE)
  expect_true(any(grepl("^verify_ran=1$", wrote)),
              info = paste("GITHUB_OUTPUT held:", paste(wrote, collapse = " | ")))
  # And a tree with no data at all must be fatal, not a pass.
  expect_gt(st, 0L)
})

test_that("an absent verdict blocks publication", {
  skip_if_not(file.exists(wf_file), "workflow file not present")
  wf <- readLines(wf_file, warn = FALSE)
  # The gate step exists, keys off verify_ran, and fails when it is not 1.
  at <- grep("^\\s*- name:\\s*Confirm the verifier reached a verdict\\s*$", wf)
  expect_length(at, 1L)
  j <- at + 1L
  body <- character(0)
  while (j <= length(wf) && !grepl("^\\s*- name:", wf[j])) {
    body <- c(body, wf[j]); j <- j + 1L
  }
  txt <- paste(body, collapse = "\n")
  expect_match(txt, "id: verdict", fixed = TRUE)
  expect_match(txt, "steps.verify.outputs.verify_ran", fixed = TRUE)
  expect_match(txt, "exit 1", fixed = TRUE)
  # Publication must depend on it. Without this the gate is advisory and the
  # run still ships data nothing checked.
  commit <- grep("^\\s*- name:\\s*Clean stale files and commit updated data\\s*$", wf)
  expect_length(commit, 1L)
  # Scan to the step boundary, not a fixed window: this step carries nearly
  # twenty lines of comment before its condition.
  cond <- NA_character_
  k <- commit + 1L
  while (k <= length(wf) && !grepl("^\\s*- name:", wf[k])) {
    if (grepl("^\\s*if:", wf[k])) { cond <- wf[k]; break }
    k <- k + 1L
  }
  expect_false(is.na(cond))
  expect_match(cond, "steps.verdict.outcome == 'success'", fixed = TRUE)
})

# ── The linter must catch it next time ──────────────────────────────────────

test_that("the linter refuses an -e block R would discard", {
  skip_if_not(file.exists(lint), "linter not present")
  d <- file.path(tempdir(), paste0("lintsz-", basename(tempfile(""))))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  pad <- paste(rep(paste0("            # ", strrep("y", 70)), 160), collapse = "\n")
  writeLines(c("name: t", "jobs:", "  j:", "    steps:",
               "      - name: Big", "        run: |", "          Rscript -e '",
               pad,
               '            cat("hi")', "            '"),
             file.path(d, "big.yml"))
  r <- run_lint(d)
  expect_equal(r$status, 1L, info = r$text)
  expect_match(r$text, "limit", info = r$text)
})

test_that("the linter checks the script files the workflow calls", {
  skip_if_not(file.exists(lint), "linter not present")
  # Moving a program into a file must not move it out of the linter's sight,
  # which is the obvious way to "fix" an oversized block and lose the check.
  d <- file.path(tempdir(), paste0("lintsc-", basename(tempfile(""))))
  dir.create(file.path(d, ".github", "workflows"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(d, "tools"), recursive = TRUE, showWarnings = FALSE)
  yml <- file.path(d, ".github", "workflows", "w.yml")
  writeLines(c("name: t", "jobs:", "  j:", "    steps:",
               "      - name: Go", "        run: Rscript tools/thing.R"), yml)

  writeLines("cat(1)", file.path(d, "tools", "thing.R"))
  expect_equal(run_lint(dirname(yml))$status, 0L)

  # A syntax error in the called script is caught.
  writeLines("cat(1", file.path(d, "tools", "thing.R"))
  bad <- run_lint(dirname(yml))
  expect_equal(bad$status, 1L, info = bad$text)

  # So is a script the workflow names but that does not exist.
  file.remove(file.path(d, "tools", "thing.R"))
  gone <- run_lint(dirname(yml))
  expect_equal(gone$status, 1L, info = gone$text)
  expect_match(gone$text, "does not exist", info = gone$text)
})
