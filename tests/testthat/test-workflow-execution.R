# Does the workflow's embedded R actually RUN?
#
# Every other test in this suite asks what the nightly's inline R *says*. None
# asked whether R would agree to execute it, and that gap cost seven nights.
#
# R caps an `Rscript -e` expression, but NOT at a count of the bytes you wrote.
# `$R_HOME/bin/R` is a shell script, and before R sees the argument it rewrites
# every space as `~+~`, every newline as `~n~` and every tab as `~t~` (lines
# 196-197), so each of those costs THREE bytes against the limit. Over it, R
# prints `WARNING: '-e <encoded expression>'` to STDOUT (not stderr), discards
# the expression, and falls through to reading a program from stdin -- which on
# a runner is /dev/null, so it reads EOF and EXITS 0, having run nothing.
#
# The "Verify pipeline output" step went silent at commit 6e17f66 on
# 2026-10-26: 9109 raw bytes, which any raw-byte threshold would have passed,
# but 13871 encoded. First affected nightly is run #246 on 2026-10-27, whose
# verify step emitted the WARNING and nothing else; run #245 the night before
# had failed loudly with a bash syntax error. 6e17f66 is titled "An apostrophe
# in a comment was taking the nightly down" -- fixing the apostrophe converted
# a loud failure into a silent one.
#
# Seven nightly commits then published with nothing verified: the core-file
# checks, the run-stamp provenance check, the feed health check, the dark_feeds
# output and the step-summary table all lived in that block.
#
# Why the existing tests passed throughout: test-pipeline-provenance.R asserts
# the text is PRESENT and executes extracted FRAGMENTS in-process, and
# test-workflow-lint.R only parsed the block. Neither form proves R accepts the
# assembled expression, and `parse()` never will -- the R is valid R, and only
# R's own argument handling refuses it, at invocation time.

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

# The ceiling applies to the ENCODED size. Measured on R 4.5.2 by bisection:
# 9997 encoded bytes run, 9998 is refused, and an all-space pad hits the same
# ceiling at exactly one third of the raw bytes a whitespace-free pad does.
# It is a property of the R build, so the linter's own fail threshold sits well
# below this; here we only assert nothing is near the measured cliff.
E_LIMIT_ENCODED <- 9997L

# What bin/R hands the length check: space, newline and tab cost 3 bytes each.
e_encoded_len <- function(code) {
  joined <- paste(code, collapse = "\n")
  nchar(joined, type = "bytes") +
    2L * lengths(regmatches(joined, gregexpr("[ \t\n]", joined)))
}

# Every Rscript -e block in a workflow, sized as bash receives it.
#
# Deliberately a second implementation, independent of the linter's: the one
# thing that has already slipped through is exactly what a shared helper would
# hide. It also takes the opposite route on purpose -- de-indent the whole
# `run:` block the way YAML does, THEN pull the quoted argument out of the
# resulting shell script, where the linter finds the argument first and
# de-indents that. Same specification, different order, so a mistake in either
# shows up as disagreement.
#
# The indentation detail is the one that matters: YAML strips a block scalar by
# the indent of its first non-empty line and no more, so whatever indentation
# the R carries ON TOP of that is part of the argument and costs 3 bytes per
# space. De-indenting the R body before measuring understates every block.
indent_of <- function(s) nchar(sub("[^ ].*$", "", s))

e_blocks <- function(lines) {
  out <- list()
  i <- 1L
  while (i <= length(lines)) {
    if (!grepl("^[[:space:]]*run:[[:space:]]*[|>][+-]?[[:space:]]*$", lines[i])) {
      i <- i + 1L
      next
    }
    k <- i + 1L
    while (k <= length(lines) && !nzchar(trimws(lines[k]))) k <- k + 1L
    if (k > length(lines)) break
    base <- indent_of(lines[k])
    j <- k
    while (j <= length(lines) &&
           (!nzchar(trimws(lines[j])) || indent_of(lines[j]) >= base)) j <- j + 1L
    script <- substring(lines[(i + 1L):(j - 1L)], base + 1L)

    m <- 1L
    while (m <= length(script)) {
      if (!grepl("Rscript -e '", script[m], fixed = TRUE)) {
        m <- m + 1L
        next
      }
      after <- sub("^.*Rscript -e '", "", script[m])
      if (grepl("'", after, fixed = TRUE)) {
        m <- m + 1L
        next                                    # one-liner, closes itself
      }
      e <- m + 1L
      while (e <= length(script) && !grepl("^'[[:space:]]*$", script[e])) e <- e + 1L
      body <- if (e > m + 1L) script[(m + 1L):(e - 1L)] else character(0)
      # The newline after the opening quote and the one before the closing
      # quote are both inside the argument.
      arg <- paste0("\n", paste(body, collapse = "\n"), "\n")
      out[[length(out) + 1L]] <- list(
        line = i + m, n = length(body),
        bytes = nchar(arg, type = "bytes"),
        encoded = e_encoded_len(arg))
      m <- e + 1L
    }
    i <- j
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

test_that("the limit counts encoded bytes, not the bytes you wrote", {
  # Two payloads of near-identical raw size. One is all x, the other all
  # spaces. If the limit were a raw byte count both would behave the same;
  # because bin/R triples whitespace, only the spaces overflow. This is the
  # detail that put the real start of the outage four nights earlier than a
  # raw-byte reading suggests, and it is why the linter encodes before it
  # measures.
  run_e <- function(e)
    suppressWarnings(system2("Rscript", c("-e", shQuote(e)),
                             stdout = TRUE, stderr = TRUE, stdin = "/dev/null"))
  dense  <- paste0("# ", strrep("x", 6000), "\ncat(\"DENSE\\n\")")
  spacey <- paste0("# ", strrep(" ", 6000), "\ncat(\"LOOSE\\n\")")
  expect_equal(nchar(dense, type = "bytes"), nchar(spacey, type = "bytes"))

  expect_true(any(grepl("DENSE", run_e(dense), fixed = TRUE)),
              info = "a dense 6KB expression should run")
  out <- run_e(spacey)
  expect_false(any(grepl("LOOSE", out, fixed = TRUE)),
               info = "6000 spaces encode to ~18KB and must be discarded")
  expect_true(any(grepl("WARNING", out, fixed = TRUE)))
})

test_that("the -e budget is shared across arguments, not per argument", {
  # A step passing two -e arguments must fit both in one budget. Nothing in
  # the workflow does today, and this records why it must not start.
  run2 <- function(n) {
    pad <- function(tag) paste0("# ", strrep("x", n), "\ncat(\"", tag, "\\n\")")
    suppressWarnings(system2("Rscript",
      c("-e", shQuote(pad("A")), "-e", shQuote(pad("B"))),
      stdout = TRUE, stderr = TRUE, stdin = "/dev/null"))
  }
  both <- run2(4000)
  expect_true(any(grepl("^A$", both)))
  expect_true(any(grepl("^B$", both)))

  split <- run2(5100)
  expect_true(any(grepl("^A$", split)), info = "the first argument should still run")
  expect_false(any(grepl("^B$", split)),
               info = "two 5.1KB arguments exceed one shared budget")
})

test_that("no workflow hands R an expression it will discard", {
  skip_if_not(dir.exists(wf_dir), "no workflows directory")
  files <- list.files(wf_dir, pattern = "\\.ya?ml$", full.names = TRUE)
  expect_gt(length(files), 0)
  for (f in files) {
    for (b in e_blocks(readLines(f, warn = FALSE))) {
      expect_lt(b$encoded, E_LIMIT_ENCODED,
                label = sprintf("%s:%d (%d lines) is %d bytes, %d encoded; R discards it and exits 0",
                                basename(f), b$line, b$n, b$bytes, b$encoded))
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

test_that("the linter counts the indentation YAML leaves behind", {
  skip_if_not(file.exists(lint), "linter not present")
  # The second regression in the same check, and subtler than the first: the
  # linter de-indented the R body before measuring it, by the body's own
  # minimum indent. YAML does not do that. A block scalar is stripped by the
  # indent of its FIRST non-empty line -- here the `Rscript -e '` line at 10 --
  # and every space the R carries on top of that is in the argument, costing 3
  # bytes once bin/R encodes it.
  #
  # The figures below are hand-computed rather than taken from the linter, so
  # this fails if the linter measures anything other than what bash passes:
  #   500 body lines of `  cat("x")`, 10 bytes each after the 10-space strip
  #   argument = "\n" + 500 lines joined by "\n" + "\n" = 11*500 + 1 = 5501
  #   whitespace = 1000 spaces + 501 newlines = 1501, at +2 bytes each
  #   encoded = 5501 + 3002 = 8503
  # Raw 5501 is comfortably under the 8000 threshold; encoded 8503 is over it.
  # De-indenting first would leave 8 bytes a line, 4499 raw and 5497 encoded,
  # and the block would be reported "ok" -- which is what this test exists to
  # stop. The arithmetic is spelled out because getting it wrong once already
  # produced a check that measured the wrong thing while looking right.
  n <- 500L
  d <- file.path(tempdir(), paste0("lintind-", basename(tempfile(""))))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(c("name: t", "jobs:", "  j:", "    steps:",
               "      - name: Indented",
               "        run: |",
               "          Rscript -e '",
               rep('            cat("x")', n),
               "          '"),
             file.path(d, "indent.yml"))

  arg <- paste0("\n", paste(rep('  cat("x")', n), collapse = "\n"), "\n")
  expect_equal(nchar(arg, type = "bytes"), 5501L)
  expect_equal(e_encoded_len(arg), 8503L)
  expect_lt(nchar(arg, type = "bytes"), 8000L)   # a raw check lets it through

  r <- run_lint(d)
  expect_equal(r$status, 1L, info = r$text)
  expect_match(r$text, "5501 bytes, 8503 once", fixed = TRUE, info = r$text)
})

test_that("the linter refuses a block that only overflows once encoded", {
  skip_if_not(file.exists(lint), "linter not present")
  # The regression this guards: the first version of the size check measured
  # RAW bytes against 8000. Deeply indented R is roughly 1.5x its raw size
  # once encoded, so a block well under the raw threshold could still be
  # discarded by R -- which is exactly how 6e17f66 slipped through at 9109 raw
  # bytes. The fixture below is ~4.9KB raw and ~11.6KB encoded, and the
  # raw-only check passed it.
  d <- file.path(tempdir(), paste0("lintenc-", basename(tempfile(""))))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  deep <- paste0("            ", strrep(" ", 40), "# ", strrep("z", 18))
  writeLines(c("name: t", "jobs:", "  j:", "    steps:",
               "      - name: Deep", "        run: |", "          Rscript -e '",
               rep(deep, 80), '            cat("hi")', "            '"),
             file.path(d, "deep.yml"))

  body <- c(rep(substring(deep, 13), 80), 'cat("hi")')
  expect_lt(sum(nchar(body, type = "bytes")) + length(body) - 1L, 8000L)
  expect_gt(e_encoded_len(body), 8000L)

  r <- run_lint(d)
  expect_equal(r$status, 1L, info = r$text)
  expect_match(r$text, "encode", info = r$text)
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
