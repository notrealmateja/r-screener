#!/usr/bin/env Rscript
# Static check for the R embedded in .github/workflows/*.yml.
#
# The inline `Rscript -e '...'` blocks are never executed until the nightly job
# runs, so a typo there is only discovered in production. That is exactly how
# `object 'csv_files' not found` shipped: the allowlist change deleted the
# variable but left a reference to it, and the step aborted after doing its
# work. This parses every block and reports symbols that are read but never
# assigned, which is the signature of that whole class of mistake.

args <- commandArgs(trailingOnly = TRUE)
wf_dir <- if (length(args)) args[1] else ".github/workflows"

# Symbols supplied by the runtime rather than the block itself.
ALLOW <- c("T", "F", "args", ".Machine", "pi", "LETTERS", "letters",
           "month.abb", "month.name")

# R's hard -e limit, the size this linter refuses, and the size it warns at.
# The gap is deliberate headroom: a block that is already at 9500 bytes will be
# pushed over by the next small edit, and the failure is silent.
E_LIMIT_HARD <- 10000L
E_LIMIT_FAIL <-  8000L
E_LIMIT_WARN <-  6000L

extract_blocks <- function(path) {
  lines <- readLines(path, warn = FALSE)
  blocks <- list(); i <- 1
  while (i <= length(lines)) {
    if (grepl("Rscript -e '", lines[i], fixed = TRUE)) {
      # A one-liner closes its own quote on the same line. Scanning ahead for a
      # lone quote regardless swallowed every line up to the NEXT block's
      # terminator: the diagnostic came out as a syntax error quoting raw YAML,
      # and the block that got swallowed was never checked at all — so a real
      # dangling reference sitting in it went unreported.
      after <- sub("^.*Rscript -e '", "", lines[i])
      if (grepl("'", after, fixed = TRUE)) {
        blocks[[length(blocks) + 1]] <- list(line = i, code = sub("'.*$", "", after))
        i <- i + 1
        next
      }
      start <- i
      # Blocks close on a line whose only content is a single quote.
      j <- i + 1
      while (j <= length(lines) && !grepl("^\\s*'\\s*$", lines[j])) j <- j + 1
      if (j > length(lines)) {
        # Unterminated: report it rather than slicing an arbitrary range, which
        # for a block near the end of the file reverses and lints the wrong
        # lines.
        blocks[[length(blocks) + 1]] <- list(line = start, code = NULL,
                                             unterminated = TRUE)
        break
      }
      body <- if (j > start + 1) lines[(start + 1):(j - 1)] else character(0)
      keep <- nzchar(trimws(body))
      if (any(keep)) {
        indent <- min(nchar(sub("[^ ].*$", "", body[keep])))
        body <- substring(body, indent + 1)
      }
      blocks[[length(blocks) + 1]] <- list(line = start, code = body)
      i <- j + 1
    } else i <- i + 1
  }
  blocks
}

problems <- 0L
files <- list.files(wf_dir, pattern = "\\.ya?ml$", full.names = TRUE)
if (!length(files)) { cat("No workflow files found in", wf_dir, "\n"); quit(status = 0) }

for (f in files) {
  for (b in extract_blocks(f)) {
    label <- sprintf("%s:%d", f, b$line)

    # A lone apostrophe ends the block, because the R is embedded in the shell
    # as Rscript -e '...'. bash closes the quote at the first ' it meets and
    # then tries to parse the rest of the R as shell, which fails somewhere
    # further on with a message that names an innocent line.
    #
    # This is not hypothetical. The word "day's" in a COMMENT took the nightly
    # down, and the failure read:
    #     syntax error near unexpected token `"data/price_history.csv"'
    # which points at code that is perfectly fine. Parsing the YAML, as this
    # linter did, never sees it: the R is valid R. Only the shell objects.
    if (!isTRUE(b$unterminated) && length(b$code)) {
      q <- grep("'", b$code, fixed = TRUE)
      if (length(q)) {
        cat(sprintf("FAIL %s\n  apostrophe inside Rscript -e '...' closes the shell quote:\n", label))
        for (i in utils::head(q, 5))
          cat(sprintf("    line %d: %s\n", b$line + i, trimws(b$code[i])))
        cat("  Reword rather than escape \u2014 an escaped quote survives review badly.\n")
        problems <- problems + 1L
        next
      }
    }

    if (isTRUE(b$unterminated)) {
      cat(sprintf("FAIL %s\n  unterminated Rscript -e block (no closing quote)\n", label))
      problems <- problems + 1L
      next
    }

    # R caps a -e expression at 10000 bytes. Past that it prints
    #     WARNING: '-e <mangled expression>'
    # throws the expression away, and EXITS 0. The step runs nothing and goes
    # green. Parsing cannot see this, because the R is perfectly valid R - only
    # R's own argument handling refuses it, and only at invocation time.
    #
    # This is not hypothetical. The nightly's "Verify pipeline output" step
    # crossed the limit at commit 856380c on 2026-10-31 and silently did
    # nothing for three nights: no provenance check, no feed-health check, no
    # dark_feeds output, so the dark-feed gate was always skipped. The step
    # reported success every time.
    #
    # A block this long belongs in a file called as Rscript path/to/it.R, which
    # has no limit, fails loudly on a syntax error, and can be unit tested.
    if (!isTRUE(b$unterminated) && length(b$code)) {
      nbytes <- sum(nchar(b$code, type = "bytes")) + max(0L, length(b$code) - 1L)
      if (nbytes > E_LIMIT_FAIL) {
        cat(sprintf("FAIL %s\n  Rscript -e block is %d bytes, over R's %d-byte -e limit.\n",
                    label, nbytes, E_LIMIT_HARD))
        cat("  R would discard this expression, print a WARNING, and exit 0,\n")
        cat("  so the step would run nothing and still report success.\n")
        cat("  Move it to a script file and call Rscript <file>.\n")
        problems <- problems + 1L
        next
      }
      if (nbytes > E_LIMIT_WARN)
        cat(sprintf("warn %s (%d bytes, within %d of R's %d-byte -e limit)\n",
                    label, nbytes, E_LIMIT_HARD - nbytes, E_LIMIT_HARD))
    }
    exprs <- tryCatch(parse(text = b$code), error = function(e) e)
    if (inherits(exprs, "error")) {
      cat(sprintf("FAIL %s\n  syntax: %s\n", label, conditionMessage(exprs)))
      problems <- problems + 1L
      next
    }
    # codetools ships with R, but degrade to a parse-only check rather than
    # failing the nightly if it is ever absent. A guard that breaks the build
    # it is meant to protect is worse than no guard.
    if (!requireNamespace("codetools", quietly = TRUE)) {
      cat(sprintf("ok   %s (%d lines, parse-only: codetools unavailable)\n",
                  label, length(b$code)))
      next
    }
    fn <- as.function(c(alist(), as.call(c(as.name("{"), as.list(exprs)))))
    globals <- tryCatch(codetools::findGlobals(fn, merge = FALSE)$variables,
                        error = function(e) character(0))
    # Anything resolvable from an attached package is fine; only unresolvable
    # bare names indicate a genuine dangling reference.
    unresolved <- Filter(function(g) !exists(g, envir = globalenv()) &&
                                     !g %in% ALLOW, globals)
    if (length(unresolved)) {
      cat(sprintf("FAIL %s\n  undefined variable(s): %s\n",
                  label, paste(unresolved, collapse = ", ")))
      problems <- problems + 1L
    } else {
      cat(sprintf("ok   %s (%d lines)\n", label, length(b$code)))
    }
  }
}

# Script files the workflow calls directly. Inlining was the only option this
# linter originally had to check, so moving a program into a file would have
# quietly removed it from scope - the opposite of the intent.
for (f in files) {
  lines <- readLines(f, warn = FALSE)
  hits <- regmatches(lines, regexpr("Rscript +[^ '\"]+\\.R\\b", lines))
  # The path in the workflow is repo-relative, because that is the working
  # directory the runner uses. This linter is run from elsewhere too - the test
  # suite calls it with an absolute workflows directory from tests/testthat -
  # so resolve against the repo root implied by the workflow file itself rather
  # than against getwd(), which reported every script as missing.
  root <- dirname(dirname(dirname(f)))
  for (h in unique(unlist(hits))) {
    target <- sub("^Rscript +", "", h)
    label <- sprintf("%s -> %s", f, target)
    resolved <- if (file.exists(target)) target else file.path(root, target)
    if (!file.exists(resolved)) {
      cat(sprintf("FAIL %s\n  workflow calls a script that does not exist\n", label))
      problems <- problems + 1L
      next
    }
    err <- tryCatch({ parse(resolved); NULL }, error = function(e) conditionMessage(e))
    if (!is.null(err)) {
      cat(sprintf("FAIL %s\n  syntax: %s\n", label, err))
      problems <- problems + 1L
    } else {
      cat(sprintf("ok   %s (%d lines, script file)\n",
                  label, length(readLines(resolved, warn = FALSE))))
    }
  }
}

if (problems) {
  cat(sprintf("\n%d workflow R block(s) failed the check.\n", problems))
  quit(status = 1)
}
cat("\nAll workflow R blocks passed.\n")
