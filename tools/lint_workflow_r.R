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
    if (isTRUE(b$unterminated)) {
      cat(sprintf("FAIL %s\n  unterminated Rscript -e block (no closing quote)\n", label))
      problems <- problems + 1L
      next
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

if (problems) {
  cat(sprintf("\n%d workflow R block(s) failed the check.\n", problems))
  quit(status = 1)
}
cat("\nAll workflow R blocks passed.\n")
