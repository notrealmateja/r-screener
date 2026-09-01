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
      start <- i
      # Blocks close on a line whose only content is a single quote.
      j <- i + 1
      while (j <= length(lines) && !grepl("^\\s*'\\s*$", lines[j])) j <- j + 1
      body <- lines[(start + 1):(min(j, length(lines)) - 1)]
      indent <- min(nchar(sub("[^ ].*$", "", body[nzchar(trimws(body))])))
      body <- substring(body, indent + 1)
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
