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

# The -e length limit does NOT apply to the bytes you wrote.
#
# $R_HOME/bin/R is a shell script, and before R sees the argument it rewrites
# every space as ~+~, every newline as ~n~ and every tab as ~t~ (lines 196-197
# of that script). Each of those characters therefore costs THREE bytes against
# the limit, and indented multi-line R runs about 1.5x its own size once
# encoded: the verify block was 14616 raw bytes but 22246 encoded.
#
# Measured on R 4.5.2: an argument of 9997 encoded bytes runs and 9998 is
# refused. A pad of pure spaces reaches that ceiling at 3328 raw bytes and a
# pad containing no whitespace at 9984 -- exactly 3x apart, which is how the
# encoding was confirmed rather than assumed.
#
# That distinction is the whole bug. At 6e17f66 (2026-10-27) the verify block
# was 9109 raw bytes -- under any 10000-byte RAW threshold, so a raw check
# passes it -- and 13871 encoded, so R threw it away. The nightly then ran
# nothing for seven published nights while reporting success every time.
#
# E_LIMIT_HARD is the ceiling measured here, not a promise about the runner's
# own R build, so E_LIMIT_FAIL keeps real headroom: a block sitting at 9500 is
# pushed over by the next small edit, and the failure is silent. The remedy is
# cheap -- move the program into a file.
#
# The limit is cumulative across all -e arguments in one invocation, not per
# argument, so a step passing two of them must fit both in the same budget.
E_LIMIT_HARD <- 9997L
E_LIMIT_FAIL <- 8000L
E_LIMIT_WARN <- 7000L

indent_of <- function(s) nchar(sub("[^ ].*$", "", s))

# YAML strips a block scalar by the indentation of its FIRST non-empty line.
# That is the only indentation bash never sees; whatever the R code carries on
# top of it is part of the argument and costs 3 bytes per space. Measuring a
# de-indented copy of the body therefore understates every block.
yaml_block_indent <- function(lines, start) {
  i <- start
  while (i > 1L &&
         !grepl("^[[:space:]]*run:[[:space:]]*[|>][+-]?[[:space:]]*$", lines[i])) {
    if (grepl("^[[:space:]]*-[[:space:]]+name:", lines[i]))
      return(indent_of(lines[start]))
    i <- i - 1L
  }
  if (i <= 1L) return(indent_of(lines[start]))
  k <- i + 1L
  while (k <= length(lines) && !nzchar(trimws(lines[k]))) k <- k + 1L
  if (k > length(lines)) indent_of(lines[start]) else indent_of(lines[k])
}

# What bin/R hands the length check.
e_encoded_len <- function(code) {
  joined <- paste(code, collapse = "\n")
  n <- nchar(joined, type = "bytes")
  fat <- lengths(regmatches(joined, gregexpr("[ \t\n]", joined)))
  n + 2L * fat
}

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
        one <- sub("'.*$", "", after)
        blocks[[length(blocks) + 1]] <- list(line = i, code = one, arg = one)
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
      # Two copies, deliberately: the de-indented one to parse, and the one
      # bash really passes to measure. The opening quote is followed by a
      # newline and the closing quote preceded by one, and both are inside the
      # argument, so they count too.
      blk <- yaml_block_indent(lines, start)
      arg <- paste0("\n", paste(substring(body, blk + 1L), collapse = "\n"), "\n")
      keep <- nzchar(trimws(body))
      if (any(keep)) {
        indent <- min(nchar(sub("[^ ].*$", "", body[keep])))
        body <- substring(body, indent + 1)
      }
      blocks[[length(blocks) + 1]] <- list(line = start, code = body, arg = arg)
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

    # Past the encoded limit above, R prints
    #     WARNING: '-e <mangled expression>'
    # to STDOUT, throws the expression away, and EXITS 0. On a runner stdin is
    # /dev/null, so R reads EOF, finds nothing to do, and the step goes green
    # having run none of it. Parsing cannot see this, because the R is perfectly
    # valid R - only R's own argument handling refuses it, at invocation time.
    #
    # This is not hypothetical. The nightly's "Verify pipeline output" step
    # crossed the limit at commit 6e17f66 on 2026-10-27 and did nothing for
    # seven published nights: no provenance check, no feed-health check, no
    # dark_feeds output, so the dark-feed gate was skipped every night. The
    # step reported success every time. 6e17f66's own stated purpose was to
    # fix an apostrophe that had been failing this step LOUDLY in bash, so
    # removing it turned a visible failure into a silent one.
    #
    # A block this long belongs in a file called as Rscript path/to/it.R, which
    # has no limit, fails loudly on a syntax error, and can be unit tested.
    if (!isTRUE(b$unterminated) && length(b$code)) {
      arg <- if (!is.null(b$arg)) b$arg else paste(b$code, collapse = "\n")
      raw <- sum(nchar(arg, type = "bytes"))
      enc <- e_encoded_len(arg)
      if (enc > E_LIMIT_FAIL) {
        cat(sprintf("FAIL %s\n  Rscript -e block is %d bytes, %d once bin/R encodes it.\n",
                    label, raw, enc))
        cat(sprintf("  The limit applies to the encoded size and is about %d here.\n",
                    E_LIMIT_HARD))
        cat("  R would discard this expression, print a WARNING to STDOUT, and\n")
        cat("  exit 0, so the step would run nothing and still report success.\n")
        cat("  Move it to a script file and call Rscript <file>.\n")
        problems <- problems + 1L
        next
      }
      if (enc > E_LIMIT_WARN)
        cat(sprintf("warn %s (%d bytes, %d encoded, within %d of the limit)\n",
                    label, raw, enc, E_LIMIT_HARD - enc))
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
