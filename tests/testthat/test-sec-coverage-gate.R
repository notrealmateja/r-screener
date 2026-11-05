# The SEC fundamentals coverage gate, and the one feed it lets off the hook.
#
# On 2026-11-04 the Alpha Vantage fundamentals feed stopped being fatal. Its
# four ratios -- pe_ratio, pb_ratio, roe, profit_margin -- now come from SEC
# XBRL, so a silent AV feed no longer freezes the ranking, and failing the
# nightly over it demanded a fix that does not exist: the endpoint is paid now
# and no key changes that.
#
# Softening an alarm is only safe if whatever replaced it is guarded at least
# as well. Nothing was guarding the replacement, so the alarm moved rather than
# disappearing, and these tests execute the new gate instead of grepping for
# it. A grep cannot tell a live branch from one guarded by FALSE.

vp_src <- repo_path("tools", "verify_pipeline.R")

test_that("sec_grade grades coverage at the boundaries it claims", {
  skip_if_not(file.exists(vp_src), "verify_pipeline.R not present")
  src <- readLines(vp_src, warn = FALSE)
  a <- grep("^sec_grade <- function", src)
  b <- grep("^sec_cov <- local\\(", src)
  expect_length(a, 1)
  expect_length(b, 1)
  expect_lt(a[1], b[1])
  e <- new.env(parent = globalenv())
  eval(parse(text = paste(src[a[1]:(b[1] - 1)], collapse = "\n")), envir = e)
  f <- e$sec_grade

  # The thresholds are arguments, so the production values get pinned too --
  # otherwise every case below could keep passing while the real gate moved.
  whole <- paste(src, collapse = "\n")
  expect_match(whole, "SEC_COV_MIN\\s*<-\\s*0\\.50")
  expect_match(whole, "SEC_COV_WARN\\s*<-\\s*0\\.85")
  expect_match(whole, "sec_grade\\(v, nrow\\(d\\), SEC_COV_MIN, SEC_COV_WARN\\)")
  MIN <- 0.50
  WARN <- 0.85

  # The live shape on 2026-11-04, read off the committed master_scored.csv:
  # 181 of 195 names carry at least one filed ratio, 126 carry all four.
  g <- f(c(rep(4, 126), rep(3, 30), rep(2, 22), rep(1, 3), rep(0, 14)),
         195, MIN, WARN)
  expect_equal(g$cov, 181)
  expect_equal(g$full, 126)
  expect_equal(g$n, 195)
  expect_equal(g$verdict, "ok")

  # A dead fetch: every ratio silently falls back to vendor data that is no
  # longer refreshed. This is the failure the gate exists for.
  expect_equal(f(rep(0, 195), 195, MIN, WARN)$verdict, "dead")
  expect_equal(f(rep(0, 195), 195, MIN, WARN)$cov, 0)

  # No column at all is a zero, not a pass: it means the step that writes it
  # never ran, which is worse than low coverage, not better.
  expect_equal(f(numeric(0), 195, MIN, WARN)$verdict, "dead")

  # Boundaries. Exactly at a threshold sits on the passing side of it.
  expect_equal(f(c(rep(1,  97), rep(0,  98)), 195, MIN, WARN)$verdict, "dead")
  expect_equal(f(c(rep(1,  98), rep(0,  97)), 195, MIN, WARN)$verdict, "degraded")
  expect_equal(f(c(rep(1, 165), rep(0,  30)), 195, MIN, WARN)$verdict, "degraded")
  expect_equal(f(c(rep(1, 166), rep(0,  29)), 195, MIN, WARN)$verdict, "ok")

  # Missing and unparseable values must not count as coverage, and must not
  # throw either -- this runs inside the nightly.
  expect_equal(f(rep(NA_real_, 195), 195, MIN, WARN)$verdict, "dead")
  expect_equal(f(rep("nope",   195), 195, MIN, WARN)$verdict, "dead")

  # A nonsense row count is reported as unknown, never as a silent pass.
  for (bad in list(0, -1, NA_integer_, c(1, 2), "195", NULL))
    expect_equal(f(c(1, 1), bad, MIN, WARN)$verdict, "unknown")
})

test_that("only the routed-around feed is exempt, and only conditionally", {
  skip_if_not(file.exists(vp_src), "verify_pipeline.R not present")
  src <- paste(readLines(vp_src, warn = FALSE), collapse = "\n")

  # Updated 2026-11-05. `routed` became a NAME rather than a bare TRUE when a
  # second feed was routed around on different grounds (see
  # test-news-staleness.R), so each exemption now says which replacement it
  # leans on instead of all of them sharing one implicit guard.
  expect_match(src, 'label = "Alpha Vantage fundamentals", warn = 14, dark = 21,\n\\s*routed = "sec"')
  # It is still the only feed routed to the SEC replacement. Granting a second
  # feed this guard would exempt it on evidence about a different data source.
  expect_equal(length(regmatches(src, gregexpr('routed = "sec"', src))[[1]]), 1)
  # And a bare TRUE is gone, so no feed can be exempt without naming its guard.
  expect_false(grepl("routed = TRUE", src, fixed = TRUE))

  # The exemption applies only while the named replacement is carrying the
  # fields, so a double failure is never quieter than a single one.
  expect_match(src, 'repl <- if \\(!is\\.null\\(fd\\$routed\\)\\) replacements\\[\\[fd\\$routed\\]\\] else NULL')
  expect_match(src, 'identical\\(st, "DARK"\\) && !is\\.null\\(repl\\) && isTRUE\\(repl\\$live\\)')
  expect_match(src, 'sec = list\\(\\n\\s*live = sec_live')
  expect_match(src, 'sec_live <- !is\\.null\\(sec_cov\\)')

  # A dark feed that is NOT routed around is still fatal. This branch was
  # dropped while making exactly this change, which emptied dark_feeds
  # entirely while the summary table went on printing DARK.
  expect_match(src, 'else if \\(identical\\(st, "DARK"\\)\\) \\{')
  expect_match(src, 'dark <- c\\(dark, sprintf\\("%s silent %d days"')

  # The replacement failing is itself fatal, on its own line.
  expect_match(src, 'dark <- c\\(dark, "SEC fundamentals coverage unknown"\\)')
  expect_match(src, 'dark <- c\\(dark, sprintf\\("SEC fundamentals coverage')
})

test_that("every feed the verifier prints as DARK is reported somewhere", {
  skip_if_not(file.exists(vp_src), "verify_pipeline.R not present")
  skip_if_not(nzchar(Sys.which("Rscript")), "Rscript not on PATH")
  skip_if_not(file.exists(repo_path("data", "master_scored.csv")), "no pipeline output")

  od <- tempfile()
  dir.create(od)
  sumf <- file.path(od, "summary.md")
  outf <- file.path(od, "out.txt")
  file.create(sumf, outf)
  wd <- getwd()
  setwd(repo_root)
  on.exit({ setwd(wd); unlink(od, recursive = TRUE) }, add = TRUE)

  # The verifier exits non-zero whenever it finds anything wrong, which is the
  # normal case on a developer checkout (there is no CI run stamp here), so the
  # exit status is not what is under test. What it REPORTED is.
  system2("Rscript", "tools/verify_pipeline.R", stdout = NULL, stderr = NULL,
          env = c(sprintf("GITHUB_STEP_SUMMARY=%s", sumf),
                  sprintf("GITHUB_OUTPUT=%s", outf)))
  sm <- readLines(sumf, warn = FALSE)
  skip_if(!length(sm), "verifier produced no step summary")

  i <- grep("^### Upstream feeds", sm)
  expect_length(i, 1)
  rows <- grep("^\\|", sm[i[1]:length(sm)], value = TRUE)
  expect_gt(length(rows), 1)

  # Whatever is dark must appear either in the fatal dark_feeds output or in
  # the reported issue list. Printing DARK in a table and then saying nothing
  # is the exact shape of a gate that looks alive and is not.
  reported <- paste(c(readLines(outf, warn = FALSE),
                      grep("issue\\(s\\)", sm, value = TRUE)), collapse = "\n")
  for (r in grep("\\| DARK \\|$", rows, value = TRUE)) {
    lab <- trimws(strsplit(r, "|", fixed = TRUE)[[1]][2])
    expect_true(grepl(lab, reported, fixed = TRUE),
                info = sprintf("printed DARK but reported nowhere: %s", lab))
  }

  # And the coverage of the replacement is always stated, pass or fail, so the
  # number is visible in the run rather than only when it breaks.
  expect_match(paste(sm, collapse = "\n"),
               "SEC fundamentals: [0-9]+ of [0-9]+ names carry a filed ratio")
})
