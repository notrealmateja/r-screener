# The nightly went red for three nights running in October 2026, and the reason
# it went UNNOTICED for one of them is more important than the reason it broke.
#
# It broke because `runs-on: ubuntu-latest` rolled from 24.04 to 26.04 while the
# binary package repo stayed pinned to the jammy (22.04) build. The xml2 binary
# installed, reported DONE, and then refused to load, because 26.04 ships
# libxml2 2.15 and that binary wanted the old soname.
#
# It went unnoticed because the install failure skipped the pipeline, and
# nothing downstream could tell "the pipeline never ran" apart from "the
# pipeline ran". The verify step inspected files a fresh checkout had just
# supplied, passed them, and the commit step published a commit titled
# "Auto-update: Daily alpha refresh 2026-10-29" whose entire content was a
# reshuffled manifest.json. Green data, a dated commit message, and five days of
# stale prices on the live site.
#
# These tests pin the guards that close both holes. They read the workflow as
# text on purpose: the `yaml` package is not in the nightly's install list, and a
# guard that cannot run in CI is not a guard.

wf_file <- repo_path(".github", "workflows", "daily-update.yml")

wf <- local({
  if (!file.exists(wf_file)) NULL else readLines(wf_file, warn = FALSE)
})

# The `if:` expression attached to a named step, or NA when it carries none.
step_if <- function(lines, step_name) {
  at <- grep(sprintf("^\\s*- name:\\s*\\Q%s\\E\\s*$", step_name), lines, perl = TRUE)
  if (!length(at)) return(NA_character_)
  # Scan forward to the next step boundary, taking the first `if:` inside.
  j <- at[1] + 1L
  while (j <= length(lines) && !grepl("^\\s*- name:", lines[j])) {
    m <- regmatches(lines[j], regexpr("^\\s*if:\\s*(.*)$", lines[j], perl = TRUE))
    if (length(m)) return(trimws(sub("^\\s*if:\\s*", "", m)))
    j <- j + 1L
  }
  NA_character_
}

# Comment lines, so a check for a banned construct is not tripped by prose that
# merely names it. test-data-integrity.R strips them for the same reason.
decomment <- function(lines) grep("^\\s*#", lines, invert = TRUE, value = TRUE)

# Body of a named step: every line up to the next step boundary.
step_body <- function(lines, step_name) {
  at <- grep(sprintf("^\\s*- name:\\s*\\Q%s\\E\\s*$", step_name), lines, perl = TRUE)
  if (!length(at)) return(character(0))
  j <- at[1] + 1L
  out <- character(0)
  while (j <= length(lines) && !grepl("^\\s*- name:", lines[j])) {
    out <- c(out, lines[j]); j <- j + 1L
  }
  out
}

# Ubuntu codename for each runner image. The pair below is the whole point of
# the first two tests, so it is written out rather than inferred.
CODENAME <- c("ubuntu-22.04" = "jammy",  "ubuntu-24.04" = "noble",
              "ubuntu-26.04" = "resolute")

test_that("the runner image is pinned, not floating", {
  skip_if(is.null(wf), "workflow file not present")
  ro <- grep("^\\s*runs-on:", wf, value = TRUE)
  expect_length(ro, 1L)
  img <- trimws(sub("^\\s*runs-on:\\s*", "", ro))
  # `ubuntu-latest` is a moving target that silently replaces the whole OS
  # underneath a pinned package repo. That is the outage, exactly.
  expect_false(grepl("latest", img, fixed = TRUE),
               info = paste("runs-on must name a version, got:", img))
  expect_true(img %in% names(CODENAME),
              info = paste("unrecognised runner image:", img,
                           "- add it to CODENAME with its Ubuntu codename"))
})

test_that("the binary package repo codename matches the pinned runner image", {
  skip_if(is.null(wf), "workflow file not present")
  img <- trimws(sub("^\\s*runs-on:\\s*", "",
                    grep("^\\s*runs-on:", wf, value = TRUE)[1]))
  skip_if_not(img %in% names(CODENAME), "runner image not in the codename map")

  rspm <- grep("packagemanager\\.posit\\.co/cran/__linux__/", wf, value = TRUE)
  expect_gt(length(rspm), 0L)
  used <- unique(sub(".*__linux__/([^/\"]+)/.*", "\\1", rspm))
  # A mismatch here hands the job binaries built for a different C library.
  # They install without complaint and fail at load time, which reads as a
  # missing package rather than the ABI break it actually is.
  expect_equal(used, unname(CODENAME[img]),
               info = paste0("runs-on is ", img, " (", CODENAME[img],
                             ") but the package repo asks for: ",
                             paste(used, collapse = ", ")))
})

test_that("the package guard reports why a package failed, not just its name", {
  skip_if(is.null(wf), "workflow file not present")
  body <- paste(decomment(step_body(wf, "Install R packages")), collapse = "\n")
  expect_true(nzchar(body))
  # requireNamespace(quietly = TRUE) returns FALSE for an INSTALLED package that
  # cannot be loaded and discards the condition. The whole diagnosis of this
  # outage was missing from the log for that one reason.
  expect_false(grepl("requireNamespace", body, fixed = TRUE),
               info = "the guard must load packages and keep the error, not requireNamespace() them")
  expect_true(grepl("loadNamespace", body, fixed = TRUE))
  expect_true(grepl("conditionMessage", body, fixed = TRUE))
})

test_that("the pipeline step is identifiable and stamps the run before running", {
  skip_if(is.null(wf), "workflow file not present")
  at <- grep("^\\s*- name:\\s*Run data pipeline\\s*$", wf)
  expect_length(at, 1L)
  body <- step_body(wf, "Run data pipeline")
  # Downstream steps gate on this id. Without it `steps.pipeline.outcome` is
  # the empty string, every comparison against 'skipped' passes, and the gates
  # below are silently inert.
  expect_true(any(grepl("^\\s*id:\\s*pipeline\\s*$", body)),
              info = "Run data pipeline needs `id: pipeline`")

  stamp <- grep("run_stamp", body)
  expect_gt(length(stamp), 0L)
  rs <- grep("Rscript", body)
  expect_gt(length(rs), 0L)
  # Order matters. Stamped BEFORE the pipeline, a run that dies half way still
  # counts as having run, which is what lets the commit step publish the good
  # data it did manage to write. Stamped after, a partial failure would be
  # indistinguishable from never having started.
  expect_true(min(stamp) < min(rs),
              info = "the run stamp must be written before the pipeline, not after")
})

test_that("verify ties its verdict to the run that is executing", {
  skip_if(is.null(wf), "workflow file not present")
  body <- paste(step_body(wf, "Verify pipeline output"), collapse = "\n")
  expect_true(nzchar(body))
  # Without this, every assertion in the step is satisfied by whatever the
  # checkout put on disk, so a skipped pipeline verifies clean.
  expect_true(grepl("GITHUB_RUN_ID", body, fixed = TRUE),
              info = "verify must compare the run stamp against GITHUB_RUN_ID")
  expect_true(grepl("run_stamp", body, fixed = TRUE))
  # And it must be fatal, not another line in the summary table.
  expect_true(grepl("hard <- c\\(hard,[^)]*run stamp|hard <- c\\(hard,[^)]*never ran",
                    body, perl = TRUE),
              info = "a missing or stale run stamp must land in `hard`, which stops the step")
})

test_that("nothing is committed when the pipeline never ran", {
  skip_if(is.null(wf), "workflow file not present")
  for (nm in c("Clean stale files and commit updated data",
               "Refresh Connect Cloud manifest")) {
    cond <- step_if(wf, nm)
    expect_false(is.na(cond), info = paste(nm, "has no `if:`"))
    # `always()` on its own is what published the fake refresh. The pipeline
    # outcome is the only thing that separates "ran and partly failed" — which
    # SHOULD still publish — from "never started", which must not.
    #
    # An allowlist is required, not `!= 'skipped'`. A CANCELLED run satisfies
    # `!= 'skipped'`, and cancellation publishes mixed vintage: core files are
    # written at different stages, so one of them is fresh, provenance passes,
    # and today's prices ship beside yesterday's scores.
    expect_match(cond, "steps\\.pipeline\\.outcome\\s*==\\s*'success'",
                 info = paste0(nm, ": if: ", cond))
    expect_match(cond, "steps\\.pipeline\\.outcome\\s*==\\s*'failure'",
                 info = paste0(nm, ": if: ", cond))
    expect_false(grepl("!=\\s*'skipped'", cond, perl = TRUE),
                 info = paste0(nm, ": a bare != 'skipped' lets a cancelled run commit: ", cond))
  }
})

test_that("a failed push cannot leave the run green", {
  skip_if(is.null(wf), "workflow file not present")
  body <- step_body(wf, "Clean stale files and commit updated data")
  push <- grep("git push", body, value = TRUE)
  expect_gt(length(push), 0L)
  # `git push || true` is a green run in which main never received the data and
  # the live site goes on serving yesterday with nothing anywhere saying so.
  expect_false(any(grepl("git push\\s*\\|\\|", push, perl = TRUE)),
               info = paste("push failure is swallowed:", paste(push, collapse = "; ")))
})

test_that("the commit subject records the data date, not just the clock", {
  skip_if(is.null(wf), "workflow file not present")
  body <- paste(step_body(wf, "Clean stale files and commit updated data"),
                collapse = "\n")
  # The subject line is the only record most people read. A message built from
  # date(1) alone announced a refresh on a night that regenerated nothing.
  expect_true(grepl("price_history", body, fixed = TRUE),
              info = "the commit subject should carry the max date from the data")
})

# The tests above assert the guard is PRESENT. These execute it.
#
# A provenance check that is too eager is worse than none: it would fail every
# green night and train everyone to ignore the alert. So scenario D below — an
# ordinary successful run — matters as much as the three that must be fatal, and
# E pins the deliberate middle case where a pipeline half-failed and should
# still publish what it wrote.

prov_src <- local({
  if (is.null(wf)) return(NULL)
  start <- grep("PROVENANCE. Every check above", wf, fixed = TRUE)
  if (!length(start)) return(NULL)
  rest <- wf[(start[1] + 1L):length(wf)]
  stop_at <- grep('^\\s*out <- c\\(out, ""\\)\\s*$', rest)
  if (!length(stop_at)) return(NULL)
  body <- wf[start[1]:(start[1] + stop_at[1] - 1L)]
  # Undo the YAML block-scalar indent so the fragment parses on its own.
  sub("^ {12}", "", body)
})

CORE_FILES <- c("data/master_scored.csv", "data/top15_sweetspot.csv",
                "data/top15_unicorns.csv", "data/price_history.csv",
                "data/macro_data.csv")

# Run the extracted fragment against a scratch tree and return what it flagged.
prov_run <- function(setup, run_id = "999") {
  d <- file.path(tempdir(), paste0("prov-", basename(tempfile(""))))
  dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)
  old <- setwd(d)
  on.exit({ setwd(old); unlink(d, recursive = TRUE) }, add = TRUE)

  # What a fresh checkout leaves on disk: every core file present and plausible.
  for (f in CORE_FILES) writeLines(c("date,x", "2026-10-26,1"), f)
  prev <- Sys.getenv("GITHUB_RUN_ID", unset = NA)
  Sys.setenv(GITHUB_RUN_ID = run_id)
  on.exit(if (is.na(prev)) Sys.unsetenv("GITHUB_RUN_ID")
          else Sys.setenv(GITHUB_RUN_ID = prev), add = TRUE)
  setup()

  e <- new.env()
  assign("CORE", CORE_FILES, e)
  assign("hard", character(0), e)
  assign("problems", character(0), e)
  assign("out", character(0), e)
  eval(parse(text = paste(prov_src, collapse = "\n")), envir = e)
  list(hard = get("hard", e), problems = get("problems", e))
}

# mtime granularity on some filesystems is whole seconds, so a rewrite has to
# land strictly after the stamp for the comparison to mean anything.
touch_after_stamp <- function(files, run_id = "999") {
  writeLines(run_id, "data/.run_stamp")
  Sys.sleep(1.1)
  for (f in files) writeLines(c("date,x", "2026-10-31,1"), f)
}

test_that("the provenance fragment could be extracted", {
  skip_if(is.null(wf), "workflow file not present")
  expect_false(is.null(prov_src))
  expect_gt(length(prov_src), 10L)
})

test_that("a skipped pipeline is fatal", {
  skip_if(is.null(prov_src), "provenance fragment not found")
  # The 2026-10-29 case exactly: nothing ran, so there is no stamp.
  r <- prov_run(function() {})
  expect_length(r$hard, 1L)
  expect_match(r$hard, "never ran")
})

test_that("a stamp from an earlier run is fatal", {
  skip_if(is.null(prov_src), "provenance fragment not found")
  r <- prov_run(function() writeLines("111", "data/.run_stamp"), run_id = "999")
  expect_length(r$hard, 1L)
  expect_match(r$hard, "not 999")
})

test_that("a pipeline that ran but rewrote nothing is fatal", {
  skip_if(is.null(prov_src), "provenance fragment not found")
  r <- prov_run(function() { writeLines("999", "data/.run_stamp"); Sys.sleep(1.1) })
  expect_length(r$hard, 1L)
  expect_match(r$hard, "rewrote no core data file")
})

test_that("an ordinary successful run is not flagged", {
  skip_if(is.null(prov_src), "provenance fragment not found")
  # The false-positive test. If this ever fails, the guard is blocking good
  # nights, which is a worse outage than the one it was built to catch.
  r <- prov_run(function() touch_after_stamp(CORE_FILES))
  expect_length(r$hard, 0L)
  expect_length(r$problems, 0L)
})

test_that("a partially failed pipeline still publishes, with a soft warning", {
  skip_if(is.null(prov_src), "provenance fragment not found")
  r <- prov_run(function() touch_after_stamp(CORE_FILES[1:3]))
  expect_length(r$hard, 0L)              # not fatal: publish the good data
  expect_length(r$problems, 1L)          # but say which files went cold
  expect_match(r$problems, "price_history\\.csv")
  expect_match(r$problems, "macro_data\\.csv")
})

# ── Upstream feed health ─────────────────────────────────────────────────────
#
# The checks above prove the pipeline RAN. These prove its INPUTS are still
# answering, which is a different failure and was invisible for 26 days: the
# Alpha Vantage key stopped working on 2026-10-06, every output stayed present
# and healthy because they are rebuilt nightly from the cache, and the run log
# went on reporting "194/195 stocks have full fundamentals" off frozen data.
#
# As with provenance, the false-positive case matters as much as the true one:
# this check makes the run go red, so a threshold that trips on a normal
# weekend would hand you a nightly alert that means nothing.

feed_src <- local({
  if (is.null(wf)) return(NULL)
  a <- grep("UPSTREAM FEED HEALTH", wf, fixed = TRUE)
  if (!length(a)) return(NULL)
  b <- grep("PROVENANCE. Every check above", wf, fixed = TRUE)
  if (!length(b) || b[1] <= a[1]) return(NULL)
  sub("^ {12}", "", wf[a[1]:(b[1] - 1L)])
})

# Run the extracted fragment over a scratch tree holding the two cache files,
# and return its table rows, soft problems, and whatever it declared dark.
feed_run <- function(av_date, news_date) {
  d <- file.path(tempdir(), paste0("feed-", basename(tempfile(""))))
  dir.create(file.path(d, "data"), recursive = TRUE, showWarnings = FALSE)
  old <- setwd(d)
  on.exit({ setwd(old); unlink(d, recursive = TRUE) }, add = TRUE)

  if (!is.na(av_date))
    writeLines(c("symbol,last_fetched", paste0("AAPL,", av_date)),
               "data/av_cache.csv")
  if (!is.na(news_date))
    writeLines(c("title,publishedDate", paste0("x,", news_date, "T01:02:03")),
               "data/market_news.csv")

  go <- file.path(d, "ghout.txt")
  file.create(go)
  prev <- Sys.getenv("GITHUB_OUTPUT", unset = NA)
  Sys.setenv(GITHUB_OUTPUT = go)
  on.exit(if (is.na(prev)) Sys.unsetenv("GITHUB_OUTPUT")
          else Sys.setenv(GITHUB_OUTPUT = prev), add = TRUE)

  e <- new.env()
  assign("out", character(0), e)
  assign("problems", character(0), e)
  assign("read_csv", readr::read_csv, e)
  eval(parse(text = paste(feed_src, collapse = "\n")), envir = e)
  list(out = get("out", e), problems = get("problems", e),
       dark = if (file.exists(go)) readLines(go, warn = FALSE) else character(0))
}

ago <- function(n) format(Sys.Date() - n)

test_that("the feed fragment could be extracted", {
  skip_if(is.null(wf), "workflow file not present")
  expect_false(is.null(feed_src))
  expect_gt(length(feed_src), 20L)
})

test_that("feeds answering today are not flagged", {
  skip_if(is.null(feed_src), "feed fragment not found")
  # The false-positive guard. If this fails, every night goes red for nothing.
  r <- feed_run(ago(0), ago(0))
  expect_length(r$problems, 0L)
  expect_length(r$dark, 0L)
  expect_true(any(grepl("| ok |", r$out, fixed = TRUE)))
})

test_that("a feed quiet for a few days is soft, not fatal", {
  skip_if(is.null(feed_src), "feed fragment not found")
  # Fundamentals rotate on a 10-day TTL, so 15 days is stale but not yet proof
  # of a dead credential. It belongs in the summary, not in a red run.
  r <- feed_run(ago(15), ago(0))
  expect_length(r$dark, 0L)
  expect_length(r$problems, 1L)
  expect_match(r$problems, "fundamentals last answered 15 days ago")
})

test_that("a feed silent past its threshold goes dark and reaches the step output", {
  skip_if(is.null(feed_src), "feed fragment not found")
  r <- feed_run(ago(26), ago(26))
  expect_length(r$dark, 1L)
  expect_match(r$dark, "^dark_feeds=")
  expect_match(r$dark, "Alpha Vantage fundamentals silent 26 days")
  expect_match(r$dark, "Alpha Vantage news silent 26 days")
  # Dark must not ALSO be reported as a soft problem; it has its own channel.
  expect_length(r$problems, 0L)
})

test_that("the two feeds carry independent thresholds", {
  skip_if(is.null(feed_src), "feed fragment not found")
  # News is refetched nightly, so 16 days dark; fundamentals rotate slowly and
  # get 21. A single shared threshold would either miss a dead news feed for
  # three weeks or cry wolf about healthy fundamentals.
  r <- feed_run(ago(16), ago(16))
  expect_length(r$dark, 1L)
  expect_match(r$dark, "news silent 16 days")
  expect_false(grepl("fundamentals silent", r$dark))
  expect_match(paste(r$problems, collapse = "|"), "fundamentals last answered 16")
})

test_that("a missing or unreadable cache is reported rather than skipped", {
  skip_if(is.null(feed_src), "feed fragment not found")
  r <- feed_run(NA, ago(0))
  expect_true(any(grepl("MISSING", r$out, fixed = TRUE)))
  # Absent is not the same as dark: there is nothing to date, so it cannot be
  # given an age, and silently dropping the row would hide it entirely.
  expect_length(r$dark, 0L)
})

test_that("the dark-feed step fails the run and names the remedy", {
  skip_if(is.null(wf), "workflow file not present")
  cond <- step_if(wf, "Fail on a dark upstream feed")
  expect_false(is.na(cond))
  expect_match(cond, "steps\\.verify\\.outputs\\.dark_feeds")
  body <- paste(step_body(wf, "Fail on a dark upstream feed"), collapse = "\n")
  expect_match(body, "exit 1")
  expect_match(body, "AV_KEY")
  # It has to sit AFTER the commit, or a dead third-party key would block
  # publication of the data every working source did refresh.
  at_commit <- grep("^\\s*- name:\\s*Clean stale files and commit updated data",
                    wf)
  at_dark <- grep("^\\s*- name:\\s*Fail on a dark upstream feed", wf)
  expect_true(at_dark[1] > at_commit[1])
})

# ── Cancellation ─────────────────────────────────────────────────────────────
#
# Cancellation is the likeliest way this job dies: four APIs, 195 tickers, and
# no timeout on any individual fetch. It was also the one outcome that reported
# nothing at all — failure() is FALSE for a cancelled run, so no issue was
# opened or commented, while Resolve was gated on success() so nothing was
# closed either. Silence in both directions, for the most probable failure.

test_that("the job is time-bounded well inside GitHub's default", {
  skip_if(is.null(wf), "workflow file not present")
  tm <- grep("^\\s*timeout-minutes:", wf, value = TRUE)
  expect_length(tm, 1L)
  mins <- as.integer(trimws(sub("^\\s*timeout-minutes:\\s*", "", tm)))
  expect_true(!is.na(mins))
  # Successful runs take ~25 minutes. The point is to fail fast rather than
  # hold a runner for GitHub's 360-minute default, so anything near that
  # defeats the purpose.
  expect_lt(mins, 180L)
  expect_gt(mins, 30L)
})

test_that("a cancelled run still reports", {
  skip_if(is.null(wf), "workflow file not present")
  cond <- step_if(wf, "Report failure")
  expect_false(is.na(cond))
  expect_match(cond, "cancelled\\(\\)",
               info = paste("Report failure must fire on cancellation; if:", cond))
})

test_that("the failure report can name a cancelled step", {
  skip_if(is.null(wf), "workflow file not present")
  code <- decomment(step_body(wf, "Report failure"))
  # Firing the step is not enough. The lookup matched only conclusion ==
  # "failure"; on a cancelled run no step has that, so the issue title said
  # "unknown step" — the same indistinguishable alert the reporter exists to
  # prevent.
  #
  # Assert against the LOOKUP, not the step body: a bare search for
  # "cancelled" is satisfied by the `if:` condition one line above and so
  # passes even with the lookup left failure-only. Mutation testing caught
  # exactly that.
  concl <- grep("conclusion", code, value = TRUE)
  expect_gt(length(concl), 0L)
  expect_true(any(grepl("cancelled", concl, fixed = TRUE)),
              info = paste("lookup must accept a cancelled conclusion:",
                           paste(concl, collapse = " | ")))
})

test_that("price history is tracked as a feed so a refused write surfaces", {
  skip_if(is.null(feed_src), "feed fragment not found")
  src <- paste(feed_src, collapse = "\n")
  expect_match(src, "data/price_history\\.csv")
  # write_history_or_keep protects the history by declining the write, which
  # leaves prices frozen until upstream recovers. Correct, but it must not go
  # unreported — that is precisely how Alpha Vantage stayed dark for 26 days.
  r <- feed_run(ago(0), ago(0))
  expect_length(r$dark, 0L)
})

# ── The manifest must match the data it ships with ───────────────────────────
#
# Connect Cloud validates every deployed file against a checksum in
# app/manifest.json. The refresh step swallowed its failure twice over -- a
# tryCatch that turned the error into a message and a FALSE nothing read, then
# continue-on-error on top -- so a failed refresh left the step green and the
# commit published freshly rewritten CSVs against the PREVIOUS manifest. The
# bundle then described data it did not contain.

test_that("the manifest step can be gated on, and does not swallow its failure", {
  skip_if(is.null(wf), "workflow file not present")
  nm <- "Refresh Connect Cloud manifest"
  at <- grep(sprintf("^\\s*- name:\\s*\\Q%s\\E\\s*$", nm), wf, perl = TRUE)
  expect_length(at, 1L)
  body <- step_body(wf, nm)

  # Without an id, steps.manifest.outcome is the empty string and the gate
  # below silently compares against nothing.
  expect_true(any(grepl("^\\s*id:\\s*manifest\\s*$", body)),
              info = "the manifest step needs `id: manifest`")
  # continue-on-error was the outer of the two swallows.
  expect_false(any(grepl("^\\s*continue-on-error:\\s*true\\s*$", body)),
               info = "continue-on-error hides the failure from the gate")

  code <- decomment(body)
  txt <- paste(code, collapse = "\n")
  expect_false(grepl("leaving manifest untouched", txt, fixed = TRUE),
               info = "a path that leaves the manifest stale must be fatal, not a message")

  # And the inner swallow. Asserting only that the body contains stop() is too
  # weak: mutation testing turned the writeManifest handler back into a
  # message() and this test still passed, because the two guard clauses above
  # it have stop() calls of their own. So require EVERY error handler in the
  # step to terminate.
  eh <- grep("error\\s*=\\s*function", code)
  expect_gt(length(eh), 0L)
  for (i in eh) {
    near <- paste(code[i:min(length(code), i + 3L)], collapse = " ")
    expect_match(near, "stop\\(",
                 info = paste("an error handler that does not stop() is a swallow:",
                              trimws(code[i])))
  }
})

test_that("nothing is committed against a stale manifest", {
  skip_if(is.null(wf), "workflow file not present")
  cond <- step_if(wf, "Clean stale files and commit updated data")
  expect_false(is.na(cond))
  # One day stale beats one day broken: yesterday's CSVs and yesterday's
  # manifest are at least a matching pair.
  expect_match(cond, "steps\\.manifest\\.outcome\\s*==\\s*'success'",
               info = paste("commit must require a refreshed manifest; if:", cond))
})

# ── Tests must fail, not vanish ──────────────────────────────────────────────
#
# A test_that registered inside `if (exists(...))` at file scope is not skipped
# when the condition is false -- it is ABSENT. testthat reports no skip and no
# failure, just a smaller suite, so a module that fails to load deletes its own
# tests and the run goes green. Measured on test-equity-curve.R: with
# R/05_backtest.R made to error, the old file dropped from 22 blocks to 15 and
# still reported zero failures.
#
# The guard belongs with the pipeline-trust tests because that is the same
# failure shape as the rest of this file: a check that cannot fail.

test_that("no test file gates whole blocks behind a bare exists() wrapper", {
  files <- list.files(repo_path("tests", "testthat"), pattern = "^test-.*\\.R$",
                      full.names = TRUE)
  expect_gt(length(files), 0L)
  offenders <- character(0)
  for (f in files) {
    src <- readLines(f, warn = FALSE)
    # Column 0 only: an `if (exists(...))` nested inside a test_that body is
    # ordinary control flow, not a registration gate.
    hit <- grep("^if\\s*\\(\\s*(!\\s*)?exists\\(", src)
    if (length(hit))
      offenders <- c(offenders, sprintf("%s:%s", basename(f),
                                        paste(hit, collapse = ",")))
  }
  expect_length(offenders, 0L)
  if (length(offenders))
    cat("Put the guard inside each test_that as skip_if_not(exists(...), ...)",
        "so an absent symbol is a countable skip:",
        paste(offenders, collapse = "; "), "\n")
})
