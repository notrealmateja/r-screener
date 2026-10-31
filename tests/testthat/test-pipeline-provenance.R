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
    expect_true(grepl("steps\\.pipeline\\.outcome\\s*!=\\s*'skipped'", cond, perl = TRUE),
                info = paste0(nm, ": if: ", cond))
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
