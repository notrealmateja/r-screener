# Does the failure notification say anything true?
#
# Run #255 emailed "Daily pipeline failed (2026-11-04) - unknown step" about a
# run in which every step succeeded except a gate that is MEANT to fail, and
# whose body said "data files are preserved ... and the deploy is skipped" about
# a run that committed data and deployed. Both halves were wrong, and the
# subject line is the whole notification for anyone reading on a phone.
#
# Cause of "unknown step": the reporter asked the Jobs API which step had
# failed, from inside the job that was still running. GitHub does not report a
# conclusion for a step of an in-progress job, so the loop matched nothing,
# raised nothing, and the `step = "unknown step"` default went out. A silent
# default again -- the same shape as dark_feeds-unset and the verify step that
# ran nothing, and the third time in this project a fallback shipped as the
# normal path.
#
# A step outcome, unlike the API, is available the instant the step ends, so
# attribution now happens in the job. That only works while every step that can
# fail has an `id:` and a branch in the chain, which is what the first test
# below enforces -- a new step added without one fails the suite rather than
# quietly degrading the next alert.
#
# Parsed as TEXT, not with the yaml package: yaml is not in the nightly's
# install list, so a test that needs it would skip in CI, which is where it
# matters. Same reason as test-pipeline-provenance.R.

wf_file <- repo_path(".github", "workflows", "daily-update.yml")

wf <- local({
  if (!file.exists(wf_file)) NULL else readLines(wf_file, warn = FALSE)
})

# Split the job into steps: a step starts at "      - name:" and runs to the
# next one. Keeps the raw block so per-step keys can be inspected.
wf_steps <- function(lines) {
  starts <- grep("^      - name: ", lines)
  if (!length(starts)) return(list())
  ends <- c(starts[-1] - 1L, length(lines))
  lapply(seq_along(starts), function(i) {
    block <- lines[starts[i]:ends[i]]
    id <- grep("^        id: ", block, value = TRUE)
    list(
      name  = sub("^      - name: ", "", lines[starts[i]]),
      id    = if (length(id)) trimws(sub("^        id:", "", id[1])) else NA_character_,
      block = block,
      nonfatal = any(grepl("^        continue-on-error:\\s*true", block))
    )
  })
}

# The reporter's own two steps do not report on themselves.
REPORTING <- c("Report failure", "Resolve failure issue")

test_that("every step that can fail the job can be named in the alert", {
  skip_if(is.null(wf), "workflow not present")
  steps <- wf_steps(wf)
  expect_gt(length(steps), 10L)

  chain <- paste(wf, collapse = "\n")
  chain <- sub(".*FAILED_STEP: >-", "", chain)
  chain <- sub("\n          DARK_FEEDS:.*", "", chain)
  expect_true(nzchar(trimws(chain)), info = "FAILED_STEP chain not found")

  fatal <- Filter(function(s) !s$nonfatal && !(s$name %in% REPORTING), steps)
  expect_gt(length(fatal), 8L)

  for (s in fatal) {
    # An id, so the job can refer to its outcome at all...
    expect_false(is.na(s$id),
                 info = sprintf("step '%s' can fail the job but has no id:, so the alert cannot name it", s$name))
    if (is.na(s$id)) next
    # ...a branch in the chain, so it is actually consulted...
    expect_match(chain, sprintf("steps\\.%s\\.outcome", s$id), fixed = FALSE,
                 info = sprintf("step '%s' (id %s) is missing from the FAILED_STEP chain", s$name, s$id))
    # ...and the label must be the step's real name, or the email names a step
    # that does not exist in the log the reader then goes looking at.
    expect_true(grepl(s$name, chain, fixed = TRUE),
                info = sprintf("chain does not carry the exact name '%s'", s$name))
  }
})

test_that("the non-fatal deploy is deliberately absent from attribution", {
  skip_if(is.null(wf), "workflow not present")
  steps <- wf_steps(wf)
  nonfatal <- Filter(function(s) s$nonfatal, steps)
  # Asserted, not skipped: this workflow has a continue-on-error deploy, and if
  # it ever loses that flag the step becomes fatal and belongs in the chain
  # instead. An earlier version of this test guarded the assertion behind the
  # step having an id, so when it had none the test made no assertions at all
  # and testthat reported it as a SKIP -- a green tick for a check that never
  # ran, which is the failure mode this whole file exists to stop.
  expect_gte(length(nonfatal), 1L)

  chain <- sub(".*FAILED_STEP: >-", "", paste(wf, collapse = "\n"))
  chain <- sub("\n          DARK_FEEDS:.*", "", chain)
  for (s in nonfatal) {
    # continue-on-error means its failure is never why the job is red. Naming
    # it would mask the real cause, since it sits before the dark-feed gate.
    expect_false(grepl(s$name, chain, fixed = TRUE),
                 info = sprintf("non-fatal step '%s' must not be named as the failure", s$name))
    if (!is.na(s$id))
      expect_false(grepl(sprintf("steps.%s.outcome", s$id), chain, fixed = TRUE),
                   info = sprintf("non-fatal step '%s' is consulted for attribution", s$name))
  }
})

test_that("the gate that fails on purpose has an id", {
  skip_if(is.null(wf), "workflow not present")
  steps <- wf_steps(wf)
  gate <- Filter(function(s) s$name == "Fail on a dark upstream feed", steps)
  expect_length(gate, 1L)
  expect_equal(gate[[1]]$id, "darkfeed")
})

test_that("attribution no longer asks the API about its own running job", {
  skip_if(is.null(wf), "workflow not present")
  joined <- paste(wf, collapse = "\n")

  # The exact lookup that produced "unknown step". Removed rather than kept as
  # a fallback: a dead path that looks like it works is how this survived.
  expect_false(grepl('/actions/runs/" + os.environ["RUN_ID"] + "/jobs', joined, fixed = TRUE),
               info = "the in-flight Jobs API step lookup is back")
  expect_false(grepl('step = "unknown step"', joined, fixed = TRUE),
               info = "the silent default is back")

  # Attribution comes from the job itself.
  expect_true(grepl('os.environ.get("FAILED_STEP"', joined, fixed = TRUE))
  # And an unnamed step is stated as a reporting defect, not passed off as the
  # failure's own description.
  expect_true(grepl("attributed", joined, fixed = TRUE))
  expect_match(joined, "missing an `id:`")
})

# Pull the reporter's python back out of the YAML and run it with the network
# stubbed, so what the owner would actually receive can be asserted rather than
# inferred from the presence of strings in the file.
#
# This exists because the text assertions below are not enough on their own:
# changing `if stall:` to `if False:` leaves every string in place, so the
# whole two-mode distinction can be disabled with every grep still passing.
# Mutation testing caught that, which is the same lesson as the verify step
# that contained the right code and never ran.
reporter_python <- function(lines) {
  i <- grep("cat > /tmp/report_failure.py <<'PYSCRIPT'", lines, fixed = TRUE)
  if (length(i) != 1) return(NULL)
  j <- grep("^          PYSCRIPT\\s*$", lines)
  j <- j[j > i][1]
  if (is.na(j)) return(NULL)
  substring(lines[(i + 1L):(j - 1L)], 11L)
}

test_that("the two failure modes actually produce different notifications", {
  skip_if(is.null(wf), "workflow not present")
  skip_if_not(nzchar(Sys.which("python3")), "python3 not available")
  skip_if_not(requireNamespace("jsonlite", quietly = TRUE), "jsonlite not available")
  py <- reporter_python(wf)
  expect_false(is.null(py))
  skip_if(is.null(py), "could not extract the reporter")

  d <- file.path(tempdir(), paste0("rep-", basename(tempfile(""))))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  owd <- setwd(d); on.exit(setwd(owd), add = TRUE)
  writeLines(py, "reporter.py")
  writeLines(c(
    "import io, json, os, re",
    "src = io.open('reporter.py').read()",
    "src = src.replace('import json, os, urllib.request, datetime', 'import json, os, datetime')",
    "src = re.sub(r'def call\\(url, payload=None\\):.*?return json\\.loads\\(body\\) if body\\.strip\\(\\) else None',",
    "             'def call(url, payload=None):\\n    sent.append((url, payload))\\n    return None',",
    "             src, flags=re.S)",
    "sent = []",
    "g = {'sent': sent, '__name__': '__main__'}",
    "exec(compile(src, 'reporter.py', 'exec'), g)",
    "out = [p for (u, p) in sent if p and 'title' in p and 'body' in p]",
    "print('RESULT' + json.dumps(out[0] if out else {}))"
  ), "driver.py")

  # Set the variables in this process and let python inherit them. system2's
  # env= argument pastes "VAR=value" into a shell command line WITHOUT quoting,
  # so a step name containing spaces -- which every one of them does -- turns
  # into stray words and the command cannot run at all.
  fire <- function(step, dark) {
    old <- Sys.getenv(c("FAILED_STEP", "DARK_FEEDS", "GH_TOKEN", "REPO",
                        "RUN_ID", "RUN_URL"), names = TRUE, unset = NA)
    Sys.setenv(FAILED_STEP = step, DARK_FEEDS = dark, GH_TOKEN = "x",
               REPO = "o/r", RUN_ID = "1", RUN_URL = "https://example.invalid/1")
    on.exit({
      set <- old[!is.na(old)]
      if (length(set)) do.call(Sys.setenv, as.list(set))
      Sys.unsetenv(names(old)[is.na(old)])
    }, add = TRUE)
    out <- suppressWarnings(system2("python3", c("-I", "driver.py"),
                                    stdout = TRUE, stderr = FALSE,
                                    stdin = "/dev/null"))
    js <- sub("^RESULT", "", grep("^RESULT", out, value = TRUE))
    if (!length(js)) return(NULL)
    jsonlite::fromJSON(js[length(js)])
  }

  stall <- fire("Fail on a dark upstream feed", "Alpha Vantage fundamentals silent 29 days")
  brk   <- fire("Run data pipeline", "")
  expect_false(is.null(stall), info = "stall scenario produced no issue")
  expect_false(is.null(brk),   info = "failure scenario produced no issue")
  skip_if(is.null(stall) || is.null(brk), "reporter did not run")

  # The subject line is the whole notification on a phone, so the two events
  # must not share one.
  expect_false(identical(stall$title, brk$title))
  expect_match(stall$title, "^Upstream feed stalled")
  expect_match(brk$title,   "^Daily pipeline failed")
  # The stalled vendor must be named in the subject, not just the body.
  expect_match(stall$title, "Alpha Vantage", fixed = TRUE)

  # A stall says the pipeline worked; a breakage must not.
  expect_match(stall$body, "ran and published normally")
  expect_false(grepl("ran and published normally", brk$body, fixed = TRUE))

  # And the "we kept the last good data" wording belongs only to a real break:
  # in a stall the data WAS committed and the deploy DID run, which is the
  # sentence that made run #255 read as a breakage.
  expect_false(grepl("the deploy is skipped", stall$body, fixed = TRUE))
  expect_match(brk$body, "the deploy is skipped")

  # Neither mode may tell the owner to rotate a key that works.
  expect_match(stall$body, "the key is valid")
})

test_that("an unnamed step is reported as a reporting defect, in the email", {
  skip_if(is.null(wf), "workflow not present")
  skip_if_not(nzchar(Sys.which("python3")), "python3 not available")
  skip_if_not(requireNamespace("jsonlite", quietly = TRUE), "jsonlite not available")
  py <- reporter_python(wf)
  skip_if(is.null(py), "could not extract the reporter")

  d <- file.path(tempdir(), paste0("rep2-", basename(tempfile(""))))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  owd <- setwd(d); on.exit(setwd(owd), add = TRUE)
  writeLines(py, "reporter.py")
  writeLines(c(
    "import io, json, os, re",
    "src = io.open('reporter.py').read()",
    "src = src.replace('import json, os, urllib.request, datetime', 'import json, os, datetime')",
    "src = re.sub(r'def call\\(url, payload=None\\):.*?return json\\.loads\\(body\\) if body\\.strip\\(\\) else None',",
    "             'def call(url, payload=None):\\n    sent.append((url, payload))\\n    return None',",
    "             src, flags=re.S)",
    "sent = []",
    "g = {'sent': sent, '__name__': '__main__'}",
    "exec(compile(src, 'reporter.py', 'exec'), g)",
    "out = [p for (u, p) in sent if p and 'title' in p and 'body' in p]",
    "print('RESULT' + json.dumps(out[0] if out else {}))"
  ), "driver.py")

  # FAILED_STEP deliberately empty: this is the unattributable case.
  Sys.setenv(FAILED_STEP = "", DARK_FEEDS = "", GH_TOKEN = "x", REPO = "o/r",
             RUN_ID = "1", RUN_URL = "https://example.invalid/1")
  on.exit(Sys.unsetenv(c("FAILED_STEP", "DARK_FEEDS", "GH_TOKEN", "REPO",
                         "RUN_ID", "RUN_URL")), add = TRUE)
  out <- suppressWarnings(system2("python3", c("-I", "driver.py"),
    stdout = TRUE, stderr = FALSE, stdin = "/dev/null"))
  js <- sub("^RESULT", "", grep("^RESULT", out, value = TRUE))
  skip_if(!length(js), "reporter did not run")
  got <- jsonlite::fromJSON(js[length(js)])

  # The old behaviour emailed "unknown step" as though that were a diagnosis.
  expect_false(grepl("unknown step", got$title, fixed = TRUE))
  # The caveat has to be in the BODY, which is what reaches the owner by email
  # — a run annotation only reaches someone who already opened the run page.
  expect_match(got$body, "could not name the failing step")
  expect_match(got$body, "defect in the reporting")
})

test_that("a deliberate upstream stall does not read as a broken pipeline", {
  skip_if(is.null(wf), "workflow not present")
  joined <- paste(wf, collapse = "\n")

  # The two modes must produce different subjects. "Daily pipeline failed" on a
  # run that published normally is what sent the owner looking for a breakage.
  expect_true(grepl("Upstream feed stalled", joined, fixed = TRUE))
  expect_true(grepl("Daily pipeline failed", joined, fixed = TRUE))
  expect_true(grepl("stall = attributed and step ==", joined, fixed = TRUE))

  # The stall body must say the pipeline ran, and must NOT claim the refresh
  # was skipped -- in a stall the data is committed and the deploy runs.
  expect_match(joined, "The pipeline ran and published normally")

  # The "deploy is skipped" sentence is true only of a real failure, so it must
  # live in that branch alone. Count it once, in the else branch.
  hits <- grep("the deploy is skipped", wf, fixed = TRUE)
  expect_length(hits, 1L)
  before <- paste(wf[max(1, hits - 12):hits], collapse = "\n")
  expect_true(grepl("else:", before, fixed = TRUE),
              info = "the skipped-deploy wording must sit in the real-failure branch only")

  # The remediation must not tell the owner to rotate a valid key.
  expect_match(joined, "premium-endpoint refusal means the key is valid")
})
