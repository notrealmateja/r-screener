# Does the price pull actually reach the session that just closed?
#
# It did not, and the comment above the cron asserted the opposite. The cron was
# moved to 21:30 UTC specifically "so each run captures the same day's close",
# but tq_get's `to` argument is EXCLUSIVE of the date given, and the call passed
# `to = Sys.Date()`. At 21:30 UTC on day D, Sys.Date() is D, so the window ended
# at D-1 and the just-closed session was never fetched. Verified against the
# vendor: from = 2026-10-26, to = 2026-10-30 returns 10-26 through 10-29.
#
# Nothing noticed, because the only freshness check is wall-clock. Published
# data one trading day behind is 1-2 calendar days old, and the price feed's
# thresholds in tools/verify_pipeline.R are warn = 7 days, dark = 12 -- sized so
# a holiday weekend cannot trip them. Run #255 on 2026-11-04 published data
# ending 2026-11-02, skipping Tuesday the 3rd entirely, and the verifier printed
# `| Price history | 2026-11-02 | 2 | ok |`.
#
# The second half of the fix matters as much as the first: reaching one day past
# today means the window can now include the session IN PROGRESS, and the vendor
# serves that as a partial bar from the opening bell. A half-session treated as
# a daily bar is a fabricated return that flows straight into daily_alpha and
# the ranking, which is the same class of bug as the reverse-split returns
# corporate_action() exists to neutralise.

decomment <- function(lines) grep("^\\s*#", lines, invert = TRUE, value = TRUE)

mom_src <- repo_path("R", "02_momentum.R")

mom <- local({
  if (!file.exists(mom_src)) NULL else {
    e <- new.env(parent = globalenv())
    ok <- tryCatch({
      suppressMessages(sys.source(mom_src, envir = e))
      TRUE
    }, error = function(x) FALSE)
    if (ok) e else NULL
  }
})

# ── The session cutoff ──────────────────────────────────────────────────────

test_that("last_finished_session holds back the session in progress", {
  skip_if(is.null(mom), "R/02_momentum.R did not load")
  f <- mom$last_finished_session
  expect_true(is.function(f))

  utc <- function(s) as.POSIXct(s, tz = "UTC")

  # A US session dated D closes at 20:00 UTC under EDT and 21:00 under EST, so
  # 21:15 UTC on D clears both. These two cases are the whole contract.
  expect_equal(f(utc("2026-11-03 21:14:59")), as.Date("2026-11-02"))
  expect_equal(f(utc("2026-11-03 21:15:00")), as.Date("2026-11-03"))

  # The on-time cron slot. This is the case the cron comment claims and the old
  # code could not deliver: 21:30 UTC must reach the SAME day.
  expect_equal(f(utc("2026-11-03 21:30:00")), as.Date("2026-11-03"))

  # Summer, when the close is an hour earlier in UTC. Still the same day.
  expect_equal(f(utc("2026-07-15 21:30:00")), as.Date("2026-07-15"))

  # Mid-session: 18:00 UTC is 13:00 ET, hours before the bell. The current day
  # must NOT be offered, or a partial bar enters the history.
  expect_equal(f(utc("2026-11-03 18:00:00")), as.Date("2026-11-02"))

  # The two real runs that drifted past midnight UTC. Both must still reach the
  # trading day that had just closed in New York.
  expect_equal(f(utc("2026-11-04 01:32:01")), as.Date("2026-11-03"))  # run #255
  expect_equal(f(utc("2026-11-04 02:59:04")), as.Date("2026-11-03"))  # run #254
})

test_that("the cutoff never offers a session that has not closed", {
  skip_if(is.null(mom), "R/02_momentum.R did not load")
  f <- mom$last_finished_session
  # Sweep every quarter hour of a day and assert the invariant directly: the
  # session returned is today only once 21:15 UTC has passed, never before.
  base <- as.POSIXct("2026-11-03 00:00:00", tz = "UTC")
  for (i in 0:95) {
    now <- base + i * 900
    got <- f(now)
    want <- if (format(now, "%H:%M", tz = "UTC") >= "21:15")
      as.Date("2026-11-03") else as.Date("2026-11-02")
    expect_equal(got, want, info = format(now, tz = "UTC", usetz = TRUE))
  }
})

# ── The gap check ───────────────────────────────────────────────────────────

test_that("sessions_behind reports a real gap and ignores a market closure", {
  skip_if(is.null(mom), "R/02_momentum.R did not load")
  g <- mom$sessions_behind
  expect_true(is.function(g))

  d <- function(...) as.Date(c(...))
  cal <- d("2026-10-29", "2026-10-30", "2026-11-02", "2026-11-03")

  # Exactly the run #255 situation: the benchmark reached Tuesday, the universe
  # stopped at Monday. That is a gap and must be reported.
  expect_equal(g(as.Date("2026-11-02"), cal), d("2026-11-03"))

  # Caught up: nothing behind.
  expect_equal(g(as.Date("2026-11-03"), cal), as.Date(character(0)))

  # A market closure is NOT a gap. If the benchmark also stops at Monday --
  # a holiday, or the vendor simply has not posted Tuesday for anyone -- then
  # ending at Monday is the correct answer and must stay silent.
  expect_equal(g(as.Date("2026-11-02"), d("2026-10-30", "2026-11-02")),
               as.Date(character(0)))

  # A weekend is the same case, and this is why no holiday table is needed:
  # the benchmark's own dates skip Saturday and Sunday for us.
  expect_equal(g(as.Date("2026-10-30"), d("2026-10-29", "2026-10-30")),
               as.Date(character(0)))

  # Several sessions behind, ordered.
  expect_equal(g(as.Date("2026-10-29"), cal), d("2026-10-30", "2026-11-02", "2026-11-03"))

  # Degenerate inputs must not error. An empty universe is maximally behind,
  # not silently fine -- absence must never read as all-clear.
  expect_equal(g(as.Date(NA), cal), cal)
  expect_equal(g(as.Date(character(0)), cal), cal)
  expect_equal(g(as.Date("2026-11-03"), as.Date(character(0))), as.Date(character(0)))

  # Unsorted and duplicated benchmark dates are handled, since these come
  # straight out of a fetched frame.
  expect_equal(g(as.Date("2026-11-02"), d("2026-11-03", "2026-10-30", "2026-11-03")),
               d("2026-11-03"))
})

# ── Regression pins on the shipped source ───────────────────────────────────

test_that("the fetch window no longer stops a day short", {
  skip_if_not(file.exists(mom_src), "R/02_momentum.R not present")
  txt <- decomment(readLines(mom_src, warn = FALSE))
  joined <- paste(txt, collapse = "\n")

  # The exact call that caused this. `to` is exclusive, so this can never reach
  # the session that just closed.
  expect_false(grepl("to\\s*=\\s*Sys\\.Date\\(\\)", joined),
               info = "to = Sys.Date() is exclusive and loses the latest session")

  # It must reach one day PAST today for the window to include today at all.
  expect_match(joined, "to\\s*=\\s*today_utc\\s*\\+\\s*1")

  # And having reached that far, it must drop whatever is not finished.
  expect_match(joined, "last_finished_session\\(")
  expect_match(joined, "filter\\(date\\s*<=\\s*last_session\\)")
})

test_that("a pull behind the benchmark warns instead of publishing quietly", {
  skip_if_not(file.exists(mom_src), "R/02_momentum.R not present")
  txt <- decomment(readLines(mom_src, warn = FALSE))
  joined <- paste(txt, collapse = "\n")

  # The gap must be computed from the benchmark's own dates, not a calendar.
  expect_match(joined, "sessions_behind\\(newest,\\s*spy_days\\)")
  # ...and it must be a warning(), which the pipeline surfaces, not a message()
  # buried in a step log that runs to thousands of lines.
  wi <- grep("sessions_behind\\(newest", txt)
  expect_length(wi, 1L)
  expect_match(paste(txt[wi:min(length(txt), wi + 12L)], collapse = "\n"),
               "warning\\(")
})
