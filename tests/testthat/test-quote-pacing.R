# Live-quote pacing.
#
# The tape polled every 60s from every session, around the clock. Raising that
# to 10s is only safe if three things hold: every session shares one fetch, a
# refusal backs all of them off at once, and the app stops polling hard when
# nothing is trading. The last is deliberately NOT a hardcoded holiday
# calendar -- a baked-in list of dates is a constant that silently goes stale,
# the same class of bug as the frozen vendor fields this app already had to
# label. It asks the feed instead.
#
# Every block below registers unconditionally and skips from the inside, so an
# unavailable dependency is a countable skip rather than a block that silently
# ceases to exist. See "Tests must fail, not vanish" in test-pipeline-provenance.R.

pacing_ready <- FALSE
if (have_pkgs("dplyr")) {
  gl <- repo_path("app", "global.R")
  if (file.exists(gl)) {
    gsrc <- readLines(gl, warn = FALSE)
    from <- grep("^QUOTE_TTL <- ", gsrc)
    if (length(from)) {
      eval(parse(text = paste(gsrc[from[1]:length(gsrc)], collapse = "\n")),
           envir = globalenv())
      pacing_ready <- all(vapply(
        c("market_is_trading", "quote_poll_interval", "quote_cache_get_many",
          "quote_note_success", "quote_note_failure", "ms_to_next_open"),
        exists, logical(1)))
    }
  }
}

pace_app_path <- repo_path("app", "app.R")
pace_app <- if (file.exists(pace_app_path))
  paste(readLines(pace_app_path, warn = FALSE), collapse = "\n") else ""

ny <- function(s) as.POSIXct(s, tz = "America/New_York")
reset_state <- function() {
  .quote_state$fails <- 0L
  .quote_state$blocked_until <- as.POSIXct(NA)
  .quote_state$traded_today <- NA
}

test_that("the timezone database actually resolves America/New_York", {
  # Everything below is expressed in New York time. If the container has no
  # tzdata, R silently falls back and the zone becomes UTC -- and because these
  # tests ALSO construct their times in that zone, both sides shift together
  # and every assertion still passes while production polls on the wrong hours.
  # Only an absolute offset can catch that.
  off <- function(s, tz) as.numeric(as.POSIXct(s, tz = tz))
  nov <- (off("2026-11-06 12:00:00", "UTC") - off("2026-11-06 12:00:00", "America/New_York")) / 3600
  jul <- (off("2026-07-01 12:00:00", "UTC") - off("2026-07-01 12:00:00", "America/New_York")) / 3600
  expect_equal(nov, -5)   # EST, after DST ends
  expect_equal(jul, -4)   # EDT
})

test_that("the session opens at 09:30 and the close is exclusive", {
  skip_if_not(pacing_ready, "pacing unavailable")
  expect_false(market_is_trading(ny("2026-11-06 09:29:59")))   # Friday
  expect_true( market_is_trading(ny("2026-11-06 09:30:00")))
  expect_true( market_is_trading(ny("2026-11-06 15:59:59")))
  # 16:00:00 is the close itself. An inclusive bound would keep the fast poll
  # running against a shut market.
  expect_false(market_is_trading(ny("2026-11-06 16:00:00")))
})

test_that("weekends are not trading days", {
  skip_if_not(pacing_ready, "pacing unavailable")
  expect_false(market_is_trading(ny("2026-11-07 12:00:00")))
  expect_false(market_is_trading(ny("2026-11-08 12:00:00")))
})

test_that("a closed market polls slowly and an open one polls fast", {
  skip_if_not(pacing_ready, "pacing unavailable")
  reset_state()
  expect_equal(quote_poll_interval(ny("2026-11-07 12:00:00")), QUOTE_POLL_SLOW)
  .quote_state$traded_today <- TRUE
  expect_equal(quote_poll_interval(ny("2026-11-06 12:00:00")), QUOTE_POLL_FAST)
})

test_that("an unobserved feed does not slow the open", {
  skip_if_not(pacing_ready, "pacing unavailable")
  reset_state()
  expect_true(is.na(.quote_state$traded_today))
  expect_equal(quote_poll_interval(ny("2026-11-06 09:30:05")), QUOTE_POLL_FAST)
})

test_that("a holiday is detected from the feed, not from a calendar", {
  skip_if_not(pacing_ready, "pacing unavailable")
  reset_state()
  .quote_state$traded_today <- FALSE
  expect_equal(quote_poll_interval(ny("2026-11-06 12:00:00")), QUOTE_POLL_IDLE)
  # Never worse than the rate it replaced.
  expect_equal(QUOTE_POLL_IDLE, 60000L)
})

test_that("the idle demotion lifts again once the feed trades", {
  skip_if_not(pacing_ready, "pacing unavailable")
  reset_state()
  now <- ny("2026-11-06 12:00:00")
  quote_note_success(qtimes = ny("2026-11-05 16:00:00"), now = now, basket = TRUE)
  expect_false(.quote_state$traded_today)
  # The self-correcting half. Without FALSE -> TRUE the feed-driven holiday
  # check would be a one-way latch, which is worse than the calendar it
  # replaced: it would never speed back up.
  quote_note_success(qtimes = ny("2026-11-06 11:59:00"), now = now, basket = TRUE)
  expect_true(.quote_state$traded_today)
  expect_equal(quote_poll_interval(now), QUOTE_POLL_FAST)
})

test_that("a single symbol may not answer a process-wide question", {
  skip_if_not(pacing_ready, "pacing unavailable")
  reset_state()
  now <- ny("2026-11-06 12:00:00")
  .quote_state$traded_today <- TRUE
  # dd_quote reports ONE stock. If that set traded_today, one viewer on a thin
  # name that had not printed today would demote the tape -- and every other
  # session -- from 10s to 60s.
  quote_note_success(qtimes = ny("2026-11-05 16:00:00"), now = now, basket = FALSE)
  expect_true(.quote_state$traded_today)
  expect_equal(quote_poll_interval(now), QUOTE_POLL_FAST)
  # The basket may.
  quote_note_success(qtimes = ny("2026-11-05 16:00:00"), now = now, basket = TRUE)
  expect_false(.quote_state$traded_today)
})

test_that("a refusal backs off, and the wait grows then caps", {
  skip_if_not(pacing_ready, "pacing unavailable")
  reset_state()
  now <- ny("2026-11-06 12:00:00")
  expect_false(quote_is_blocked(now))
  expect_equal(vapply(1:6, function(i) quote_note_failure(now), numeric(1)),
               c(15, 30, 60, 120, 300, 300))
  expect_true(quote_is_blocked(now))
  expect_false(quote_is_blocked(now + 301))   # must expire on its own
})

test_that("one success clears the backoff", {
  skip_if_not(pacing_ready, "pacing unavailable")
  reset_state()
  now <- ny("2026-11-06 12:00:00")
  quote_note_failure(now); quote_note_failure(now)
  expect_true(quote_is_blocked(now))
  quote_note_success(qtimes = now, now = now, basket = TRUE)
  expect_false(quote_is_blocked(now))
  expect_equal(quote_note_failure(now), 15)   # reset, not decrement
})

test_that("an idle session does not sleep through the opening bell", {
  skip_if_not(pacing_ready, "pacing unavailable")
  reset_state()
  # A flat 300s scheduled at 09:29 meant the next poll could land at 09:34,
  # showing a pre-open price through five minutes of live trading.
  expect_equal(quote_poll_interval(ny("2026-11-06 09:29:00")), 60000)
  expect_lte(quote_poll_interval(ny("2026-11-06 09:25:00")), QUOTE_POLL_SLOW)
  # Far from any open it is simply capped.
  expect_equal(quote_poll_interval(ny("2026-11-06 02:00:00")), QUOTE_POLL_SLOW)
  # And the bell is found across a weekend rather than skipped.
  expect_gt(ms_to_next_open(ny("2026-11-07 12:00:00")), 0)
})

test_that("the cache TTL tracks the poll rate instead of a fixed 60s", {
  skip_if_not(pacing_ready, "pacing unavailable")
  reset_state()
  .quote_state$traded_today <- TRUE
  expect_equal(quote_fresh_ttl(ny("2026-11-06 12:00:00")), QUOTE_POLL_FAST / 1000)
  expect_equal(quote_fresh_ttl(ny("2026-11-07 12:00:00")), QUOTE_POLL_SLOW / 1000)
})

test_that("the batch read splits what is fresh from what must be fetched", {
  skip_if_not(pacing_ready, "pacing unavailable")
  quote_cache_put("PACE1", 10, 1, ny("2026-11-06 12:00:00"))
  r <- quote_cache_get_many(c("PACE1", "PACE_MISSING"), ttl = 10, now = Sys.time())
  expect_equal(names(r$hits), "PACE1")
  expect_equal(r$missing, "PACE_MISSING")
})

test_that("an entry past the TTL is reported missing, not served", {
  skip_if_not(pacing_ready, "pacing unavailable")
  quote_cache_put("PACE2", 10, 1, Sys.time())
  r <- quote_cache_get_many("PACE2", ttl = -1, now = Sys.time())
  expect_equal(length(r$hits), 0L)
  expect_equal(r$missing, "PACE2")
})

test_that("the batch read ignores blank and duplicate symbols", {
  skip_if_not(pacing_ready, "pacing unavailable")
  quote_cache_put("PACE3", 10, 1, Sys.time())
  r <- quote_cache_get_many(c("PACE3", "PACE3", "", NA_character_), ttl = 600,
                            now = Sys.time())
  expect_equal(names(r$hits), "PACE3")
  expect_equal(length(r$missing), 0L)
})

# ── Structural guards on the wiring ─────────────────────────────────────────
# The pacing only pays off if app.R actually uses it. Each of these pins one
# specific call site; a bare grep for a function name elsewhere in the file is
# not enough, which two surviving mutations already proved.

test_that("the tape consults the shared cache before it fetches", {
  skip_if_not(nzchar(pace_app), "app.R unreadable")
  body <- substr(sub(".*live_quotes <- reactive\\(\\{", "", pace_app), 1, 4500)
  # Anchored on the unconditional read AND its early return. The backoff branch
  # calls quote_cache_get_many too and sits above getQuote, so an ordering check
  # that accepts any occurrence stays green with the primary read stubbed out.
  i_cache  <- regexpr("cached <- quote_cache_get_many(top_syms)", body, fixed = TRUE)[1]
  i_return <- regexpr("if (!length(cached$missing)) return(as_rows(cached$hits))",
                      body, fixed = TRUE)[1]
  i_fetch  <- regexpr("getQuote(cached$missing)", body, fixed = TRUE)[1]
  expect_true(i_cache > 0); expect_true(i_return > 0); expect_true(i_fetch > 0)
  expect_lt(i_cache, i_return)
  expect_lt(i_return, i_fetch)
})

test_that("the fetch writes its result back to the shared cache", {
  skip_if_not(nzchar(pace_app), "app.R unreadable")
  # Without the write-back every session fetches for itself and the whole
  # "one fetch shared by all viewers" argument for the faster rate collapses.
  body <- substr(sub(".*live_quotes <- reactive\\(\\{", "", pace_app), 1, 4500)
  expect_true(grepl("quote_cache_put(out$symbol, out$price, out$change_pct, out$qtime)",
                    body, fixed = TRUE))
})

test_that("both quote reactives are paced, not pinned to a constant", {
  skip_if_not(nzchar(pace_app), "app.R unreadable")
  # Two of them: the tape and dd_quote. One occurrence would leave the other
  # on a fixed interval while this assertion stayed green.
  n <- length(gregexpr("invalidateLater(quote_poll_interval())", pace_app,
                       fixed = TRUE)[[1]])
  expect_gte(n, 2L)
  # Exactly one fixed interval survives, and only inside dd_quote_chart, where
  # the slow redraw is the point. Anywhere else it would be a quote path that
  # quietly stopped following the pacer.
  fixed_hits <- gregexpr("invalidateLater(60000)", pace_app, fixed = TRUE)[[1]]
  expect_equal(length(fixed_hits), 1L)
  chart <- regexpr("dd_quote_chart <- reactive({", pace_app, fixed = TRUE)[1]
  expect_true(chart > 0)
  expect_gt(fixed_hits[1], chart)
  expect_lt(fixed_hits[1], chart + 400)
})

test_that("both cache reads ask for the pacing TTL, not the old constant", {
  skip_if_not(nzchar(pace_app), "app.R unreadable")
  # quote_fresh_ttl can track the rate perfectly and still achieve nothing if
  # its callers keep passing the fixed 60.
  expect_true(grepl("quote_cache_get(sym, ttl = quote_fresh_ttl())", pace_app, fixed = TRUE))
  expect_true(grepl("cached <- quote_cache_get_many(top_syms)", pace_app, fixed = TRUE))
})

test_that("the app consults the shared backoff on both paths", {
  skip_if_not(nzchar(pace_app), "app.R unreadable")
  # quote_is_blocked() being correct is worthless if nothing ever calls it:
  # the backoff is the brake that justifies the faster rate.
  expect_gte(length(gregexpr("quote_is_blocked()", pace_app, fixed = TRUE)[[1]]), 2L)
})

test_that("a response with no usable rows is treated as a refusal", {
  skip_if_not(nzchar(pace_app), "app.R unreadable")
  # A 200 whose fields are all empty throws nothing. Counted as success it
  # would clear the backoff and keep the app hammering a refusing feed.
  expect_true(grepl("if (!nrow(out)) stop(", pace_app, fixed = TRUE))
})

test_that("only the basket poll answers the process-wide trading question", {
  skip_if_not(nzchar(pace_app), "app.R unreadable")
  expect_true(grepl("quote_note_success(out$qtime, basket = TRUE)", pace_app, fixed = TRUE))
  expect_true(grepl("quote_note_success(tt, basket = FALSE)", pace_app, fixed = TRUE))
})

test_that("the tape is rebuilt in score order, not cache-hit order", {
  skip_if_not(nzchar(pace_app), "app.R unreadable")
  # bind_rows puts every cache hit ahead of every fetched row, so a partly warm
  # cache would reshuffle the tape between ticks.
  expect_true(grepl("rows[order(match(rows$symbol, top_syms)), ]", pace_app, fixed = TRUE))
})

test_that("the tape markup does not depend on prices", {
  skip_if_not(nzchar(pace_app), "app.R unreadable")
  # .ticker-content carries a 60s scroll animation and replacing the node
  # restarts it from 0%. At a 10s poll that snaps the marquee back six times a
  # cycle. The renderUI must read the seed only; prices arrive as a patch.
  # Bounded at the observe that follows it, or the price-patch block below
  # would be mistaken for part of the renderer.
  body <- sub(".*output\\$ticker_tape <- renderUI\\(\\{", "", pace_app)
  body <- sub("\n  observe\\(\\{.*$", "", body)
  expect_false(grepl("live_quotes()", body, fixed = TRUE))
  expect_true(grepl("seed <- tape_seed()", body, fixed = TRUE))
  expect_true(grepl("edgeTape", pace_app, fixed = TRUE))
})

test_that("the price chart does not ride the fast quote cadence", {
  skip_if_not(nzchar(pace_app), "app.R unreadable")
  # output$dd_price rebuilds seven traces over ~365 rows and every push
  # discards the viewer's zoom. It keeps the 60s beat and reads the cache only.
  expect_true(grepl("lq <- dd_quote_chart()", pace_app, fixed = TRUE))
  # Matched on the ASSIGNMENT, not the bare name: the comment on the very next
  # line explains the choice and quotes dd_quote() literally, which a looser
  # grep reads as the call it is meant to forbid.
  body <- substr(sub(".*output\\$dd_price <- renderPlotly\\(\\{", "", pace_app), 1, 6000)
  expect_false(grepl("lq <- dd_quote()", body, fixed = TRUE))
})

test_that("stale prices stop the strip claiming to be live", {
  skip_if_not(nzchar(pace_app), "app.R unreadable")
  # The backed-off branch serves cache entries up to 300s old through the same
  # renderer as fresh ones. The label beside them says LIVE, so it has to change.
  expect_true(grepl("stale = isTRUE(quote_is_blocked())", pace_app, fixed = TRUE))
  expect_true(grepl("lab.classList.toggle('delayed', !!d.stale);", pace_app, fixed = TRUE))
})
