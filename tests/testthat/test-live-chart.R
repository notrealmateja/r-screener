# Carrying the price line forward to the live quote.
#
# The chart drew price_history, which ends at the last nightly bar, so it
# stopped a day or two short of the price printed in the header above it. The
# risk in fixing that is inventing data: a quote is a price, not a bar, and the
# moving averages and Bollinger bands were computed by the pipeline. These
# tests pin that only the close is ever filled in.

if (have_pkgs("dplyr")) {
  gl <- repo_path("app", "global.R")
  have_fn <- FALSE
  if (file.exists(gl)) {
    src <- readLines(gl, warn = FALSE)
    from <- grep("^append_live_point <- function", src)
    if (length(from)) {
      eval(parse(text = paste(src[from[1]:length(src)], collapse = "\n")),
           envir = globalenv())
      have_fn <- exists("append_live_point")
    }
  }

  bars <- function() data.frame(
    date   = as.Date("2026-09-17") + 0:4,        # 17th .. 21st
    close  = c(10, 11, 12, 13, 14),
    ma20   = c(9, 9.5, 10, 10.5, 11),
    volume = c(100, 110, 120, 130, 140)
  )

  test_that("a newer quote extends the line by one point", {
    skip_if_not(have_fn, "append_live_point unavailable")
    out <- append_live_point(bars(), 15.5, as.POSIXct("2026-09-23 16:00:00"),
                             now = as.POSIXct("2026-09-23 16:00:30"))
    expect_equal(nrow(out), 6)
    expect_equal(as.Date(out$date[6]), as.Date("2026-09-23"))
    expect_equal(out$close[6], 15.5)
    expect_true(out$is_live[6])
    expect_false(any(out$is_live[1:5]))
  })

  # The point of the whole exercise: do not invent a bar.
  test_that("only the close is filled in on the added point", {
    skip_if_not(have_fn, "append_live_point unavailable")
    out <- append_live_point(bars(), 15.5, as.POSIXct("2026-09-23 16:00:00"),
                             now = as.POSIXct("2026-09-23 16:00:30"))
    expect_true(is.na(out$ma20[6]))
    expect_true(is.na(out$volume[6]))
    # and the settled history is untouched
    expect_equal(out$close[1:5], bars()$close)
    expect_equal(out$ma20[1:5], bars()$ma20)
  })

  test_that("a quote from the same session replaces rather than duplicates", {
    skip_if_not(have_fn, "append_live_point unavailable")
    out <- append_live_point(bars(), 14.9, as.POSIXct("2026-09-21 16:00:00"),
                             now = as.POSIXct("2026-09-21 16:00:30"))
    expect_equal(nrow(out), 5)                 # no duplicate x value
    expect_equal(out$close[5], 14.9)           # provisional close updated
    expect_true(out$is_live[5])
    expect_equal(anyDuplicated(out$date), 0)
  })

  test_that("a quote older than the chart is ignored", {
    skip_if_not(have_fn, "append_live_point unavailable")
    out <- append_live_point(bars(), 99, as.POSIXct("2026-09-18 16:00:00"),
                             now = as.POSIXct("2026-09-21 16:00:30"))
    expect_equal(nrow(out), 5)
    expect_equal(out$close, bars()$close)      # nothing rewritten
    expect_false(any(out$is_live))
  })

  test_that("unusable quotes leave the chart alone", {
    skip_if_not(have_fn, "append_live_point unavailable")
    for (bad in list(NA_real_, 0, -3, NaN, Inf, numeric(0), c(1, 2))) {
      out <- append_live_point(bars(), bad, as.POSIXct("2026-09-23 16:00:00"),
                               now = as.POSIXct("2026-09-23 16:00:30"))
      expect_equal(nrow(out), 5,
                   info = paste("accepted:", paste(bad, collapse = ",")))
    }
  })

  test_that("empty or malformed input is returned untouched", {
    skip_if_not(have_fn, "append_live_point unavailable")
    expect_equal(nrow(append_live_point(bars()[0, ], 15, Sys.time())), 0)
    expect_null(append_live_point(NULL, 15, Sys.time()))
    nodate <- data.frame(close = 1:3)
    expect_equal(append_live_point(nodate, 15, Sys.time()), nodate)
  })

  test_that("unsorted input is ordered before the point is added", {
    skip_if_not(have_fn, "append_live_point unavailable")
    b <- bars()[c(3, 1, 5, 2, 4), ]
    out <- append_live_point(b, 15.5, as.POSIXct("2026-09-23 16:00:00"),
                             now = as.POSIXct("2026-09-23 16:00:30"))
    expect_false(is.unsorted(as.Date(out$date)))
    expect_true(out$is_live[nrow(out)])
  })

  # Found by probing during the audit, alongside the same error in pick_price.
  # Over a weekend Yahoo keeps returning Friday's last print. It lands exactly
  # on the chart's final bar, so it overwrote that close and drew a green
  # "Live" marker on it — two days after the trade.
  test_that("a quote from an earlier session does not mark the last bar live", {
    skip_if_not(have_fn, "append_live_point unavailable")
    out <- append_live_point(bars(), 14.9, as.POSIXct("2026-09-21 16:00:00"),
                             now = as.POSIXct("2026-09-23 10:00:00"))
    expect_equal(nrow(out), 5)                  # nothing appended
    expect_false(any(out$is_live))              # and nothing badged
    expect_equal(out$close[5], bars()$close[5]) # the stored close stands
  })

  # as.Date() on a POSIXct converts in UTC no matter where the viewer is, so an
  # after-hours print past midnight UTC was dated tomorrow and drawn past the
  # end of the chart.
  test_that("an after-hours trade is dated by the local clock, not UTC", {
    skip_if_not(have_fn, "append_live_point unavailable")
    tt  <- as.POSIXct("2026-09-22 19:45:00")    # 19:45 local, past midnight UTC
    out <- append_live_point(bars(), 15.5, tt, now = as.POSIXct("2026-09-22 19:45:30"))
    expect_equal(format(as.Date(out$date[nrow(out)]), "%Y-%m-%d"), "2026-09-22")
    expect_true(out$is_live[nrow(out)])
  })
}
