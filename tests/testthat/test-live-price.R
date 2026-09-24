# Which price the screen shows, and whether it says how old it is.
#
# The ticker tape has polled Yahoo every 60s for a while, but Deep Dive read
# the close stored by the nightly run. Both sat on screen at once disagreeing —
# the tape showed PD at $14.83 while the panel below said $14.46 — which reads
# as a bug rather than as two timestamps. These cover the choice and, more
# importantly, that a price is never shown without a label saying which it is.

if (have_pkgs("dplyr")) {
  gl <- repo_path("app", "global.R")
  have_pick <- FALSE
  if (file.exists(gl)) {
    src <- readLines(gl, warn = FALSE)
    from <- grep("^pick_price <- function", src)
    to   <- grep("^STORED_AS_OF <- local", src)
    if (length(from) && length(to) && to[1] > from[1]) {
      eval(parse(text = paste(src[from[1]:(to[1] - 1)], collapse = "\n")), envir = globalenv())
      have_pick <- exists("pick_price")
    }
  }

  test_that("a usable live quote wins over the stored close", {
    skip_if_not(have_pick, "pick_price unavailable")
    p <- pick_price(live_price = 14.83, live_chg = 2.1,
                    live_time = as.POSIXct("2026-09-23 16:00:01"),
                    stored_close = 14.46, stored_chg = 0.4,
                    stored_date = as.Date("2026-09-21"),
                    now = as.POSIXct("2026-09-23 16:00:30"))
    expect_equal(p$price, 14.83)
    expect_equal(p$chg, 2.1)
    expect_true(p$is_live)
    expect_true(grepl("LIVE", p$label))
    expect_true(grepl("16:00", p$label))
  })

  test_that("the stored close is used when no quote comes back", {
    skip_if_not(have_pick, "pick_price unavailable")
    p <- pick_price(stored_close = 14.46, stored_chg = 0.4,
                    stored_date = as.Date("2026-09-21"))
    expect_equal(p$price, 14.46)
    expect_false(p$is_live)
    expect_true(grepl("close", p$label))
    expect_true(grepl("Sep 21", p$label))
  })

  # A quote feed that fails often returns something unusable rather than
  # nothing. Treat those as absent, not as a price.
  test_that("unusable quotes fall back instead of being displayed", {
    skip_if_not(have_pick, "pick_price unavailable")
    for (bad in list(NA_real_, 0, -5, NaN, Inf, numeric(0), c(1, 2))) {
      p <- pick_price(live_price = bad, stored_close = 14.46,
                      stored_date = as.Date("2026-09-21"))
      expect_false(p$is_live, info = paste("accepted a bad price:", paste(bad, collapse = ",")))
      expect_equal(p$price, 14.46)
    }
  })

  test_that("a price is never shown without a label", {
    skip_if_not(have_pick, "pick_price unavailable")
    cases <- list(
      pick_price(live_price = 10, live_time = NULL),
      pick_price(stored_close = 10, stored_date = NULL),
      pick_price(),
      pick_price(live_price = 10, live_time = as.POSIXct("2026-09-23 09:31:00"))
    )
    for (p in cases) {
      expect_true(is.character(p$label) && nzchar(p$label))
    }
  })

  # Found by probing during the audit. quote_cache_get ages the FETCH, not the
  # TRADE, so a quote pulled on Sunday for Friday's last print looked fresh all
  # the way through and came out badged green "LIVE 16:00".
  test_that("a trade from an earlier session is not badged live", {
    skip_if_not(have_pick, "pick_price unavailable")
    p <- pick_price(live_price = 14.83, live_chg = 2.1,
                    live_time = as.POSIXct("2026-09-18 16:00:00"),   # Friday
                    stored_close = 14.46, stored_date = as.Date("2026-09-18"),
                    now = as.POSIXct("2026-09-20 10:00:00"))         # Sunday
    expect_false(p$is_live)
    expect_false(grepl("LIVE", p$label))
    expect_true(grepl("Sep 18", p$label))   # the day is on the badge now
    expect_equal(p$price, 14.83)            # still the last price anyone paid
    expect_equal(p$chg, 2.1)
  })

  test_that("a trade from today is still live", {
    skip_if_not(have_pick, "pick_price unavailable")
    p <- pick_price(live_price = 14.83,
                    live_time = as.POSIXct("2026-09-24 09:31:00"),
                    now = as.POSIXct("2026-09-24 09:31:20"))
    expect_true(p$is_live)
    expect_true(grepl("LIVE 09:31", p$label))
  })

  test_that("an untimed quote is still live but carries no stamp", {
    skip_if_not(have_pick, "pick_price unavailable")
    p <- pick_price(live_price = 14.83, live_time = NULL)
    expect_true(p$is_live)
    expect_equal(p$label, "LIVE")
  })

  test_that("a missing change does not masquerade as a flat day", {
    skip_if_not(have_pick, "pick_price unavailable")
    p <- pick_price(live_price = 10, live_chg = NA_real_)
    expect_true(is.na(p$chg))   # the caller decides how to render it
  })
}
