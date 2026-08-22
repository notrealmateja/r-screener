# Live wire (RSS polling) and Live TV embeds.
#
# The nightly CSV cannot change during the day, so a timer that re-read it
# would redraw identical rows. The wire re-fetches the feeds instead.

if (have_pkgs("dplyr", "tibble")) {
  suppressMessages({ library(dplyr); library(tibble) })

  ui_src <- readLines(repo_path("app", "global.R"))
  ui_start <- grep("^LIVE_NEWS_TTL", ui_src)[1]
  if (!is.na(ui_start) && have_pkgs("xml2")) {
    suppressMessages(library(xml2))
    eval(parse(text = paste(ui_src[ui_start:length(ui_src)], collapse = "\n")))
  }
  have_ui <- exists("parse_feed_time") && exists("tv_embed_url")

  # ── feed timestamps ──────────────────────────────────────────────────────
  test_that("RFC-822 pubDates parse", {
    skip_if_not(have_ui, "global.R live section missing")
    expect_false(is.na(parse_feed_time("Sat, 22 Aug 2026 17:01:18 GMT")))
    expect_false(is.na(parse_feed_time("Mon, 27 Jan 2025 14:26:00 -0500")))
  })

  test_that("a non-RFC-822 pubDate still parses", {
    skip_if_not(have_ui, "global.R live section missing")
    # Investing.com emits plain "2026-08-22 17:04:56". Parsing only RFC-822 left
    # those items with no timestamp, so they sorted last and were cut by the
    # item cap — the feed looked broken when it was only mis-parsed.
    t <- parse_feed_time("2026-08-22 17:04:56")
    expect_false(is.na(t))
    expect_equal(format(t, "%Y-%m-%d"), "2026-08-22")
  })

  test_that("mixed formats in one vector all parse", {
    skip_if_not(have_ui, "global.R live section missing")
    v <- parse_feed_time(c("Sat, 22 Aug 2026 17:01:18 GMT",
                           "2026-08-22 17:04:56",
                           "2026-08-22T17:04:56"))
    expect_equal(sum(!is.na(v)), 3)
  })

  test_that("an unparseable date yields NA rather than erroring", {
    skip_if_not(have_ui, "global.R live section missing")
    expect_true(is.na(parse_feed_time("not a date")))
    expect_equal(length(parse_feed_time(character(0))), 0)
  })

  test_that("the stale WSJ feed is not in the feed list", {
    skip_if_not(have_ui, "global.R live section missing")
    # It still serves, but its newest item is dated January 2025. A stale feed
    # is worse than no feed because it reads as coverage.
    expect_false(any(grepl("dj\\.com", LIVE_NEWS_FEEDS)))
    expect_gte(length(LIVE_NEWS_FEEDS), 2)
    expect_true(all(grepl("^https://", LIVE_NEWS_FEEDS)))
    expect_true(all(nzchar(names(LIVE_NEWS_FEEDS))))
  })

  # ── live TV ──────────────────────────────────────────────────────────────
  # tv_embed_url resolves the current live video id over the network, so these
  # stub the resolver: the URL shape is the thing under test, not YouTube.
  with_stub_id <- function(id, expr) {
    orig <- tv_live_video_id
    assign("tv_live_video_id", function(channel) id, envir = environment(tv_embed_url))
    on.exit(assign("tv_live_video_id", orig, envir = environment(tv_embed_url)), add = TRUE)
    force(expr)
  }

  test_that("a live channel embeds its resolved video id", {
    skip_if_not(have_ui, "global.R live section missing")
    # /embed/live_stream?channel= is the documented form, but it renders "This
    # video is unavailable" even while the channel is demonstrably live, so the
    # current video id is resolved and embedded directly instead.
    with_stub_id("QB5BNdBFujE", {
      u <- tv_embed_url("Bloomberg TV")
      expect_match(u, "^https://www\\.youtube\\.com/embed/QB5BNdBFujE\\?")
      expect_false(grepl("live_stream", u, fixed = TRUE))
    })
  })

  test_that("an unresolvable channel falls back rather than erroring", {
    skip_if_not(have_ui, "global.R live section missing")
    with_stub_id(NA_character_, {
      u <- tv_embed_url("Bloomberg TV")
      expect_match(u, "live_stream\\?channel=")
      expect_match(u, TV_CHANNELS[["Bloomberg TV"]], fixed = TRUE)
    })
  })

  test_that("no video id is hardcoded in source", {
    skip_if_not(have_ui, "global.R live section missing")
    # Ids change whenever the broadcaster restarts the stream, so a pinned id
    # dies within days. Only the channel ids may live in source.
    src <- paste(readLines(repo_path("app", "global.R")), collapse = "\n")
    expect_false(grepl('embed/[A-Za-z0-9_-]{11}\\?autoplay', src))
  })

  test_that("streams start muted, since browsers block autoplay with sound", {
    skip_if_not(have_ui, "global.R live section missing")
    with_stub_id("QB5BNdBFujE", expect_match(tv_embed_url("Bloomberg TV"), "mute=1", fixed = TRUE))
    with_stub_id(NA_character_,  expect_match(tv_embed_url("Bloomberg TV"), "mute=1", fixed = TRUE))
  })

  test_that("channel ids look like real YouTube channel ids", {
    skip_if_not(have_ui, "global.R live section missing")
    expect_gte(length(TV_CHANNELS), 2)
    for (id in TV_CHANNELS) expect_match(id, "^UC[A-Za-z0-9_-]{22}$")
  })

  # ── the chart hover regression ───────────────────────────────────────────
  # Strip comments first: the fix is described in a comment that names the old
  # setting, and a naive grep over the whole file matches that prose.
  app_code <- function() {
    l <- readLines(repo_path("app", "app.R"))
    paste(l[!grepl("^\\s*#", l)], collapse = "\n")
  }

  test_that("the price chart no longer uses unified hover", {
    src <- app_code()
    # "x unified" stacks price, three moving averages and both Bollinger bands
    # into one tall tooltip that jumps as the cursor moves.
    expect_false(grepl('hovermode\\s*=\\s*"x unified"', src))
    expect_true(grepl('hovertemplate', src))
    # the overlays must not answer the hover
    expect_true(grepl('name="MA20",\\s*hoverinfo="skip"', src))
    expect_true(grepl('name="MA200",\\s*hoverinfo="skip"', src))
  })

  test_that("leaving the overview stops the embedded stream", {
    src <- app_code()
    # The player lives on the Overview pane. Panes are hidden with CSS rather
    # than unmounted, so without this the video keeps playing, and keeps making
    # sound, while the viewer is on another tab.
    expect_true(grepl("about:blank", src, fixed = TRUE))
    expect_true(grepl("pane-overview iframe", src, fixed = TRUE))
    # the standalone TV tab was folded into Overview
    expect_false(grepl('id="pane-tv"', src, fixed = TRUE))
  })

  test_that("the all-100% confidence columns are gone from the tables", {
    src <- app_code()
    # sweet_spot_confidence and unicorn_confidence each had exactly one
    # distinct value across the universe, so the bar drew an identical green
    # line on every row and told the reader nothing.
    expect_false(grepl("Confidence\\s*=\\s*sapply", src))
    expect_false(grepl("`Wk Conf`\\s*=", src))
  })
}
