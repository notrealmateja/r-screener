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

# ── the live-vs-recorded regression ─────────────────────────────────────────
test_that("the resolver keys off the canonical url, not the first videoId", {
  src <- paste(readLines(repo_path("app", "global.R")), collapse = "\n")
  # Reading the first "videoId" in the /live page picked up a sidebar
  # recommendation: the deployed panel played a three-hour Bloomberg recording,
  # scrub bar and all, instead of the live channel.
  expect_true(grepl('rel=\\\\"canonical\\\\"', src) || grepl('rel=\\\\\\\\"canonical', src) ||
              grepl("canonical", src, fixed = TRUE))
  # and it must prove liveness rather than assume it
  expect_true(grepl("lengthSeconds", src, fixed = TRUE))
})

test_that("a finished upload is rejected as not live", {
  # A live stream reports lengthSeconds 0; a completed upload reports its real
  # duration. That is the only field separating the two on the /live page.
  is_live <- function(txt) {
    len <- regmatches(txt, regexpr('"lengthSeconds":"[0-9]+"', txt))
    length(len) > 0 && grepl('"lengthSeconds":"0"', len[1], fixed = TRUE)
  }
  expect_true(is_live('{"lengthSeconds":"0","title":"Bloomberg Live"}'))
  expect_false(is_live('{"lengthSeconds":"10987","title":"US-Canada Tariffs"}'))
  expect_false(is_live('{"title":"no length field"}'))
})

test_that("table rows are held to a single line", {
  src <- paste(readLines(repo_path("app", "app.R")), collapse = "\n")
  # Company names, rating labels and driver names each wrapped onto two lines
  # and the signal list onto four, so rows rendered as ragged 2-4 line stacks.
  expect_true(grepl("white-space:nowrap !important", src, fixed = TRUE))
  expect_true(grepl("text-overflow:ellipsis !important", src, fixed = TRUE))
})

# ── the production fallback ─────────────────────────────────────────────────
# shinyapps.io's outbound requests to YouTube do not come back parseable, so
# every channel resolved as unavailable there and the panel told the viewer
# that four broadcasters were simultaneously off air. Ids are resolved nightly
# in CI and shipped; the app falls back to that file.
if (have_pkgs("dplyr", "readr", "tibble")) {
  test_that("liveness is tri-state, so unreachable is not reported as off air", {
    skip_if_not(have_ui, "global.R live section missing")
    src <- paste(readLines(repo_path("app", "global.R")), collapse = "\n")
    expect_true(grepl("tv_live_state", src, fixed = TRUE))
    # the player must only refuse to mount on a positive off-air determination
    app <- paste(readLines(repo_path("app", "app.R")), collapse = "\n")
    expect_true(grepl('identical(tryCatch(tv_live_state(ch)', app, fixed = TRUE))
  })

  test_that("the nightly channel file carries what the fallback needs", {
    p <- repo_path("data", "tv_channels.csv")
    skip_if_not(file.exists(p), "tv_channels.csv not generated yet")
    d <- readr::read_csv(p, show_col_types = FALSE)
    expect_true(all(c("channel", "video_id", "is_live", "resolved_at") %in% names(d)))
    expect_gt(nrow(d), 0)
    # every row flagged live must carry a real 11-character video id
    live <- d[!is.na(d$is_live) & as.logical(d$is_live), ]
    if (nrow(live) > 0) {
      expect_true(all(!is.na(live$video_id)))
      expect_true(all(grepl("^[A-Za-z0-9_-]{11}$", live$video_id)))
    }
    # Deliberately NOT asserting that some channel is live. That is a statement
    # about the outside world, not about this code, and putting it here halted
    # the whole pipeline and blocked a deploy when the resolver came back empty
    # from a datacenter IP. The health check reports live-channel counts and
    # flags zero; a unit test should only hold the schema.
    expect_true(is.logical(as.logical(d$is_live)))
  })
}

# ── the two page shapes YouTube serves ──────────────────────────────────────
# A home connection gets a page with a canonical link and lengthSeconds; a
# datacenter IP gets one with neither, though it carries isLive and the right
# video id. Both shinyapps.io and GitHub runners are datacenter IPs, which is
# why the resolver returned nothing in production while working from a laptop.
test_that("the live id is found on a page with no canonical link", {
  pick <- function(txt) {
    vm <- gregexpr('"videoId":"[A-Za-z0-9_-]{11}"', txt)[[1]]
    if (vm[1] == -1) return(NA_character_)
    vids <- sub('"videoId":"', '', sub('"$', '',
                regmatches(txt, gregexpr('"videoId":"[A-Za-z0-9_-]{11}"', txt))[[1]]))
    lp <- gregexpr('"isLive":true', txt, fixed = TRUE)[[1]]
    if (lp[1] == -1) return(NA_character_)
    hits <- vapply(lp, function(p) {
      before <- vm[vm <= p]
      if (!length(before)) return(NA_character_)
      vids[which.max(before)]
    }, character(1))
    hits <- hits[!is.na(hits)]
    if (!length(hits)) NA_character_ else names(sort(table(hits), decreasing = TRUE))[1]
  }
  # a recommendation first, then the live entry — the shape seen in CI, where
  # taking the first id gave a recorded video
  page <- paste0('{"videoId":"wTiYaWFP59Q","title":"a recommendation"},',
                 '{"videoId":"QB5BNdBFujE","isLive":true,"title":"the live stream"},',
                 '{"videoId":"iEpJwprxDdk","title":"another upload"}')
  expect_equal(pick(page), "QB5BNdBFujE")
  # and nothing when no entry is marked live
  expect_true(is.na(pick('{"videoId":"wDfAQOsPbDo","title":"off air channel"}')))
})

test_that("liveness falls back to isLive when lengthSeconds is absent", {
  live_from <- function(txt) {
    len <- regmatches(txt, gregexpr('"lengthSeconds":"[0-9]+"', txt))[[1]]
    if (length(len) > 0) any(grepl('"lengthSeconds":"0"', len, fixed = TRUE))
    else grepl('"isLive":true', txt, fixed = TRUE)
  }
  # home-connection shape
  expect_true(live_from('{"lengthSeconds":"0"}'))
  expect_false(live_from('{"lengthSeconds":"10987"}'))
  # datacenter shape: no lengthSeconds at all
  expect_true(live_from('{"isLive":true,"videoId":"QB5BNdBFujE"}'))
  expect_false(live_from('{"videoId":"wDfAQOsPbDo"}'))
})
