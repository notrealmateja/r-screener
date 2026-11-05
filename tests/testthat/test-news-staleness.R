# The Market News Feed, and the gate that stopped failing the nightly over it.
#
# data/market_news.csv froze on 2026-10-06 when Alpha Vantage moved
# NEWS_SENTIMENT behind a paywall. For a month the panel kept rendering those
# 50 stories with no date on any of them, directly beneath LIVE WIRE, which
# stamps every item "3m ago". Nothing in the panel was false. The arrangement
# was: a reader scrolling from a column of "3m ago" into an undated list reads
# the second list as today's news too.
#
# Two things changed on 2026-11-05, and they only make sense together:
#   1. the app dates the snapshot -- per item, in the header, and in a banner;
#   2. the verifier stopped treating that frozen feed as fatal.
#
# (2) is only defensible because of (1), so these tests hold (1) in place. If
# the dating is removed, the exemption loses its justification and this file
# fails, which is the point: the alarm was not deleted, its job was moved into
# the page itself.

app_src  <- repo_path("app", "app.R")
vp_src   <- repo_path("tools", "verify_pipeline.R")

# ── the app tells the reader how old the snapshot is ────────────────────────

test_that("news_snapshot_age measures the newest story, and says so or says NA", {
  skip_if_not(file.exists(app_src), "app.R not present")
  src <- readLines(app_src, warn = FALSE)
  a <- grep("^  news_snapshot_age <- function", src)
  b <- grep("^  output\\$news_count <- renderUI", src)
  expect_length(a, 1)
  expect_length(b, 1)
  expect_lt(a[1], b[1])

  # Run it, do not grep it. A grep cannot tell this function from one that
  # returns 0 unconditionally.
  e <- new.env(parent = globalenv())
  eval(parse(text = paste(src[a[1]:(b[1] - 1)], collapse = "\n")), envir = e)
  f <- e$news_snapshot_age
  today <- as.Date("2026-11-05")

  # The age is taken from the NEWEST story, not the oldest and not the first
  # row, or a single recent item in a stale file would hide the freeze.
  d <- data.frame(publishedDate = c("2026-10-05T01:00:00", "2026-10-06T02:00:00",
                                    "2026-09-01"), stringsAsFactors = FALSE)
  expect_equal(f(d, today), 30)

  expect_equal(f(data.frame(publishedDate = "2026-11-05"), today), 0)
  expect_equal(f(data.frame(publishedDate = "2026-11-02"), today), 3)

  # Unknown is reported as unknown. Treating an unreadable date as fresh is
  # the exact failure this whole change exists to stop.
  expect_true(is.na(f(data.frame(publishedDate = c("", "not a date")), today)))
  expect_true(is.na(f(data.frame(headline = "x"), today)))
  expect_true(is.na(f(NULL, today)))
  expect_true(is.na(f(data.frame(publishedDate = character(0)), today)))

  # The threshold is pinned here too, so the cases above cannot keep passing
  # while the live cut moves out from under them.
  expect_match(paste(src, collapse = "\n"), "NEWS_STALE_DAYS <- 3")
})

test_that("every story in the panel carries its own date", {
  skip_if_not(file.exists(app_src), "app.R not present")
  src <- paste(readLines(app_src, warn = FALSE), collapse = "\n")

  # The item builds a date and renders it. Both halves are asserted: building
  # `when` and never printing it is the bug wearing a disguise.
  expect_match(src, "when     <- if \\(has_date\\)")
  expect_match(src, "if \\(is\\.na\\(dt\\)\\) \"UNDATED\"")
  expect_match(src, "span\\(when, style=", perl = TRUE)

  # The year is part of the stamp. "OCT 06" beside a live "3m ago" column
  # still reads as this October to someone skimming.
  expect_match(src, 'format\\(dt, "%b %d %Y"\\)')
})

test_that("the panel header and a banner both state the age", {
  skip_if_not(file.exists(app_src), "app.R not present")
  src <- paste(readLines(app_src, warn = FALSE), collapse = "\n")

  # Header: count AND age, never the bare count that was there before.
  expect_match(src, "STORIES · \\{round\\(age\\)\\} DAYS OLD")
  expect_match(src, "STORIES · DATE UNKNOWN")
  expect_false(grepl('glue("{nrow(news_data)} STORIES")', src, fixed = TRUE))

  # Banner: in words, above the list, for a reader who scrolled past the head.
  expect_match(src, "snap_age <- news_snapshot_age\\(\\)")
  expect_match(src, "This snapshot stopped updating")
  expect_match(src, "These stories carry no readable date")
  expect_match(src, "div\\(banner, do\\.call\\(tagList, items\\)\\)")
})

test_that("no panel vouches for another panel's freshness", {
  skip_if_not(file.exists(app_src), "app.R not present")
  src <- paste(readLines(app_src, warn = FALSE), collapse = "\n")
  # The live-wire fallback used to promise "the nightly news panel below is
  # unaffected". True when written, false the day the source went quiet, and
  # read by exactly the people already wondering whether the page was stale.
  expect_false(grepl("news panel below is unaffected", src, fixed = TRUE))
  expect_match(src, "Market News Feed below carries its own date")
})

test_that("the shipped data is in the state this disclosure exists for", {
  f <- repo_path("data", "market_news.csv")
  skip_if_not(file.exists(f), "market_news.csv not present")
  d <- suppressWarnings(readr::read_csv(f, show_col_types = FALSE))
  skip_if(is.null(d) || !nrow(d) || !("publishedDate" %in% names(d)),
          "no dated news rows")
  age <- as.numeric(Sys.Date() -
           max(as.Date(substr(as.character(d$publishedDate), 1, 10)), na.rm = TRUE))
  # Not an assertion that the feed is broken -- it is an assertion that the
  # banner is reachable. If this feed ever starts answering again the test
  # still passes; it only fails if the age becomes unmeasurable.
  expect_false(is.na(age))
  expect_gte(age, 0)
})

# ── the verifier stopped failing over it, on a condition ────────────────────

test_that("news_grade decides live-or-dead at the boundary it claims", {
  skip_if_not(file.exists(vp_src), "verify_pipeline.R not present")
  src <- readLines(vp_src, warn = FALSE)
  a <- grep("^news_grade <- function", src)
  b <- grep("^news_alt <- local\\(", src)
  expect_length(a, 1)
  expect_length(b, 1)
  expect_lt(a[1], b[1])
  e <- new.env(parent = globalenv())
  eval(parse(text = paste(src[a[1]:(b[1] - 1)], collapse = "\n")), envir = e)
  f <- e$news_grade
  today <- as.Date("2026-11-05")

  expect_equal(f("2026-11-05T00:39:01Z", 3, today)$verdict, "ok")
  expect_equal(f("2026-11-02", 3, today)$verdict, "ok")      # exactly at the cut
  expect_equal(f("2026-11-01", 3, today)$verdict, "dead")    # one day past it
  expect_equal(f("2026-11-02", 3, today)$age, 3)

  # Newest wins, so one fresh item in an old file reads as live -- which is
  # correct: the pull answered.
  expect_equal(f(c("2026-04-27", "2026-11-05"), 3, today)$verdict, "ok")

  # Unreadable is its own verdict, and it is NOT "ok".
  expect_equal(f(character(0), 3, today)$verdict, "unknown")
  expect_equal(f(c(NA, ""), 3, today)$verdict, "unknown")
  expect_true(is.na(f("nope", 3, today)$age))

  whole <- paste(src, collapse = "\n")
  expect_match(whole, "NEWS_ALT_MAX_AGE <- 3")
  expect_match(whole, "news_grade\\(d\\$published_parsed, NEWS_ALT_MAX_AGE, Sys\\.Date\\(\\)\\)")
})

test_that("the news exemption is conditional, not a deletion", {
  skip_if_not(file.exists(vp_src), "verify_pipeline.R not present")
  src <- paste(readLines(vp_src, warn = FALSE), collapse = "\n")

  # The feed is routed by NAME, and the name resolves to a replacement whose
  # liveness is checked. A bare TRUE here would exempt it unconditionally.
  expect_match(src, 'label = "Alpha Vantage news", warn = 7, dark = 14,\\n\\s*routed = "news"')
  expect_match(src, "news_live <- !is\\.null\\(news_alt\\) && identical\\(news_alt\\$verdict, \"ok\"\\)")
  expect_match(src, "news = list\\(\\n\\s*live = news_live")
  expect_match(src, "isTRUE\\(repl\\$live\\)")

  # And the non-routed branch still exists. Narrowing the exempting `if` once
  # already dropped the else that carried every other dark feed into the fatal
  # channel, while the summary table kept printing DARK.
  expect_match(src, "\\} else if \\(identical\\(st, \"DARK\"\\)\\) \\{")
  expect_match(src, "dark <- c\\(dark, sprintf\\(\"%s silent %d days\", fd\\$label, age\\)\\)")

  # Exactly two feeds are routed, and they are routed to different guards.
  expect_equal(length(gregexpr('routed = "', src)[[1]]), 2L)
  expect_match(src, 'routed = "sec"')
  expect_match(src, 'routed = "news"')
})
