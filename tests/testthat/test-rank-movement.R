# Rank movement.
#
# The tables could say what a stock scores but never whether it was climbing or
# falling, even though the pipeline had been committing a daily snapshot since
# 2026-05-05. tools/rebuild_score_history.R recovered those from git; this
# covers the arithmetic that turns them into an arrow.

if (have_pkgs("dplyr", "tibble", "readr")) {
  suppressMessages({ library(dplyr); library(tibble); library(readr) })

  gl <- repo_path("app", "global.R")
  have_helpers <- FALSE
  if (file.exists(gl)) {
    src <- readLines(gl, warn = FALSE)
    from <- grep("^rank_delta <- function", src)
    if (length(from)) {
      eval(parse(text = paste(src[from[1]:length(src)], collapse = "\n")),
           envir = globalenv())
      have_helpers <- exists("rank_delta") && exists("delta_html")
    }
  }

  # Six consecutive TRADING days. The fixture used to be six consecutive
  # calendar days, which quietly included a Saturday and a Sunday — the same
  # confusion between "a day" and "a day the market was open" that the lookback
  # itself got wrong.
  BIZ <- as.Date(c("2026-09-01", "2026-09-02", "2026-09-03",
                   "2026-09-04", "2026-09-07", "2026-09-08"))
  hist <- function() tibble(
    date = rep(BIZ, each = 3),
    symbol = rep(c("AAA", "BBB", "CCC"), 6),
    rank = c(1,2,3,  1,2,3,  2,1,3,  3,1,2,  3,2,1,  3,1,2),
    n_universe = 3L
  )

  test_that("a positive delta means the stock climbed the ranking", {
    skip_if_not(have_helpers, "global.R helpers unavailable")
    # CCC goes from rank 3 on day 1 to rank 2 on the last day: up one place.
    d <- rank_delta(lookback = 5, sh = hist())
    expect_equal(d$delta[d$symbol == "CCC"], 1L)
    # AAA goes 1 -> 3: down two.
    expect_equal(d$delta[d$symbol == "AAA"], -2L)
    # BBB goes 2 -> 1: up one.
    expect_equal(d$delta[d$symbol == "BBB"], 1L)
  })

  # The guard that keeps the column honest. The list grew from 50 names to 195
  # on 2026-08-05; a rank move measured across that is the list growing.
  test_that("comparisons across a universe size change are dropped", {
    skip_if_not(have_helpers, "global.R helpers unavailable")
    h <- hist()
    h$n_universe[h$date == min(h$date)] <- 50L   # different universe at the start
    d <- rank_delta(lookback = 5, sh = h)
    expect_true(is.null(d) || nrow(d) == 0)
  })

  # Found by probing during the audit. Clamping to the oldest date meant a
  # lookback of 5 and a lookback of 99 returned the same delta on a four-day
  # history, while the column header still said "5D".
  test_that("a lookback longer than the history returns nothing, not a shorter one", {
    skip_if_not(have_helpers, "global.R helpers unavailable")
    h <- hist()                                 # six dates
    expect_equal(nrow(rank_delta(lookback = 5,  sh = h)), 3)   # fits
    expect_null(rank_delta(lookback = 6,  sh = h))             # exactly too long
    expect_null(rank_delta(lookback = 99, sh = h))
  })

  test_that("too little history yields nothing rather than a wrong number", {
    skip_if_not(have_helpers, "global.R helpers unavailable")
    one <- hist() %>% filter(date == min(date))
    expect_null(rank_delta(lookback = 5, sh = one))
    expect_null(rank_delta(lookback = 5, sh = NULL))
    expect_null(rank_delta(lookback = 5, sh = hist()[0, ]))
  })

  test_that("a shorter lookback reads a nearer date", {
    skip_if_not(have_helpers, "global.R helpers unavailable")
    # One session back: AAA is rank 3 on both of the last two days -> no move.
    d1 <- rank_delta(lookback = 1, sh = hist())
    expect_equal(d1$delta[d1$symbol == "AAA"], 0L)
  })

  # Found by probing during the audit, against the live file. The history has
  # one row per day the pipeline RAN, not per trading day, so counting rows
  # backwards measured 13 calendar days and labelled it "5D".
  test_that("the lookback is measured in trading days, not rows", {
    skip_if_not(have_helpers, "global.R helpers unavailable")
    # Fri 2026-09-04 .. Thu 2026-09-24, but the pipeline missed several runs.
    ran <- as.Date(c("2026-09-04", "2026-09-08", "2026-09-11",
                     "2026-09-17", "2026-09-22", "2026-09-24"))
    h <- tibble(date = rep(ran, each = 2),
                symbol = rep(c("AAA", "BBB"), 6),
                rank = rep(c(1L, 2L), 6), n_universe = 2L)
    # Five trading days before Thu 09-24 is Wed 09-17 — which is on file.
    # Counting five rows back would have reached 09-04, three weeks earlier.
    d <- rank_delta(lookback = 5, sh = h)
    expect_false(is.null(d))
    expect_equal(nrow(d), 2)
  })

  test_that("a gap wider than the tolerance reports nothing", {
    skip_if_not(have_helpers, "global.R helpers unavailable")
    # Nothing within four days of the five-trading-day mark (Wed 09-17).
    ran <- as.Date(c("2026-09-04", "2026-09-24"))
    h <- tibble(date = rep(ran, each = 2),
                symbol = rep(c("AAA", "BBB"), 2),
                rank = rep(c(1L, 2L), 2), n_universe = 2L)
    expect_null(rank_delta(lookback = 5, sh = h))
  })

  test_that("a weekend is never counted as a trading day", {
    skip_if_not(have_helpers, "global.R helpers unavailable")
    # Mon 2026-09-07 back one trading day is Fri 2026-09-04, not Sun 09-06.
    ran <- as.Date(c("2026-09-04", "2026-09-06", "2026-09-07"))
    h <- tibble(date = rep(ran, each = 2),
                symbol = rep(c("AAA", "BBB"), 3),
                rank = c(1L,2L,  1L,2L,  2L,1L), n_universe = 2L)
    d <- rank_delta(lookback = 1, sh = h)
    expect_equal(d$delta[d$symbol == "AAA"], -1L)   # 1 -> 2 against Friday
  })

  test_that("arrows are coloured by direction and a gap shows a dash", {
    skip_if_not(have_helpers, "global.R helpers unavailable")
    expect_true(grepl("9650", delta_html(7L)))        # up triangle
    expect_true(grepl("00C853", delta_html(7L)))      # green
    expect_true(grepl("9660", delta_html(-7L)))       # down triangle
    expect_true(grepl("FF3D00", delta_html(-7L)))     # red
    expect_true(grepl("ndash", delta_html(NA_integer_)))
    expect_false(grepl("9650|9660", delta_html(0L)))  # flat is not an arrow
  })

  # ── the shipped file ──────────────────────────────────────────────────────
  test_that("score_history.csv has what the tables read", {
    p <- repo_path("data", "score_history.csv")
    skip_if_not(file.exists(p), "score history not generated yet")
    d <- read_csv(p, show_col_types = FALSE)
    expect_true(all(c("date","symbol","master_score","rank","n_universe") %in% names(d)))
    skip_if(nrow(d) == 0, "empty history")
    expect_equal(anyDuplicated(d[c("symbol","date")]), 0)
    # Within a day, rank 1 is the best score and ranks run 1..n
    last <- d %>% filter(date == max(as.Date(date)))
    expect_equal(min(last$rank), 1)
    expect_equal(max(last$rank), nrow(last))
    expect_equal(last$symbol[which.min(last$rank)],
                 last$symbol[which.max(last$master_score)])
  })
}
