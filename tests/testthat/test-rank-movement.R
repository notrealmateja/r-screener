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

  hist <- function() tibble(
    date = rep(as.Date("2026-09-01") + 0:5, each = 3),
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
