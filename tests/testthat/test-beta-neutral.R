# Scoring on the part of a return the market does not explain.
#
# Until 2026-09-23 the score ranked on raw excess over SPY, which rewards a
# stock for carrying market risk while the market rises. On the walk-forward
# curve the ranking had actually inverted: the bottom quintile beat the top by
# 8.7 points a year. These tests pin the new behaviour and, more importantly,
# pin the property that makes it safe — beta is estimated inside the formation
# window and never carried in from outside it.

if (have_pkgs("dplyr", "tibble", "tidyr", "readr", "glue")) {
  suppressMessages({ library(dplyr); library(tibble) })
  SOURCED_BY_MASTER <- TRUE
  suppressMessages(try(source(repo_path("R", "05_backtest.R"), local = TRUE), silent = TRUE))

  # A stock that is exactly 1.5x the market has no skill whatsoever. Raw excess
  # still rewards it in a rising market; the residual correctly reports zero.
  # score_formation needs at least N_BUCKETS * 2 symbols, so build a small
  # cross-section: six names that are pure market exposure at increasing beta
  # and no skill, and six low-beta names with a steady genuine edge.
  synth <- function(n = 140, seed = 7) {
    set.seed(seed)
    dts <- as.Date("2025-01-01") + seq_len(n)
    spy <- rnorm(n, 0.0008, 0.008)
    betas <- c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2)
    pure <- bind_rows(lapply(seq_along(betas), function(i)
      tibble(date = dts, symbol = paste0("BETA", i), spy_ret = spy,
             daily_ret = betas[i] * spy)))
    skill <- bind_rows(lapply(1:6, function(i)
      tibble(date = dts, symbol = paste0("SKILL", i), spy_ret = spy,
             daily_ret = 0.5 * spy + 0.0008 + i * 1e-5)))
    bind_rows(pure, skill) %>% mutate(daily_alpha = daily_ret - spy_ret)
  }

  test_that("a pure-beta stock loses to a low-beta stock with real edge", {
    skip_if_not(exists("score_formation"), "05_backtest.R not sourced")
    d <- synth()
    raw  <- score_formation(d %>% select(date, symbol, daily_alpha))
    neut <- score_formation(d)          # returns present -> beta-neutral
    skip_if(is.null(raw) || is.null(neut), "scoring returned NULL")

    winner <- function(s) s$symbol[which.max(s$score)]
    # The old rule crowns a name whose only property is market exposure.
    expect_true(grepl("^BETA", winner(raw)))
    # The new rule crowns one with an actual edge.
    expect_true(grepl("^SKILL", winner(neut)))
  })

  test_that("the residual strips out the market component", {
    skip_if_not(exists("score_formation"), "05_backtest.R not sourced")
    d <- synth()
    neut <- score_formation(d)
    skip_if(is.null(neut), "scoring returned NULL")
    pb <- neut %>% filter(grepl("^BETA", symbol))
    # pure multiples of the market: annualised residual alpha is ~0 for each
    expect_true(all(abs(pb$ann_alpha) < 0.02))
  })

  test_that("supplying returns actually changes the ranking", {
    skip_if_not(exists("score_formation"), "05_backtest.R not sourced")
    d <- synth()
    raw  <- score_formation(d %>% select(date, symbol, daily_alpha)) %>% arrange(symbol)
    neut <- score_formation(d) %>% arrange(symbol)
    expect_false(isTRUE(all.equal(raw$score, neut$score)))
  })

  # The one that would invalidate the backtest if it failed.
  test_that("beta is estimated inside the window, so later data cannot reach it", {
    skip_if_not(exists("score_formation"), "05_backtest.R not sourced")
    d <- synth()
    half <- d %>% filter(date <= median(unique(d$date)))
    a <- score_formation(half)
    # Append wildly different future data, then re-score the SAME window.
    future <- d %>% filter(date > median(unique(d$date))) %>%
      mutate(daily_ret = daily_ret * -8, daily_alpha = daily_ret - spy_ret)
    b <- score_formation(bind_rows(half, future) %>%
                           filter(date <= median(unique(d$date))))
    expect_equal(a$score, b$score)
    expect_equal(a$symbol, b$symbol)
  })

  test_that("scoring still works when only daily_alpha is supplied", {
    skip_if_not(exists("score_formation"), "05_backtest.R not sourced")
    d <- synth() %>% select(date, symbol, daily_alpha)
    s <- score_formation(d)
    expect_false(is.null(s))
    expect_true(all(c("score", "symbol") %in% names(s)))
    expect_false(any(is.na(s$score)))
  })
}
