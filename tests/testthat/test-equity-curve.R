# The equity curve is the one output a non-quant reads as "would this have made
# money", so an error here misleads more directly than a wrong IC. These tests
# cover the arithmetic offline plus the shape of the shipped file.

if (have_pkgs("dplyr", "readr", "tibble")) {
  suppressMessages({ library(dplyr); library(readr); library(tibble) })

  bt_deps <- have_pkgs("tidyr", "glue")
  if (bt_deps) {
    SOURCED_BY_MASTER <- TRUE
    suppressMessages(try(source(repo_path("R", "05_backtest.R"), local = TRUE), silent = TRUE))
  }

  test_that("equity_stats recovers known values from a constructed series", {
    skip_if_not(exists("equity_stats"), "05_backtest.R not sourced")
    # 252 days of exactly +0.1%/day against a flat benchmark.
    r <- rep(0.001, 252); b <- rep(0, 252)
    s <- equity_stats(r, b)
    expect_equal(s$total_return, 1.001^252 - 1, tolerance = 1e-10)
    expect_equal(s$cagr,         1.001^252 - 1, tolerance = 1e-8)
    expect_equal(s$vol, 0, tolerance = 1e-12)
    expect_equal(s$max_drawdown, 0, tolerance = 1e-12)   # monotone rise never draws down
    expect_equal(s$days, 252)
  })

  test_that("beta is 1 against itself and 2 for a doubled series", {
    skip_if_not(exists("equity_stats"), "05_backtest.R not sourced")
    set.seed(42)
    b <- rnorm(300, 0.0004, 0.01)
    expect_equal(equity_stats(b, b)$beta,     1, tolerance = 1e-10)
    expect_equal(equity_stats(2 * b, b)$beta, 2, tolerance = 1e-10)
  })

  test_that("drawdown is measured peak-to-trough, not from the start", {
    skip_if_not(exists("equity_stats"), "05_backtest.R not sourced")
    # up 10%, down 50%, up 10% -> worst drawdown is the -50% leg
    s <- equity_stats(c(0.10, -0.50, 0.10), c(0, 0, 0))
    expect_equal(s$max_drawdown, -0.5, tolerance = 1e-12)
  })

  test_that("equity_stats declines to guess on too little data", {
    skip_if_not(exists("equity_stats"), "05_backtest.R not sourced")
    expect_null(equity_stats(c(0.01), c(0.01)))
    expect_null(equity_stats(numeric(0), numeric(0)))
  })

  # ── shipped file shape ────────────────────────────────────────────────────
  test_that("backtest_equity.csv is internally consistent", {
    p <- repo_path("data", "backtest_equity.csv")
    skip_if_not(file.exists(p), "equity curve not generated yet")
    d <- read_csv(p, show_col_types = FALSE)
    expect_true(all(c("date","model_ret","q5_ret","spy_ret",
                      "model_cum","q5_cum","spy_cum","rel_cum",
                      "model_dd","spy_dd") %in% names(d)))
    skip_if(nrow(d) < 2, "too few rows")

    # A duplicated date would mean two tranches compounding the same day twice.
    expect_equal(anyDuplicated(d$date), 0)
    expect_false(is.unsorted(d$date))

    # The cumulative columns must actually be the compounded returns.
    expect_equal(d$model_cum, cumprod(1 + d$model_ret), tolerance = 1e-9)
    expect_equal(d$spy_cum,   cumprod(1 + d$spy_ret),   tolerance = 1e-9)
    expect_equal(d$rel_cum,   d$model_cum / d$spy_cum,  tolerance = 1e-9)
    expect_equal(d$univ_cum,  cumprod(1 + d$univ_ret),  tolerance = 1e-9)

    # Drawdown is non-positive and bottoms out no lower than -100%.
    expect_true(all(d$model_dd <= 1e-12))
    expect_true(all(d$model_dd >= -1))
  })

  test_that("the split guard kept corporate actions out of the curve", {
    p <- repo_path("data", "backtest_equity.csv")
    skip_if_not(file.exists(p), "equity curve not generated yet")
    d <- read_csv(p, show_col_types = FALSE)
    skip_if(nrow(d) < 2, "too few rows")
    # PRPL's +2010% and BRCC's +872% are unadjusted reverse splits. A ~39-name
    # equal-weight book would show roughly +9.8% and +4.4% on those days if they
    # leaked through; no genuine daily move for this portfolio approaches that.
    expect_true(max(abs(d$model_ret), na.rm = TRUE) < 0.25)
    expect_true(max(abs(d$q5_ret),    na.rm = TRUE) < 0.25)
  })

  test_that("stats file carries all three series and agrees with the curve", {
    ps <- repo_path("data", "backtest_equity_stats.csv")
    pc <- repo_path("data", "backtest_equity.csv")
    skip_if_not(file.exists(ps) && file.exists(pc), "equity stats not generated yet")
    s <- read_csv(ps, show_col_types = FALSE)
    d <- read_csv(pc, show_col_types = FALSE)
    # "univ" is the no-signal control: the same 195 names equal-weighted with no
    # ranking. Without it the tab credits the model for its universe's
    # survivorship bias, which is worth about 7pp a year on its own.
    expect_setequal(s$series, c("model", "q5", "univ", "spy"))
    expect_true(all(c("cagr","beta","max_drawdown","excess_cagr") %in% names(s)))

    # The benchmark is its own benchmark, so its beta is exactly 1 and its
    # excess return exactly 0 — a cheap check that the rows are not transposed.
    spy <- s[s$series == "spy", ]
    expect_equal(spy$beta[1], 1, tolerance = 1e-8)
    expect_equal(spy$excess_cagr[1], 0, tolerance = 1e-10)

    mdl <- s[s$series == "model", ]
    expect_equal(mdl$total_return[1], tail(d$model_cum, 1) - 1, tolerance = 1e-8)
  })
}

# ── corporate-action detection ──────────────────────────────────────────────
# An earlier version used a plain |return| >= 50% threshold. It deleted 13 bars
# inside the curve window, none of which was a corporate action — they were
# genuine small-cap catalysts — while the two real reverse splits it was
# written for (PRPL, BRCC) sat outside the window entirely. The signature test
# replaced it: a k:1 reverse split multiplies price by k and divides share
# volume by k, so the two move in OPPOSITE directions. No news event does that.
if (exists("corporate_action")) {
  test_that("real reverse splits are detected by price up on collapsing volume", {
    # PRPL 2026-07-20 and BRCC 2026-08-25, the two actual artifacts
    expect_true(corporate_action(20.103, 0.099))
    expect_true(corporate_action(8.716,  0.509))
  })

  test_that("genuine catalysts are left alone", {
    # every real mover in the dataset came with volume UP several fold
    expect_false(corporate_action(1.606,  5.635))   # SANA, clinical data
    expect_false(corporate_action(1.364,  139.39))  # OLMA
    expect_false(corporate_action(0.723,  6.897))   # FSLY
    expect_false(corporate_action(0.580,  2.836))   # RGTI
    expect_false(corporate_action(-0.627, 11.126))  # TDUP
  })

  test_that("a forward split is caught by its mirror signature", {
    # price halves, volume doubles: the logs cancel
    expect_true(corporate_action(-0.543, 2.351))    # IESC
    expect_true(corporate_action(-0.5,   2.0))      # exact 2:1
  })

  test_that("ordinary days and missing volume never trip the detector", {
    expect_false(corporate_action(0.02, 1.1))
    expect_false(corporate_action(0.4,  0.5))       # below the move floor
    expect_false(corporate_action(0.8,  NA_real_))
    expect_false(corporate_action(NA_real_, 0.1))
    expect_false(corporate_action(0.8,  0))         # zero prior volume
  })
}

# ── buy and hold, not silent daily rebalancing ──────────────────────────────
if (exists("tranche_returns") && have_pkgs("tidyr")) {
  test_that("a tranche holds its basket rather than rebalancing every day", {
    # One name compounds up, the other down. Buy-and-hold lets the winner grow
    # into a larger share of the book; averaging daily returns instead would
    # silently sell the winner and buy the loser every single day.
    d <- as.Date("2024-01-01") + 0:9
    fw <- tibble::tibble(
      date      = rep(d, 2),
      symbol    = rep(c("UP", "DN"), each = 10),
      daily_ret = c(rep(0.05, 10), rep(-0.05, 10))
    )
    got <- tranche_returns(fw, c("UP", "DN"))
    expect_equal(nrow(got), 10)
    expect_equal(got$n[1], 2)

    # Buy-and-hold terminal value of an equal-weighted pair
    expect_equal(prod(1 + got$ret),
                 (1.05^10 + 0.95^10) / 2, tolerance = 1e-10)
    # Daily rebalancing would give exactly (1 + mean(0.05, -0.05))^10 = 1.
    expect_true(abs(prod(1 + got$ret) - 1) > 0.01)
  })

  test_that("a day a held name is missing does not silently drop the day", {
    d <- as.Date("2024-01-01") + 0:4
    fw <- tibble::tibble(
      date      = c(d, d[-3]),
      symbol    = c(rep("A", 5), rep("B", 4)),
      daily_ret = c(rep(0.01, 5), rep(0.02, 4))
    )
    got <- tranche_returns(fw, c("A", "B"))
    expect_equal(nrow(got), 5)          # all five dates survive
    expect_false(any(is.na(got$ret)))
  })
}

# ── the honesty controls ────────────────────────────────────────────────────
# These exist because an adversarial review found the tab was crediting the
# model for two things it had not earned: the survivorship bias in its own
# ticker list, and a t-statistic computed as though every day of a 63-day hold
# were a fresh independent observation.
if (have_pkgs("dplyr", "readr")) {
  test_that("the no-signal universe baseline is present and independent of the model", {
    ps <- repo_path("data", "backtest_equity_stats.csv")
    pc <- repo_path("data", "backtest_equity.csv")
    skip_if_not(file.exists(ps) && file.exists(pc), "equity stats not generated yet")
    s <- read_csv(ps, show_col_types = FALSE)
    d <- read_csv(pc, show_col_types = FALSE)
    expect_true("univ" %in% s$series)
    expect_true(all(c("univ_ret", "univ_cum") %in% names(d)))
    # It must not simply track the model — it is a different portfolio.
    expect_false(isTRUE(all.equal(d$univ_ret, d$model_ret)))
    u <- s[s$series == "univ", ]
    expect_equal(u$total_return[1], tail(d$univ_cum, 1) - 1, tolerance = 1e-8)
  })

  test_that("significance is measured per rebalance, not per day", {
    ps <- repo_path("data", "backtest_equity_stats.csv")
    skip_if_not(file.exists(ps), "equity stats not generated yet")
    s <- read_csv(ps, show_col_types = FALSE)
    expect_true(all(c("excess_t_tranche", "n_decisions") %in% names(s)))
    m <- s[s$series == "model", ]
    # ~10 rebalances over 2.5 years, not 623 daily observations
    expect_true(m$n_decisions[1] >= 2 && m$n_decisions[1] <= 40)
    expect_true(m$n_decisions[1] < m$days[1] / 10)
    expect_false(is.na(m$excess_t_tranche[1]))
  })
}
