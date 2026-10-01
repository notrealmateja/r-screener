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

  # ...but surviving is not the same as being right. The 0-fill is a HOLD, not
  # an exclusion: a missing cell freezes that position at its last value, so a
  # day where most of the book has no price reads as flat rather than as
  # unknown. That is why run_equity_curve trims thin trailing days before they
  # ever reach this function.
  test_that("a missing cell freezes the position rather than excluding it", {
    skip_if_not(exists("tranche_returns"), "05_backtest.R not sourced")
    d <- as.Date("2024-01-01") + 0:1
    fw <- tibble::tibble(
      date      = c(d, d[1]),           # B has no row on day 2
      symbol    = c("A", "A", "B"),
      daily_ret = c(0.00, 0.10, 0.00)
    )
    got <- tranche_returns(fw, c("A", "B"))
    # A gained 10% and B was frozen, so the book reads +5% — diluted, not +10%.
    expect_equal(got$ret[2], 0.05, tolerance = 1e-12)
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

  # The curve used to end on 2026-09-22, a day carrying 35 of 194 symbols
  # because the pull was still landing. Every absent name was marked flat.
  test_that("the curve does not end on a day most of the universe is missing", {
    pp <- repo_path("data", "price_history.csv")
    ps <- repo_path("data", "backtest_equity_stats.csv")
    skip_if_not(file.exists(pp) && file.exists(ps), "not generated yet")
    px <- read_csv(pp, col_select = c(symbol, date, daily_ret),
                   show_col_types = FALSE) %>% filter(!is.na(daily_ret))
    cov <- px %>% count(date, name = "n_sym")
    ref <- stats::median(cov$n_sym)
    end <- as.Date(read_csv(ps, show_col_types = FALSE)$end_date[1])
    n_end <- cov$n_sym[cov$date == end]
    skip_if(length(n_end) != 1, "end date not present in price history")
    expect_gte(n_end, 0.80 * ref)
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

# ── the overlapping composite ───────────────────────────────────────────────
# A single rebalance calendar has to start somewhere, and because the price
# window rolls, one week of new data redrew every basket boundary: the bottom
# quintile went 25.3% -> 39.3%/yr and the top-minus-bottom spread flipped from
# +10.1pp to -5.4pp. Sweeping all 63 start days put that spread between -10.3pp
# and +41.6pp. The curve now holds every calendar at once, and the sweep is
# published so the sensitivity is visible rather than drawn from blindly.
if (have_pkgs("dplyr", "readr")) {
  suppressMessages({ library(dplyr); library(readr) })

  test_that("the calendar sweep is published and complete", {
    p <- repo_path("data", "backtest_phase_sweep.csv")
    skip_if_not(file.exists(p), "phase sweep not generated yet")
    d <- read_csv(p, show_col_types = FALSE)
    expect_true(all(c("phase","n_tranches","days","top","bottom","univ","spy",
                      "spread","edge_vs_univ","excess") %in% names(d)))
    expect_gt(nrow(d), 1)
    expect_equal(anyDuplicated(d$phase), 0)
    # Each calendar must cover the SAME dates, or the spread measures different
    # windows as much as different calendars.
    expect_equal(length(unique(d$days)), 1)
    expect_equal(d$spread, d$top - d$bottom, tolerance = 1e-12)
  })

  test_that("the headline is the average of the calendars, not one of them", {
    ps <- repo_path("data", "backtest_equity_stats.csv")
    pw <- repo_path("data", "backtest_phase_sweep.csv")
    skip_if_not(file.exists(ps) && file.exists(pw), "not generated yet")
    s <- read_csv(ps, show_col_types = FALSE)
    w <- read_csv(pw, show_col_types = FALSE)
    skip_if(nrow(w) < 2, "too few calendars")
    for (ser in c("model", "q5", "univ")) {
      col <- c(model = "top", q5 = "bottom", univ = "univ")[[ser]]
      got <- s$cagr[s$series == ser]
      # The composite averages daily returns across calendars, so it is not
      # identical to the mean of the per-calendar CAGRs — but it must sit
      # inside their range, and close to their centre.
      expect_gte(got, min(w[[col]]), label = paste(ser, "below every calendar"))
      expect_lte(got, max(w[[col]]), label = paste(ser, "above every calendar"))
      expect_equal(got, mean(w[[col]]), tolerance = 0.03,
                   label = paste(ser, "far from the calendar mean"))
    }
  })

  test_that("every date in the curve has all calendars live", {
    p <- repo_path("data", "backtest_equity.csv")
    pw <- repo_path("data", "backtest_phase_sweep.csv")
    skip_if_not(file.exists(p) && file.exists(pw), "not generated yet")
    d <- read_csv(p, show_col_types = FALSE)
    skip_if(!"n_books" %in% names(d), "pre-composite curve")
    w <- read_csv(pw, show_col_types = FALSE)
    # A date averaging fewer books is noisier by construction and would make
    # the start of the curve look more volatile than the strategy is.
    expect_equal(unique(d$n_books), nrow(w))
  })

  test_that("averaging calendars does not inflate the independent sample", {
    ps <- repo_path("data", "backtest_equity_stats.csv")
    pc <- repo_path("data", "backtest_equity.csv")
    skip_if_not(file.exists(ps) && file.exists(pc), "not generated yet")
    s <- read_csv(ps, show_col_types = FALSE)
    d <- read_csv(pc, show_col_types = FALSE)
    skip_if(!"block" %in% names(d), "pre-composite curve")
    n_dec <- s$n_decisions[s$series == "model"]
    # 63 overlapping calendars are not 63x the evidence: on any day 62/63 of
    # the book is yesterday's. The independent unit is still one holding
    # period, so n_decisions counts whole 63-day blocks, not days or calendars.
    whole <- d %>% count(block) %>% filter(n == 63) %>% nrow()
    expect_equal(n_dec, whole)
    expect_lt(n_dec, nrow(d) / 50)        # nowhere near one per day
    n_cal <- nrow(read_csv(repo_path("data", "backtest_phase_sweep.csv"),
                           show_col_types = FALSE))
    expect_lt(n_dec, n_cal)               # nor one per calendar
  })

  test_that("the calendar spread is reported, and not as a confidence interval", {
    ps <- repo_path("data", "backtest_equity_stats.csv")
    pw <- repo_path("data", "backtest_phase_sweep.csv")
    skip_if_not(file.exists(ps) && file.exists(pw), "not generated yet")
    s <- read_csv(ps, show_col_types = FALSE); w <- read_csv(pw, show_col_types = FALSE)
    skip_if(!"phase_spread_min" %in% names(s), "pre-composite stats")
    expect_equal(s$phase_model_cagr_min[1], min(w$top), tolerance = 1e-12)
    expect_equal(s$phase_model_cagr_max[1], max(w$top), tolerance = 1e-12)
    expect_equal(s$phase_spread_med[1], median(w$spread), tolerance = 1e-12)
    expect_equal(s$phase_top_beats_bottom[1], mean(w$spread > 0), tolerance = 1e-12)
    expect_equal(s$phases[1], nrow(w))
  })
}
