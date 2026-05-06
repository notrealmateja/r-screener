# =============================================================================
# 04_master_score.R
# EdgeScreener — Quantitative Scoring Engine
#
# Architecture:
#   1. Z-score normalize all metrics across full universe
#   2. Detect market regime from FRED macro data
#   3. Apply dynamic factor weights by regime
#   4. Compute composite percentile score
#   5. Backtest confidence: historical hit rate for 3%+ gain
#   6. Apply Sweet Spot filter for Top 15
#   7. Apply Unicorn filter (mktcap < $5B)
#   8. Assign Simple ratings: Strong Buy / Buy / Hold / Sell
# =============================================================================

run_module4 <- function(fund_data = NULL) {
  message("\n=== MODULE 4: QUANTITATIVE SCORING ENGINE ===\n")

  library(dplyr)
  library(readr)
  library(tidyr)

  # ── LOAD DATA ─────────────────────────────────────────────────────────────────
  if (is.null(fund_data)) {
    path <- if (file.exists("data/fundamentals_scored.csv"))
      "data/fundamentals_scored.csv" else "app/fundamentals_scored.csv"
    if (!file.exists(path)) stop("Run 01_fundamentals.R first.")
    fund_data <- read_csv(path, show_col_types = FALSE)
  }

  # Load price series cache for backtesting
  price_cache_path <- "data/price_series_cache.rds"
  price_cache <- if (file.exists(price_cache_path)) readRDS(price_cache_path) else list()

  df <- fund_data
  n  <- nrow(df)
  message("Scoring ", n, " stocks...\n")

  # ── STEP 1: Z-SCORE NORMALIZATION ─────────────────────────────────────────────
  # Normalize each metric against the full universe
  # z = (x - mean(x)) / sd(x), then clip to [-3, 3] and rescale to [0, 100]
  message("[Step 1] Z-score normalizing all metrics...")

  z_norm <- function(x, higher_is_better = TRUE) {
    x <- suppressWarnings(as.numeric(x))
    valid <- x[!is.na(x) & is.finite(x)]
    if (length(valid) < 3 || all(is.na(x))) return(rep(50, length(x)))
    mu  <- mean(valid)
    sig <- sd(valid)
    if (sig == 0) return(rep(50, length(x)))
    z <- (x - mu) / sig
    z <- pmax(pmin(z, 3), -3)  # clip extremes
    if (!higher_is_better) z <- -z
    # Rescale from [-3,3] to [0,100]
    score <- (z + 3) / 6 * 100
    score <- (z + 3) / 6 * 100; round(ifelse(is.na(x) | !is.finite(x) | length(score)==0, 50, score), 1)
  }

  # Value metrics (lower P/E is better, but negative P/E is bad)
  df$z_pe    <- z_norm(ifelse(df$pe_ratio <= 0 | df$pe_ratio > 200, NA, df$pe_ratio), FALSE)
  df$z_pb    <- z_norm(ifelse(df$pb_ratio <= 0 | df$pb_ratio > 50,  NA, df$pb_ratio), FALSE)
  df$z_ps    <- z_norm(ifelse(df$ps_ratio <= 0 | df$ps_ratio > 30,  NA, df$ps_ratio), FALSE)
  df$z_peg   <- z_norm(ifelse(df$peg_ratio<= 0 | df$peg_ratio > 10, NA, df$peg_ratio), FALSE)
  df$z_dcf   <- z_norm(df$dcf_upside, TRUE)  # higher DCF upside is better

  # Quality metrics (higher is better)
  df$z_roe     <- z_norm(df$roe,              TRUE)
  df$z_roa     <- z_norm(df$roa,              TRUE)
  df$z_roic    <- z_norm(df$roic,             TRUE)
  df$z_margin  <- z_norm(df$profit_margin,    TRUE)
  df$z_gmargin <- z_norm(df$gross_margin,     TRUE)
  df$z_omargin <- z_norm(df$operating_margin, TRUE)
  df$z_cr      <- z_norm(df$current_ratio,    TRUE)
  df$z_ic      <- z_norm(df$interest_coverage,TRUE)
  df$z_de      <- z_norm(df$debt_equity,      FALSE)  # lower debt is better

  # Growth metrics (higher is better)
  df$z_rev_growth  <- z_norm(df$revenue_growth,   TRUE)
  df$z_earn_growth <- z_norm(df$earnings_growth,  TRUE)
  df$z_fcf_growth  <- z_norm(df$fcf_growth,       TRUE)
  df$z_eps_growth  <- z_norm(df$eps_growth,        TRUE)
  df$z_eps_surp    <- z_norm(df$eps_surprise_pct, TRUE)
  df$z_beat_count  <- z_norm(df$earnings_beat_count, TRUE)

  # Momentum metrics
  df$z_ret1d  <- z_norm(df$ret_1d,  TRUE)
  df$z_ret5d  <- z_norm(df$ret_5d,  TRUE)
  df$z_ret1m  <- z_norm(df$ret_1m,  TRUE)
  df$z_ret3m  <- z_norm(df$ret_3m,  TRUE)
  df$z_ret6m  <- z_norm(df$ret_6m,  TRUE)
  df$z_ret1y  <- z_norm(df$ret_1y,  TRUE)
  df$z_rsi    <- z_norm(ifelse(df$rsi14 > 70, NA, df$rsi14), TRUE)  # overbought penalty
  df$z_adx    <- z_norm(df$adx,     TRUE)   # trend strength
  df$z_trend  <- z_norm(df$trend_strength, TRUE)

  # Analyst / sentiment
  df$z_analyst <- z_norm(df$analyst_score, TRUE)
  df$z_news    <- rep(50, nrow(df))  # news_score not available, default to neutral

  # Safety metrics
  df$z_beta      <- z_norm(abs(df$beta - 1), FALSE)  # closer to 1 is baseline
  df$z_short     <- z_norm(df$short_pct,     FALSE)   # lower short interest is better
  df$z_vol_ratio <- z_norm(df$vol_20d / pmax(df$vol_252d, 0.01), FALSE) # low recent vol vs historical

  # ── STEP 2: COMPOSITE FACTOR SCORES ──────────────────────────────────────────
  message("[Step 2] Computing composite factor scores...")

  df <- df %>% mutate(
    # VALUE SCORE: penalize both overvalued AND deeply negative
    value_score = round(
      z_pe    * 0.30 +
      z_pb    * 0.20 +
      z_ps    * 0.15 +
      z_peg   * 0.20 +
      z_dcf   * 0.15,
      1),

    # QUALITY SCORE: profitability + balance sheet strength
    quality_score = round(
      z_roe     * 0.25 +
      z_roa     * 0.15 +
      z_roic    * 0.15 +
      z_margin  * 0.20 +
      z_gmargin * 0.10 +
      z_omargin * 0.05 +
      z_de      * 0.05 +
      z_cr      * 0.05,
      1),

    # GROWTH SCORE: revenue + earnings + EPS surprise consistency
    growth_score = round(
      z_rev_growth  * 0.30 +
      z_earn_growth * 0.25 +
      z_fcf_growth  * 0.15 +
      z_eps_growth  * 0.15 +
      z_eps_surp    * 0.10 +
      z_beat_count  * 0.05,
      1),

    # MOMENTUM SCORE: multi-timeframe price action + technicals
    momentum_score = round(
      z_ret1d  * 0.15 +
      z_ret5d  * 0.20 +
      z_ret1m  * 0.25 +
      z_ret3m  * 0.20 +
      z_ret6m  * 0.10 +
      z_ret1y  * 0.05 +
      z_rsi    * 0.03 +
      z_adx    * 0.02,
      1),

    # SAFETY SCORE: low volatility, low beta deviation, low short interest
    safety_score = round(
      z_beta      * 0.40 +
      z_short     * 0.30 +
      z_vol_ratio * 0.30,
      1),

    # ANALYST SCORE
    analyst_score = round(
      z_analyst * 0.70 +
      z_news    * 0.30,
      1)
  )

  # ── STEP 3: REGIME DETECTION & DYNAMIC WEIGHTS ───────────────────────────────
  message("[Step 3] Detecting market regime and applying dynamic weights...")

  regime <- if (!is.na(df$regime[1])) df$regime[1] else "NEUTRAL"
  message("  Current regime: ", regime)

  # Dynamic weights shift based on macro environment
  weights <- switch(regime,
    "RISK_OFF"  = list(value=0.30, quality=0.30, growth=0.10, momentum=0.10, safety=0.15, analyst=0.05),
    "INVERTED"  = list(value=0.28, quality=0.28, growth=0.12, momentum=0.12, safety=0.15, analyst=0.05),
    "HIGH_RATE" = list(value=0.30, quality=0.25, growth=0.15, momentum=0.15, safety=0.10, analyst=0.05),
    "LOW_RATE"  = list(value=0.15, quality=0.20, growth=0.30, momentum=0.25, safety=0.05, analyst=0.05),
    "NEUTRAL"   = list(value=0.20, quality=0.22, growth=0.25, momentum=0.20, safety=0.08, analyst=0.05),
    # Fallback for any unexpected regime value
               list(value=0.20, quality=0.22, growth=0.25, momentum=0.20, safety=0.08, analyst=0.05)
  )

  message("  Weights — Value:", weights$value, " Quality:", weights$quality,
          " Growth:", weights$growth, " Momentum:", weights$momentum,
          " Safety:", weights$safety, " Analyst:", weights$analyst)

  # ── STEP 4: MASTER SCORE + PERCENTILE RANK ───────────────────────────────────
  message("[Step 4] Computing master scores and percentile ranks...")

  df <- df %>% mutate(
    # Raw master score (weighted composite)
    master_score_raw = round(
      value_score    * weights$value    +
      quality_score  * weights$quality  +
      growth_score   * weights$growth   +
      momentum_score * weights$momentum +
      safety_score   * weights$safety   +
      analyst_score  * weights$analyst,
      2),

    # Confidence penalty: reduce score for stocks with sparse data
    confidence_penalty = round((100 - data_confidence) * 0.10, 2),
    master_score = round(pmax(master_score_raw - confidence_penalty, 0), 1),

    # Percentile rank among all stocks in universe
    master_percentile = round(rank(master_score) / n() * 100, 1),
    value_percentile    = round(rank(value_score)    / n() * 100, 1),
    quality_percentile  = round(rank(quality_score)  / n() * 100, 1),
    growth_percentile   = round(rank(growth_score)   / n() * 100, 1),
    momentum_percentile = round(rank(momentum_score) / n() * 100, 1),

    # Simple rating based on master percentile
    rating = case_when(
      master_percentile >= 80 ~ "Strong Buy",
      master_percentile >= 60 ~ "Buy",
      master_percentile >= 35 ~ "Hold",
      TRUE                    ~ "Sell"
    ),

    # Primary driver (which factor is strongest for this stock)
    primary_driver = case_when(
      growth_score   == pmax(growth_score, momentum_score, value_score, quality_score) ~ "Growth",
      momentum_score == pmax(growth_score, momentum_score, value_score, quality_score) ~ "Momentum",
      quality_score  == pmax(growth_score, momentum_score, value_score, quality_score) ~ "Quality",
      TRUE                                                                              ~ "Value"
    )
  )

  # ── STEP 5: BACKTESTED CONFIDENCE ENGINE ─────────────────────────────────────
  message("[Step 5] Running backtested confidence engine (252-day lookback)...")
  message("  Calculating historical hit rate for 3%+ daily gain when signals match...")

  calc_backtest_confidence <- function(sym, yah_data, df_row) {
    tryCatch({
      # Get price series
      px_series <- if (!is.null(price_cache[[sym]])) price_cache[[sym]]$px_series else NULL
      if (is.null(px_series) || length(px_series) < 63) return(list(
        confidence_1d = NA, confidence_5d = NA,
        expected_return_1d = NA, signal_matches = NA
      ))

      n_px <- length(px_series)

      # Define signal conditions that must match today's conditions
      # We look back 252 days and find all days where similar signals fired
      daily_rets <- diff(px_series) / head(px_series, -1) * 100

      # Today's signal state
      today_rsi      <- df_row$rsi14
      today_above50  <- df_row$above_ma50
      today_above200 <- df_row$above_ma200
      today_bb_pct   <- df_row$bb_pct
      today_adx      <- df_row$adx
      today_trend    <- df_row$trend_strength
      today_mom      <- df_row$momentum_score

      # Scan each historical day (rolling window, at least 21 days lookback)
      match_days   <- c()
      returns_next1d <- c()
      returns_next5d <- c()

      window <- min(252, n_px - 6)
      for (d in 21:window) {
        # Historical RSI (approximate using past returns)
        past_px  <- px_series[1:d]
        past_ret <- diff(past_px) / head(past_px, -1) * 100

        # Compute rolling RSI at day d (14-period)
        if (length(past_ret) < 14) next
        gains  <- pmax(tail(past_ret, 14), 0)
        losses <- abs(pmin(tail(past_ret, 14), 0))
        avg_g  <- ifelse(any(gains  > 0), mean(gains[gains   > 0], na.rm=TRUE), 0)
        avg_l  <- ifelse(any(losses > 0), mean(losses[losses > 0], na.rm=TRUE), NA)
        if (is.na(avg_l) || avg_l == 0) next
        rs       <- avg_g / avg_l
        hist_rsi <- 100 - (100 / (1 + rs))

        # Historical trend strength (% positive days in last 20)
        hist_trend <- mean(tail(past_ret, 20) > 0, na.rm=TRUE) * 100

        # Historical MA positions
        hist_sma50  <- if (d >= 50)  mean(tail(past_px, 50))  else NA
        hist_sma200 <- if (d >= 200) mean(tail(past_px, 200)) else NA
        hist_above50  <- !is.na(hist_sma50)  && past_px[d] > hist_sma50
        hist_above200 <- !is.na(hist_sma200) && past_px[d] > hist_sma200

        # Signal match criteria (fuzzy matching — within 15% of today's values)
        rsi_match    <- !is.na(today_rsi)   && !is.na(hist_rsi)   &&
                        abs(hist_rsi - today_rsi) < 15
        trend_match  <- !is.na(today_trend) && !is.na(hist_trend) &&
                        abs(hist_trend - today_trend) < 20
        ma50_match   <- is.na(today_above50)  || hist_above50  == today_above50
        ma200_match  <- is.na(today_above200) || hist_above200 == today_above200

        if (rsi_match && trend_match && ma50_match && ma200_match) {
          match_days <- c(match_days, d)
          # Next day return
          if (d + 1 <= length(daily_rets)) {
            returns_next1d <- c(returns_next1d, daily_rets[d+1])
          }
          # Next 5-day cumulative return
          if (d + 5 <= length(daily_rets)) {
            cum5 <- prod(1 + daily_rets[(d+1):(d+5)]/100) - 1
            returns_next5d <- c(returns_next5d, cum5 * 100)
          }
        }
      }

      if (length(returns_next1d) < 5) {
        return(list(confidence_1d=NA, confidence_5d=NA,
                    expected_return_1d=NA, signal_matches=length(match_days)))
      }

      # Hit rate: % of matching days where next-day return >= 3%
      conf_1d <- round(mean(returns_next1d >= 3, na.rm=TRUE) * 100, 1)
      # Weekly hit rate: % of matching days where 5-day return >= 3%
      conf_5d <- if (length(returns_next5d) >= 5)
        round(mean(returns_next5d >= 3, na.rm=TRUE) * 100, 1) else NA
      # Expected return = mean of next-day returns on matching days
      exp_ret <- round(mean(returns_next1d, na.rm=TRUE), 2)

      list(confidence_1d    = conf_1d,
           confidence_5d    = conf_5d,
           expected_return_1d = exp_ret,
           signal_matches   = length(match_days))
    }, error=function(e) {
      list(confidence_1d=NA, confidence_5d=NA,
           expected_return_1d=NA, signal_matches=NA)
    })
  }

  # Run backtest for all stocks
  message("  Running backtest (this may take a few minutes)...")
  bt_results <- lapply(seq_len(nrow(df)), function(i) {
    sym <- df$symbol[i]
    if (i %% 10 == 0) message("  Backtesting: ", i, "/", nrow(df), " — ", sym)
    calc_backtest_confidence(sym, price_cache[[sym]], df[i,])
  })

  df$confidence_1d      <- sapply(bt_results, function(x) x$confidence_1d)
  df$confidence_5d      <- sapply(bt_results, function(x) x$confidence_5d)
  df$expected_return_1d <- sapply(bt_results, function(x) x$expected_return_1d)
  df$signal_matches     <- sapply(bt_results, function(x) x$signal_matches)

  message("  Backtest complete.")
  message("  Stocks with confidence_1d >= 75%: ",
          sum(df$confidence_1d >= 75, na.rm=TRUE))
  message("  Stocks with confidence_5d >= 75%: ",
          sum(df$confidence_5d >= 75, na.rm=TRUE))

  # ── STEP 6: SWEET SPOT SCORE (Top 15) ────────────────────────────────────────
  message("\n[Step 6] Computing Sweet Spot scores...")
  # Goal: highest predicted return today, not over/underpriced, not "obvious"
  # Must have sufficient backtest signal matches to be included

  df <- df %>% mutate(
    # Valuation check: is stock reasonably priced relative to its peers?
    # Use percentile — must be between 25th and 90th percentile value score
    # (too cheap = distressed, too expensive = overvalued)
    valuation_ok = value_percentile >= 20 & value_percentile <= 88,

    # Popularity penalty: mega caps everyone already knows about
    popularity_penalty = case_when(
      !is.na(market_cap) & market_cap > 1e12  ~ 10,  # >$1T market cap
      !is.na(market_cap) & market_cap > 500e9 ~ 5,   # >$500B
      TRUE                                    ~ 0
    ),

    # Momentum already high penalty (avoid chasing tops)
    chase_penalty = case_when(
      !is.na(ret_1m) & ret_1m > 30 ~ 8,
      !is.na(ret_1m) & ret_1m > 20 ~ 4,
      TRUE                         ~ 0
    ),

    # Sweet Spot Score
    sweet_spot_score = round(
      momentum_score   * 0.30 +
      growth_score     * 0.25 +
      value_score      * 0.25 +
      quality_score    * 0.15 +
      analyst_score    * 0.05 -
      popularity_penalty -
      chase_penalty,
      1),

    # Sweet Spot confidence: combine backtest confidence with data confidence
    sweet_spot_confidence = round(
      coalesce(confidence_1d, 50) * 0.70 +
      data_confidence             * 0.30,
      1),

    # Minimum signal matches required for Top 15 eligibility
    top15_eligible = valuation_ok &
                     !is.na(signal_matches) & signal_matches >= 3 &
                     !is.na(confidence_1d)  & confidence_1d >= 30 &
                     !is.na(master_score),

    # Sweet spot rank (among eligible stocks only)
    sweet_spot_rank = NA_real_
  )

  # Rank eligible stocks by sweet spot score
  eligible_idx <- which(df$top15_eligible)
  if (length(eligible_idx) > 0) {
    ranks <- rank(-df$sweet_spot_score[eligible_idx])
    df$sweet_spot_rank[eligible_idx] <- ranks
  }

  top20 <- df %>%
    filter(top15_eligible) %>%
    arrange(sweet_spot_rank) %>%
    head(20)

  message("  Top 20 Sweet Spot stocks identified: ", nrow(top20))
  if (nrow(top20) > 0) {
    message("  #1: ", top20$symbol[1],
            " | Sweet Score: ", top20$sweet_spot_score[1],
            " | Confidence: ", top20$sweet_spot_confidence[1], "%",
            " | Expected: +", top20$expected_return_1d[1], "%")
  }

  # ── STEP 7: UNICORN SCORE (Top 15 Unicorns) ──────────────────────────────────
  message("\n[Step 7] Computing Unicorn scores...")
  # Criteria: mktcap < $5B, rev growth > 15%, positive 3m momentum

  df <- df %>% mutate(
    # Unicorn eligibility
    is_unicorn_eligible = !is.na(market_cap)     & market_cap < 5e9      &
                          !is.na(revenue_growth)  & revenue_growth > 0.15 &
                          !is.na(ret_3m)          & ret_3m > 0            &
                          growth_percentile > 50,

    # Unicorn Score: growth-first, momentum-second
    unicorn_score = round(
      growth_score   * 0.40 +
      momentum_score * 0.35 +
      quality_score  * 0.25,
      1),

    # Weekly confidence for unicorns
    unicorn_confidence = round(
      coalesce(confidence_5d, 50) * 0.70 +
      data_confidence             * 0.30,
      1),

    unicorn_rank = NA_real_
  )

  unicorn_idx <- which(df$is_unicorn_eligible)
  if (length(unicorn_idx) > 0) {
    u_ranks <- rank(-df$unicorn_score[unicorn_idx])
    df$unicorn_rank[unicorn_idx] <- u_ranks
  }

  top15_unicorns <- df %>%
    filter(is_unicorn_eligible) %>%
    arrange(unicorn_rank) %>%
    head(15)

  message("  Unicorn candidates found: ", sum(df$is_unicorn_eligible, na.rm=TRUE))
  message("  Top 15 Unicorns identified: ", nrow(top15_unicorns))
  if (nrow(top15_unicorns) > 0) {
    message("  #1 Unicorn: ", top15_unicorns$symbol[1],
            " | Unicorn Score: ", top15_unicorns$unicorn_score[1],
            " | Market Cap: $", round(top15_unicorns$market_cap[1]/1e9, 1), "B",
            " | Rev Growth: ", round(top15_unicorns$revenue_growth[1]*100, 1), "%")
  }

  # ── STEP 8: FINAL ASSEMBLY ────────────────────────────────────────────────────
  message("\n[Step 8] Final assembly and saving...")

  # Add tier column for display
  df <- df %>% mutate(
    tier = case_when(
      !is.na(sweet_spot_rank) & sweet_spot_rank <= 15 ~ "Top 15",
      !is.na(unicorn_rank)    & unicorn_rank    <= 15 ~ "Unicorn",
      rating == "Strong Buy"                          ~ "Strong Buy",
      rating == "Buy"                                 ~ "Buy",
      rating == "Hold"                                ~ "Hold",
      TRUE                                            ~ "Sell"
    ),

    # Score explanation string for UI
    score_explanation = paste0(
      "Growth: ", growth_percentile, "th | ",
      "Momentum: ", momentum_percentile, "th | ",
      "Quality: ", quality_percentile, "th | ",
      "Value: ", value_percentile, "th"
    ),

    # Regime-adjusted label
    rating_with_regime = paste0(rating, " (", regime, ")")
  )

  # Save master scored file
  if (!dir.exists("data")) dir.create("data", recursive=TRUE)
  if (!dir.exists("app"))  dir.create("app",  recursive=TRUE)
  write_csv(df,             "data/master_scored.csv")
  write_csv(df,             "app/master_scored.csv")
  write_csv(top20,          "data/top15_sweetspot.csv")
  write_csv(top20,          "app/top15_sweetspot.csv")
  write_csv(top15_unicorns, "data/top15_unicorns.csv")
  write_csv(top15_unicorns, "app/top15_unicorns.csv")

  # Save meta
  saveRDS(list(
    last_updated   = Sys.time(),
    n_stocks       = nrow(df),
    regime         = regime,
    fed_rate       = df$fed_rate[1],
    yield_spread   = df$yield_spread[1],
    weights        = weights,
    top20_symbols  = top20$symbol,
    unicorn_symbols= top15_unicorns$symbol
  ), "app/meta.rds")

  message("\n=== MODULE 4 COMPLETE ===")
  message("Universe scored:      ", nrow(df))
  message("Top 20 Sweet Spot:    ", nrow(top20))
  message("Top 15 Unicorns:      ", nrow(top15_unicorns))
  message("Current Regime:       ", regime)
  message("Avg confidence (1d):  ", round(mean(df$confidence_1d, na.rm=TRUE), 1), "%")
  if (nrow(top20) > 0) {
    message("\nTOP 20 SWEET SPOT:")
    for (i in 1:min(5, nrow(top20))) {
      message("  ", i, ". ", top20$symbol[i],
              " | Score: ", top20$sweet_spot_score[i],
              " | Conf: ", top20$sweet_spot_confidence[i], "%",
              " | Exp: +", top20$expected_return_1d[i], "%",
              " | ", top20$rating[i])
    }
  }
  if (nrow(top15_unicorns) > 0) {
    message("\nTOP 15 UNICORNS:")
    for (i in 1:min(5, nrow(top15_unicorns))) {
      message("  ", i, ". ", top15_unicorns$symbol[i],
              " | Score: ", top15_unicorns$unicorn_score[i],
              " | Rev Growth: ", round(top15_unicorns$revenue_growth[i]*100,1), "%",
              " | Conf: ", top15_unicorns$unicorn_confidence[i], "%")
    }
  }

  list(
    master    = df,
    top20     = top20,
    unicorns  = top15_unicorns,
    regime    = regime,
    weights   = weights
  )
}

if (!exists("SOURCED_BY_MASTER")) results <- run_module4()
