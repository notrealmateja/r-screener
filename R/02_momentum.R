# =============================================================================
# MODULE 2 — PRICE, MOMENTUM, TECHNICALS & ALPHA CALCULATION
#
# Alpha philosophy:
#   Each day this runs it computes each stock's excess return vs SPY and
#   APPENDS a row to data/alpha_history.csv.  Over time the history grows —
#   after 20 days you have a rough signal; after 63 days (one quarter) the
#   information ratio becomes statistically meaningful; after 252 days (one
#   year) the estimates are robust.  Every metric that feeds Module 4 is
#   confidence-weighted by how many days of history exist.
#
# Key alpha metrics produced
#   daily_alpha       — today's excess return vs SPY
#   beta              — 252-day rolling market sensitivity
#   jensen_alpha_ann  — annualized Jensen's alpha (CAPM-adjusted)
#   info_ratio        — annualised alpha / tracking-error (consistency)
#   alpha_hit_rate    — % of days stock beat SPY (over available history)
#   alpha_30d / 63d   — cumulative alpha over recent windows
#   confidence_weight — scales 0→1 as days_tracked grows to 63
# =============================================================================
library(tidyquant); library(dplyr); library(tidyr)
library(TTR); library(readr); library(glue); library(lubridate)

run_module2 <- function(tickers = NULL) {
  message("\n=== MODULE 2: MOMENTUM, TECHNICALS & ALPHA ===\n")

  if (is.null(tickers)) {
    fund   <- read_csv("data/fundamentals_scored.csv", show_col_types = FALSE)
    tickers <- fund$symbol
  }
  tickers <- unique(tickers)

  # ── 1. Fetch prices (tickers + SPY benchmark) ──────────────────────────────
  all_syms <- unique(c(tickers, "SPY"))
  message(glue("Pulling 1-year prices for {length(tickers)} stocks + SPY benchmark..."))

  prices_raw <- tq_get(all_syms,
                       get  = "stock.prices",
                       from = Sys.Date() - 365,
                       to   = Sys.Date())

  spy_ret <- prices_raw %>%
    filter(symbol == "SPY") %>%
    arrange(date) %>%
    transmute(date, spy_ret = (close / lag(close)) - 1)

  prices <- prices_raw %>% filter(symbol != "SPY")

  # ── 2. Technical indicators ────────────────────────────────────────────────
  message("Calculating technical indicators...")
  tech <- prices %>%
    group_by(symbol) %>%
    arrange(date) %>%
    mutate(
      ma20        = SMA(close, 20),
      ma50        = SMA(close, 50),
      ma200       = SMA(close, 200),
      ema12       = EMA(close, 12),
      ema26       = EMA(close, 26),
      rsi14       = RSI(close, 14),
      macd_line   = MACD(close, 12, 26, 9)[, "macd"],
      macd_signal = MACD(close, 12, 26, 9)[, "signal"],
      macd_hist   = macd_line - macd_signal,
      bb_upper    = as.numeric(TTR::BBands(cbind(close, close, close), n = 20)[, "up"]),
      bb_lower    = as.numeric(TTR::BBands(cbind(close, close, close), n = 20)[, "dn"]),
      bb_mid      = as.numeric(TTR::BBands(cbind(close, close, close), n = 20)[, "mavg"]),
      bb_pct      = (close - bb_lower) / (bb_upper - bb_lower),
      atr14       = ATR(cbind(high, low, close), 14)[, "atr"],
      daily_ret   = (close / lag(close)) - 1,
      vol20       = rollapply(daily_ret, 20, sd, na.rm = TRUE,
                              fill = NA, align = "right") * sqrt(252)
    ) %>%
    ungroup()

  # ── 3. Alpha vs SPY ────────────────────────────────────────────────────────
  message("Computing daily alpha vs SPY...")
  tech <- tech %>%
    left_join(spy_ret, by = "date") %>%
    mutate(
      # Raw excess return (daily)
      daily_alpha = daily_ret - spy_ret,

      # Rolling beta: cov(stock, spy) / var(spy) over trailing 63 days
      # We compute it post-hoc in the summary below using lm per group
    )

  compute_beta <- function(stock_ret, bench_ret) {
    ok <- !is.na(stock_ret) & !is.na(bench_ret)
    if (sum(ok) < 20) return(NA_real_)
    coef(lm(stock_ret[ok] ~ bench_ret[ok]))[2]
  }

  today <- max(tech$date, na.rm = TRUE)
  d1m   <- today - 30
  d3m   <- today - 90
  d6m   <- today - 180
  d1y   <- today - 365

  # ── 4. Per-ticker summary (today's snapshot) ──────────────────────────────
  message("Summarising per-ticker metrics...")
  summary_today <- tech %>%
    group_by(symbol) %>%
    arrange(date) %>%
    summarize(
      # ---- price levels ----
      price_now   = last(close),
      price_1m    = close[which.min(abs(date - d1m))],
      price_3m    = close[which.min(abs(date - d3m))],
      price_6m    = close[which.min(abs(date - d6m))],
      price_1y    = first(close),

      # ---- returns ----
      ret_1m      = (price_now / price_1m) - 1,
      ret_3m      = (price_now / price_3m) - 1,
      ret_6m      = (price_now / price_6m) - 1,
      ret_1y      = (price_now / price_1y) - 1,

      # ---- technicals ----
      ma20        = last(ma20,        na_rm = TRUE),
      ma50        = last(ma50,        na_rm = TRUE),
      ma200       = last(ma200,       na_rm = TRUE),
      rsi14       = last(rsi14,       na_rm = TRUE),
      macd_line   = last(macd_line,   na_rm = TRUE),
      macd_signal = last(macd_signal, na_rm = TRUE),
      macd_hist   = last(macd_hist,   na_rm = TRUE),
      bb_upper    = last(bb_upper,    na_rm = TRUE),
      bb_lower    = last(bb_lower,    na_rm = TRUE),
      bb_pct      = last(bb_pct,      na_rm = TRUE),
      vol_annual  = last(vol20,       na_rm = TRUE),
      avg_vol_30  = mean(volume[date >= d1m], na.rm = TRUE),

      # ---- alpha (trailing 252 days of data available in this pull) ----
      beta             = compute_beta(daily_ret, spy_ret),
      # Jensen's alpha: mean excess return adjusted for beta * market return
      # annualised: multiply mean daily by 252
      spy_mean_daily   = mean(spy_ret[!is.na(spy_ret)], na.rm = TRUE),
      mean_daily_ret   = mean(daily_ret[!is.na(daily_ret)], na.rm = TRUE),
      mean_daily_alpha = mean(daily_alpha[!is.na(daily_alpha)], na.rm = TRUE),
      sd_daily_alpha   = sd(daily_alpha[!is.na(daily_alpha)], na.rm = TRUE),
      days_in_pull     = sum(!is.na(daily_alpha)),

      # Today's raw alpha (most recent observation)
      today_alpha      = last(daily_alpha, na_rm = TRUE),

      .groups = "drop"
    ) %>%
    mutate(
      # Jensen: E[R_i] - Rf - beta*(E[R_m] - Rf).  We set Rf≈0 for simplicity.
      jensen_alpha_ann = (mean_daily_ret - beta * spy_mean_daily) * 252,

      # Information ratio = annualised alpha / annualised tracking error
      tracking_error   = sd_daily_alpha * sqrt(252),
      info_ratio       = ifelse(tracking_error > 0,
                                (mean_daily_alpha * 252) / tracking_error,
                                NA_real_),

      # Technical composite scores (unchanged from before — used as filters)
      above_ma20   = price_now > ma20,
      above_ma50   = price_now > ma50,
      above_ma200  = price_now > ma200,
      golden_cross = ma50 > ma200,
      rsi_zone     = case_when(rsi14 >= 70 ~ "Overbought",
                               rsi14 <= 30 ~ "Oversold",
                               TRUE        ~ "Neutral"),
      macd_bullish = macd_line > macd_signal,
      trend_score  = case_when(
        above_ma50 & above_ma200 & golden_cross ~ 100,
        above_ma50 & above_ma200               ~ 80,
        above_ma50 & !above_ma200              ~ 55,
        !above_ma50 & above_ma200              ~ 35,
        TRUE                                   ~ 15),
      rsi_score    = case_when(
        is.na(rsi14)            ~ 50,
        rsi14 >= 50 & rsi14 < 65 ~ 85,
        rsi14 >= 65 & rsi14 < 70 ~ 65,
        rsi14 >= 70              ~ 30,
        rsi14 >= 40 & rsi14 < 50 ~ 45,
        TRUE                     ~ 20),
      ret3m_score  = percent_rank(replace_na(ret_3m, 0)) * 100,
      ret6m_score  = percent_rank(replace_na(ret_6m, 0)) * 100,
      macd_score   = ifelse(macd_bullish, 70, 30),
      momentum_score = (trend_score * 0.25 + rsi_score * 0.15 +
                          ret3m_score * 0.30 + ret6m_score * 0.20 +
                          macd_score * 0.10),
      across(where(is.numeric), ~ round(., 6))
    )

  # ── 5. Append today's alpha snapshot to the compounding history ────────────
  message("Appending today's alpha snapshot to alpha_history.csv...")
  if (!dir.exists("data")) dir.create("data", recursive = TRUE)

  today_snapshot <- summary_today %>%
    transmute(
      date             = today,
      symbol,
      daily_alpha      = today_alpha,
      mean_daily_alpha,
      jensen_alpha_ann,
      info_ratio,
      beta,
      ret_1m,
      ret_3m,
      momentum_score
    )

  history_path <- "data/alpha_history.csv"
  if (file.exists(history_path)) {
    existing <- read_csv(history_path, show_col_types = FALSE)
    # Avoid duplicate rows if re-run on same day
    existing <- existing %>% filter(!(date == today & symbol %in% today_snapshot$symbol))
    alpha_history <- bind_rows(existing, today_snapshot) %>% arrange(symbol, date)
  } else {
    alpha_history <- today_snapshot %>% arrange(symbol, date)
  }
  write_csv(alpha_history, history_path)
  message(glue("alpha_history.csv now has {nrow(alpha_history)} rows across ",
               "{n_distinct(alpha_history$date)} trading days."))

  # ── 6. Compute compounding rolling alpha metrics from full history ─────────
  message("Computing compounding alpha metrics from full history...")
  rolling_alpha <- alpha_history %>%
    group_by(symbol) %>%
    arrange(date) %>%
    summarize(
      days_tracked      = n(),

      # Cumulative alpha over all available history (compounded)
      cum_alpha_all     = prod(1 + replace_na(daily_alpha, 0)) - 1,

      # Rolling windows (use available days if fewer than window)
      alpha_30d         = {
        d <- tail(daily_alpha[!is.na(daily_alpha)], 30)
        if (length(d) == 0) NA_real_ else prod(1 + d) - 1
      },
      alpha_63d         = {
        d <- tail(daily_alpha[!is.na(daily_alpha)], 63)
        if (length(d) == 0) NA_real_ else prod(1 + d) - 1
      },

      # Hit rate: % of days the stock beat SPY
      alpha_hit_rate    = mean(daily_alpha > 0, na.rm = TRUE),

      # Annualised alpha & information ratio from history
      hist_mean_alpha   = mean(daily_alpha, na.rm = TRUE),
      hist_sd_alpha     = sd(daily_alpha,   na.rm = TRUE),
      hist_alpha_ann    = hist_mean_alpha * 252,
      hist_ir           = ifelse(hist_sd_alpha > 0,
                                 (hist_mean_alpha / hist_sd_alpha) * sqrt(252),
                                 NA_real_),

      # Confidence weight: ramps 0 → 1 as days_tracked grows to 63 (one quarter)
      # After 63 days the signal is considered fully weighted
      confidence_weight = pmin(days_tracked / 63, 1.0),

      # Alpha streak: consecutive days with positive alpha (most recent)
      alpha_streak      = {
        signs <- rev(as.integer(daily_alpha > 0))
        if (length(signs) == 0 || is.na(signs[1])) 0L
        else {
          streak <- 0L
          for (s in signs) {
            if (!is.na(s) && s == 1L) streak <- streak + 1L
            else break
          }
          streak
        }
      },

      .groups = "drop"
    )

  # ── 7. Merge and save ─────────────────────────────────────────────────────
  summary_out <- summary_today %>%
    left_join(rolling_alpha, by = "symbol")

  write_csv(summary_out, "data/momentum_scored.csv")

  # Full price + indicator history for charting
  write_csv(
    tech %>% select(symbol, date, open, high, low, close, volume,
                    ma20, ma50, ma200, rsi14, macd_line, macd_signal,
                    macd_hist, bb_upper, bb_lower, bb_mid,
                    daily_ret, spy_ret, daily_alpha),
    "data/price_history.csv"
  )

  message("Saved: data/momentum_scored.csv + data/price_history.csv")
  top_alpha <- summary_out %>% arrange(desc(hist_alpha_ann)) %>% slice(1)
  message(glue("Top alpha stock today: {top_alpha$symbol} ",
               "(ann. alpha {round(top_alpha$hist_alpha_ann*100,1)}%, ",
               "IR {round(top_alpha$hist_ir,2)}, ",
               "{top_alpha$days_tracked} days tracked)"))

  # Pass merged result upstream
  fund_data <- tryCatch(
    read_csv("data/fundamentals_scored.csv", show_col_types = FALSE),
    error = function(e) NULL
  )
  if (!is.null(fund_data)) {
    result <- merge(fund_data, summary_out, by = "symbol", all.x = TRUE)
  } else {
    result <- summary_out
  }
  result
}

if (!exists("SOURCED_BY_MASTER")) momentum_data <- run_module2()
