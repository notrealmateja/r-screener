# =============================================================================
# MODULE 4 — ALPHA-CENTERED QUANTITATIVE SCORING ENGINE
#
# Primary rank: alpha_score (excess return vs SPY, compounded daily)
# Supporting:   quality_gate (fundamentals), tech_filter (trend/momentum)
# Confidence:   confidence_weight ramps 0→1 as days_tracked → 63
#
# All column names match what app/app.R and app/global.R expect:
#   master_score, rating, golden_cross_flag, regime,
#   sector, company, squeeze_candidate, etc.
# =============================================================================

run_module4 <- function(fund_data = NULL) {
  message("\n=== MODULE 4: ALPHA-CENTERED SCORING ENGINE ===\n")

  library(dplyr)
  library(readr)
  library(tidyr)

  # ── 1. Static lookups for the 50-ticker universe ──────────────────────────
  # company name, sector, and rough market-cap tier (used by the app UI)
  ticker_meta <- tibble::tribble(
    ~symbol,  ~company,                            ~sector,
    "AAPL",   "Apple Inc.",                        "Technology",
    "MSFT",   "Microsoft Corp.",                   "Technology",
    "NVDA",   "NVIDIA Corp.",                      "Technology",
    "AMZN",   "Amazon.com Inc.",                   "Consumer Discretionary",
    "GOOGL",  "Alphabet Inc.",                     "Communication Services",
    "META",   "Meta Platforms Inc.",               "Communication Services",
    "TSLA",   "Tesla Inc.",                        "Consumer Discretionary",
    "JPM",    "JPMorgan Chase & Co.",              "Financials",
    "V",      "Visa Inc.",                         "Financials",
    "UNH",    "UnitedHealth Group Inc.",           "Health Care",
    "XOM",    "Exxon Mobil Corp.",                 "Energy",
    "LLY",    "Eli Lilly and Co.",                 "Health Care",
    "JNJ",    "Johnson & Johnson",                 "Health Care",
    "WMT",    "Walmart Inc.",                      "Consumer Staples",
    "MA",     "Mastercard Inc.",                   "Financials",
    "PG",     "Procter & Gamble Co.",              "Consumer Staples",
    "HD",     "Home Depot Inc.",                   "Consumer Discretionary",
    "MRK",    "Merck & Co. Inc.",                  "Health Care",
    "ORCL",   "Oracle Corp.",                      "Technology",
    "BAC",    "Bank of America Corp.",             "Financials",
    "ABBV",   "AbbVie Inc.",                       "Health Care",
    "KO",     "Coca-Cola Co.",                     "Consumer Staples",
    "PEP",    "PepsiCo Inc.",                      "Consumer Staples",
    "AVGO",   "Broadcom Inc.",                     "Technology",
    "CVX",    "Chevron Corp.",                     "Energy",
    "COST",   "Costco Wholesale Corp.",            "Consumer Staples",
    "MCD",    "McDonald's Corp.",                  "Consumer Discretionary",
    "TMO",    "Thermo Fisher Scientific Inc.",     "Health Care",
    "CRM",    "Salesforce Inc.",                   "Technology",
    "NFLX",   "Netflix Inc.",                      "Communication Services",
    "ACN",    "Accenture PLC",                     "Technology",
    "LIN",    "Linde PLC",                         "Materials",
    "DHR",    "Danaher Corp.",                     "Health Care",
    "TXN",    "Texas Instruments Inc.",            "Technology",
    "NEE",    "NextEra Energy Inc.",               "Utilities",
    "PM",     "Philip Morris International Inc.",  "Consumer Staples",
    "MS",     "Morgan Stanley",                    "Financials",
    "RTX",    "RTX Corp.",                         "Industrials",
    "AMGN",   "Amgen Inc.",                        "Health Care",
    "HON",    "Honeywell International Inc.",      "Industrials",
    "UPS",    "United Parcel Service Inc.",        "Industrials",
    "QCOM",   "Qualcomm Inc.",                     "Technology",
    "IBM",    "IBM Corp.",                         "Technology",
    "CAT",    "Caterpillar Inc.",                  "Industrials",
    "GE",     "GE Aerospace",                      "Industrials",
    "INTU",   "Intuit Inc.",                       "Technology",
    "SPGI",   "S&P Global Inc.",                   "Financials",
    "AMD",    "Advanced Micro Devices Inc.",       "Technology",
    "ISRG",   "Intuitive Surgical Inc.",           "Health Care",
    "BLK",    "BlackRock Inc.",                    "Financials"
  )

  # ── 2. Load data ────────────────────────────────────────────────────────────
  if (is.null(fund_data)) {
    fund_data <- read_csv("data/fundamentals_scored.csv", show_col_types = FALSE)
    mom_data  <- read_csv("data/momentum_scored.csv",     show_col_types = FALSE)
    df <- merge(fund_data, mom_data, by = "symbol", all = TRUE)
  } else {
    df <- fund_data
  }

  # Merge squeeze scores if available
  squeeze_path <- "data/squeeze_scored.csv"
  if (file.exists(squeeze_path)) {
    sq <- read_csv(squeeze_path, show_col_types = FALSE) %>%
      select(symbol, squeeze_score = squeeze_score, squeeze_tier)
    df <- left_join(df, sq, by = "symbol")
  } else {
    df$squeeze_score <- NA_real_
    df$squeeze_tier  <- "No Signal"
  }

  # Merge Polygon data (options, SEC financials, VWAP, snapshot)
  poly_path <- "data/polygon_scored.csv"
  if (file.exists(poly_path)) {
    poly <- read_csv(poly_path, show_col_types = FALSE) %>%
      select(symbol,
             put_call_ratio, put_call_oi, options_volume, avg_iv,
             options_score, options_sentiment, above_vwap, squeeze_poly,
             debt_equity_poly, revenue_poly, gross_margin, fcf,
             shares_float, market_cap_poly, vwap, vol_ratio, open_gap_pct)
    df <- left_join(df, poly, by = "symbol")
    # Prefer Polygon debt/equity where AV didn't return it
    if (!"debt_equity" %in% names(df)) df$debt_equity <- NA_real_
    df <- df %>% mutate(
      debt_equity = dplyr::coalesce(debt_equity, debt_equity_poly),
      market_cap  = dplyr::coalesce(market_cap,  market_cap_poly)
    )
  } else {
    df <- df %>% mutate(
      put_call_ratio = NA_real_, options_score = 50,
      options_sentiment = "Unknown", above_vwap = NA,
      debt_equity_poly = NA_real_, gross_margin = NA_real_
    )
  }

  # Merge company/sector lookup
  df <- left_join(df, ticker_meta, by = "symbol")

  n <- nrow(df)
  message("Scoring ", n, " stocks...\n")

  # ── 3. Derive market regime from macro data ──────────────────────────────────
  regime <- "NEUTRAL"
  macro_path <- "data/macro_data.csv"
  if (file.exists(macro_path)) {
    macro <- tryCatch(read_csv(macro_path, show_col_types = FALSE), error = function(e) NULL)
    if (!is.null(macro) && nrow(macro) > 0) {
      get_latest <- function(ticker_sym) {
        v <- macro %>% filter(ticker == ticker_sym) %>%
          arrange(desc(date)) %>% slice(1) %>% pull(price)
        if (length(v) == 0) NA_real_ else v
      }
      ff    <- get_latest("FEDFUNDS")
      t10y2 <- get_latest("T10Y2Y")
      regime <- dplyr::case_when(
        !is.na(t10y2) & t10y2 < -0.25              ~ "INVERTED",
        !is.na(ff)    & ff >= 4.5                   ~ "HIGH_RATE",
        !is.na(ff)    & ff <= 1.5                   ~ "LOW_RATE",
        !is.na(t10y2) & t10y2 > 0.5                ~ "NEUTRAL",
        TRUE                                        ~ "NEUTRAL"
      )
    }
  }
  message(glue::glue("Market regime: {regime}\n"))

  # ── 4. Helper: robust 0-100 percentile scaler ──────────────────────────────
  pct_scale <- function(x, higher_is_better = TRUE) {
    r <- percent_rank(ifelse(is.finite(x), x, NA_real_)) * 100
    if (!higher_is_better) r <- 100 - r
    round(ifelse(is.na(x) | !is.finite(x), 50, r), 1)
  }

  # ── 5. Alpha score (primary ranking signal) ─────────────────────────────────
  message("[Step 1] Computing alpha factor scores...")

  df <- df %>% mutate(
    hist_alpha_ann    = ifelse(is.finite(hist_alpha_ann),    hist_alpha_ann,    NA_real_),
    hist_ir           = ifelse(is.finite(hist_ir),           hist_ir,           NA_real_),
    alpha_hit_rate    = ifelse(is.finite(alpha_hit_rate),    alpha_hit_rate,    NA_real_),
    alpha_streak      = ifelse(is.finite(alpha_streak),      alpha_streak,      NA_real_),
    confidence_weight = ifelse(is.finite(confidence_weight), confidence_weight, 0),
    days_tracked      = ifelse(is.finite(days_tracked),      as.integer(days_tracked), 0L),

    p_ann_alpha  = pct_scale(hist_alpha_ann,  TRUE),
    p_ir         = pct_scale(hist_ir,         TRUE),
    p_hit_rate   = pct_scale(alpha_hit_rate,  TRUE),
    p_streak     = pct_scale(alpha_streak,    TRUE),
    p_alpha_63d  = pct_scale(alpha_63d,       TRUE),

    alpha_score_raw = round(
      p_ann_alpha * 0.30 +
      p_ir        * 0.25 +
      p_hit_rate  * 0.20 +
      p_alpha_63d * 0.15 +
      p_streak    * 0.10, 1),

    alpha_score = round(
      alpha_score_raw * confidence_weight + 50 * (1 - confidence_weight), 1)
  )

  # ── 6. Quality gate (fundamental health) ───────────────────────────────────
  message("[Step 2] Computing quality gate from fundamentals...")

  df <- df %>% mutate(
    q_pe     = pct_scale(ifelse(pe_ratio > 0 & pe_ratio < 200, pe_ratio, NA_real_), FALSE),
    q_pb     = pct_scale(ifelse(pb_ratio > 0 & pb_ratio < 50,  pb_ratio, NA_real_), FALSE),
    q_roe    = pct_scale(roe,           TRUE),
    q_de     = pct_scale(debt_equity,   FALSE),
    q_margin = pct_scale(profit_margin, TRUE),
    q_pe     = ifelse(is.na(pe_ratio),      50, q_pe),
    q_pb     = ifelse(is.na(pb_ratio),      50, q_pb),
    q_roe    = ifelse(is.na(roe),           50, q_roe),
    q_de     = ifelse(is.na(debt_equity),   50, q_de),
    q_margin = ifelse(is.na(profit_margin), 50, q_margin),
    # Analyst consensus from Alpha Vantage (50 if not available)
    q_analyst = ifelse(is.na(analyst_score), 50, analyst_score),

    quality_gate = round(
      q_pe       * 0.22 +
      q_pb       * 0.13 +
      q_roe      * 0.25 +
      q_de       * 0.15 +
      q_margin   * 0.15 +
      q_analyst  * 0.10,   # analyst consensus as a quality signal
      1)
  )

  # ── 7. Technical momentum filter ────────────────────────────────────────────
  message("[Step 3] Computing technical momentum filter...")

  df <- df %>% mutate(
    tech_filter = round(
      pct_scale(momentum_score, TRUE) * 0.50 +
      pct_scale(ret_6m,         TRUE) * 0.30 +
      pct_scale(rsi14,          TRUE) * 0.20, 1),
    tech_filter = ifelse(is.na(momentum_score), 50, tech_filter)
  )

  # ── 8. Final score and all app-expected columns ────────────────────────────
  message("[Step 4] Computing final scores and app columns...")

  df <- df %>% mutate(
    # Options signal from Polygon (50 if unavailable — no penalty for missing data)
    options_signal = ifelse(is.na(options_score), 50, options_score),

    # Primary ranking score — now includes options market sentiment
    final_score  = round(
      alpha_score    * 0.55 +
      tech_filter    * 0.22 +
      quality_gate   * 0.13 +
      options_signal * 0.10,   # Polygon put/call ratio signal
      1),

    # ── App expects these exact column names ──
    master_score = final_score,                            # app reads master_score

    rating = dplyr::case_when(                             # app reads rating
      final_score >= 78 & confidence_weight >= 0.3 ~ "Strong Buy",
      final_score >= 65                            ~ "Buy",
      final_score >= 45                            ~ "Hold",
      final_score >= 30                            ~ "Underperform",
      TRUE                                         ~ "Avoid"
    ),

    signal = rating,                                       # keep signal too

    golden_cross_flag = isTRUE(golden_cross),              # app reads golden_cross_flag

    squeeze_candidate = (!is.na(squeeze_score) &           # app reads squeeze_candidate
                           squeeze_score >= 60),

    regime = regime,                                       # app reads regime

    # Confidence metadata
    confidence_band = dplyr::case_when(
      days_tracked >= 126 ~ "Proven",
      days_tracked >=  63 ~ "Established",
      days_tracked >=  20 ~ "Developing",
      TRUE                ~ "Emerging"
    ),

    # Enriched columns for tables
    master_percentile  = round(percent_rank(final_score) * 100, 1),
    expected_return_1d = round(ifelse(is.finite(hist_alpha_ann), hist_alpha_ann / 252, NA_real_), 6),
    revenue_growth     = earningsGrowth,   # best proxy available

    primary_driver = dplyr::case_when(
      p_ann_alpha >= 70 & days_tracked >= 20          ~ "Alpha Leader",
      p_ir        >= 70 & days_tracked >= 20          ~ "High IR",
      !is.na(put_call_ratio) & put_call_ratio < 0.6   ~ "Smart $ Bullish",
      p_hit_rate  >= 70                               ~ "Consistent Beat",
      tech_filter >= 70                               ~ "Strong Trend",
      !is.na(analyst_upside) & analyst_upside > 0.15  ~ "Analyst Upside",
      quality_gate >= 70                              ~ "Quality",
      TRUE                                            ~ "Balanced"
    ),

    signal_matches = paste0(
      ifelse(isTRUE(golden_cross),                                    "GX ",       ""),
      ifelse(isTRUE(macd_bullish),                                    "MACD ",     ""),
      ifelse(!is.na(rsi14) & rsi14 >= 50 & rsi14 < 70,               "RSI ",      ""),
      ifelse(isTRUE(above_ma200),                                     "MA200 ",    ""),
      ifelse(!is.na(alpha_hit_rate) & alpha_hit_rate >= 0.55,         "α>SPY ",    ""),
      ifelse(!is.na(put_call_ratio) & put_call_ratio < 0.75,          "PUT/CALL ", ""),
      ifelse(!is.na(analyst_upside) & analyst_upside > 0.10,          "ANALYST",   "")
    ) %>% trimws(),

    across(where(is.numeric), ~ round(., 4))
  )

  # ── 9. Save master_scored.csv ───────────────────────────────────────────────
  message("[Step 5] Saving results...\n")
  if (!dir.exists("data")) dir.create("data", recursive = TRUE)
  write_csv(df, "data/master_scored.csv")
  message("Saved: data/master_scored.csv (", n, " stocks)\n")

  # ── 10. Top 15 Sweetspot: all columns the app table needs ──────────────────
  top15_sweetspot <- df %>%
    arrange(desc(final_score)) %>%
    head(15) %>%
    transmute(
      symbol, company, sector, price,
      # scores
      sweet_spot_score      = final_score,
      master_score          = final_score,
      master_percentile,
      alpha_score,
      quality_gate,
      tech_filter,
      # rating / confidence
      rating,
      signal,
      sweet_spot_confidence = round(confidence_weight * 100, 1),
      confidence_band,
      days_tracked,
      # return expectation
      expected_return_1d,
      # alpha metrics
      hist_alpha_ann,
      hist_ir,
      alpha_hit_rate,
      alpha_streak,
      confidence_weight,
      # descriptive
      primary_driver,
      signal_matches,
      # fundamental
      pe_ratio, pb_ratio, market_cap, roe, profit_margin,
      # momentum
      ret_1m, ret_3m, ret_6m, rsi14,
      momentum_score,
      # Polygon options
      put_call_ratio, options_sentiment, options_score, avg_iv, above_vwap,
      # Alpha Vantage analyst
      analyst_target, analyst_upside, analyst_score
    )
  write_csv(top15_sweetspot, "data/top15_sweetspot.csv")

  # ── 11. Top 15 Unicorns: IR-ranked with all app columns ───────────────────
  top15_unicorns <- df %>%
    filter(!is.na(hist_ir)) %>%
    arrange(desc(hist_ir)) %>%
    head(15) %>%
    transmute(
      symbol, company, sector, price,
      unicorn_score         = round(pmin(pmax((hist_ir + 3) / 6 * 100, 0), 100), 1),
      master_score          = final_score,
      rating,
      signal,
      unicorn_confidence    = round(confidence_weight * 100, 1),
      confidence_5d         = round(confidence_weight * 100, 1),
      confidence_band,
      days_tracked,
      hist_ir,
      hist_alpha_ann,
      alpha_hit_rate,
      alpha_streak,
      expected_return_1d,
      primary_driver,
      # fundamental
      pe_ratio, market_cap,
      revenue_growth,
      # momentum
      ret_1m, ret_3m, rsi14,
      momentum_score,
      # Polygon
      put_call_ratio, options_sentiment, avg_iv,
      # AV analyst
      analyst_target, analyst_upside
    )
  write_csv(top15_unicorns, "data/top15_unicorns.csv")

  message("Saved: top15_sweetspot.csv + top15_unicorns.csv\n")

  # ── 12. Meta file ──────────────────────────────────────────────────────────
  meta_obj <- list(
    last_updated     = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    n_stocks         = n,
    max_days_tracked = max(df$days_tracked, na.rm = TRUE),
    top_alpha_stock  = df$symbol[which.max(df$hist_alpha_ann)],
    regime           = regime
  )
  if (!dir.exists("data")) dir.create("data", recursive = TRUE)
  saveRDS(meta_obj, "data/meta.rds")
  # Also write to data/data/ so app.R's "data/meta.rds" path resolves correctly
  # when Shiny sets working directory to app/
  if (!dir.exists("app/data")) dir.create("app/data", recursive = TRUE)
  saveRDS(meta_obj, "app/data/meta.rds")

  # ── 13. Print leaderboard ─────────────────────────────────────────────────
  lb <- df %>% arrange(desc(final_score)) %>% head(5)
  message("=== MODULE 4 COMPLETE ===")
  message("Top 5 by master_score:")
  for (i in seq_len(nrow(lb))) {
    message(sprintf("  %d. %-6s  score=%.1f  alpha=%.1f  IR=%.2f  %s  [%s]",
                    i,
                    lb$symbol[i],
                    lb$master_score[i],
                    lb$alpha_score[i],
                    ifelse(is.na(lb$hist_ir[i]), 0, lb$hist_ir[i]),
                    lb$rating[i],
                    lb$confidence_band[i]))
  }

  return(df)
}

# Auto-run if sourced by master
if (exists("SOURCED_BY_MASTER")) results <- run_module4()
