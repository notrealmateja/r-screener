# =============================================================================
# MODULE 2 — PRICE, MOMENTUM & TECHNICAL ANALYSIS
# RSI, MACD, Bollinger Bands, moving averages, returns
# =============================================================================
library(tidyquant); library(dplyr); library(tidyr)
library(TTR); library(readr); library(glue); library(lubridate)

run_module2 <- function(tickers=NULL) {
  message("\n=== MODULE 2: MOMENTUM & TECHNICALS ===\n")

  if (is.null(tickers)) {
    fund <- read_csv("data/fundamentals_scored.csv", show_col_types=FALSE)
    tickers <- fund$symbol
  }

  message(glue("Pulling 1-year prices for {length(tickers)} stocks..."))
  prices <- tq_get(tickers, get="stock.prices",
                   from=Sys.Date()-365, to=Sys.Date())
  message("Calculating technical indicators...")

  tech <- prices %>%
    group_by(symbol) %>%
    arrange(date) %>%
    mutate(
      ma20   = SMA(close, 20),
      ma50   = SMA(close, 50),
      ma200  = SMA(close, 200),
      ema12  = EMA(close, 12),
      ema26  = EMA(close, 26),
      rsi14  = RSI(close, 14),
      macd_line   = MACD(close,12,26,9)[,"macd"],
      macd_signal = MACD(close,12,26,9)[,"signal"],
      macd_hist   = macd_line - macd_signal,
      bb_upper = as.numeric(TTR::BBands(cbind(close,close,close),n=20)[,"up"]),
      bb_lower = as.numeric(TTR::BBands(cbind(close,close,close),n=20)[,"dn"]),
      bb_mid   = as.numeric(TTR::BBands(cbind(close,close,close),n=20)[,"mavg"]),
      bb_pct   = (close - bb_lower) / (bb_upper - bb_lower),
      atr14    = ATR(cbind(high,low,close), 14)[,"atr"],
      daily_ret = (close/lag(close))-1,
      vol20     = rollapply(daily_ret, 20, sd, na.rm=TRUE, fill=NA, align="right") * sqrt(252)
    ) %>%
    ungroup()

  today       <- max(tech$date, na.rm=TRUE)
  d1m  <- today - 30; d3m  <- today - 90
  d6m  <- today - 180; d1y <- today - 365

  summary <- tech %>%
    group_by(symbol) %>%
    arrange(date) %>%
    summarize(
      price_now  = last(close),
      price_1m   = close[which.min(abs(date - d1m))],
      price_3m   = close[which.min(abs(date - d3m))],
      price_6m   = close[which.min(abs(date - d6m))],
      price_1y   = first(close),
      ret_1m     = (price_now/price_1m)-1,
      ret_3m     = (price_now/price_3m)-1,
      ret_6m     = (price_now/price_6m)-1,
      ret_1y     = (price_now/price_1y)-1,
      ma20       = last(ma20, na_rm=TRUE),
      ma50       = last(ma50, na_rm=TRUE),
      ma200      = last(ma200, na_rm=TRUE),
      rsi14      = last(rsi14, na_rm=TRUE),
      macd_line  = last(macd_line, na_rm=TRUE),
      macd_signal= last(macd_signal, na_rm=TRUE),
      macd_hist  = last(macd_hist, na_rm=TRUE),
      bb_upper   = last(bb_upper, na_rm=TRUE),
      bb_lower   = last(bb_lower, na_rm=TRUE),
      bb_pct     = last(bb_pct, na_rm=TRUE),
      vol_annual = last(vol20, na_rm=TRUE),
      avg_vol_30 = mean(volume[date >= d1m], na.rm=TRUE),
      .groups="drop"
    ) %>%
    mutate(
      above_ma20  = price_now > ma20,
      above_ma50  = price_now > ma50,
      above_ma200 = price_now > ma200,
      golden_cross= ma50 > ma200,
      rsi_zone    = case_when(rsi14>=70~"Overbought", rsi14<=30~"Oversold", TRUE~"Neutral"),
      macd_bullish= macd_line > macd_signal,
      trend_score = case_when(
        above_ma50 & above_ma200 & golden_cross ~ 100,
        above_ma50 & above_ma200               ~ 80,
        above_ma50 & !above_ma200              ~ 55,
        !above_ma50 & above_ma200              ~ 35,
        TRUE                                   ~ 15),
      rsi_score   = case_when(
        is.na(rsi14) ~ 50,
        rsi14>=50 & rsi14<65 ~ 85,
        rsi14>=65 & rsi14<70 ~ 65,
        rsi14>=70            ~ 30,
        rsi14>=40 & rsi14<50 ~ 45,
        TRUE ~ 20),
      ret3m_score  = percent_rank(replace_na(ret_3m,0))*100,
      ret6m_score  = percent_rank(replace_na(ret_6m,0))*100,
      macd_score   = ifelse(macd_bullish, 70, 30),
      momentum_score = (trend_score*0.25 + rsi_score*0.15 +
                        ret3m_score*0.30 + ret6m_score*0.20 + macd_score*0.10),
      across(where(is.numeric), ~round(.,4))
    )

  write_csv(summary, "data/momentum_scored.csv")
  # Also save full price history for charting
  write_csv(tech %>% select(symbol,date,open,high,low,close,volume,
                             ma20,ma50,ma200,rsi14,macd_line,macd_signal,
                             macd_hist,bb_upper,bb_lower,bb_mid,daily_ret),
            "data/price_history.csv")

  message("Saved: data/momentum_scored.csv + data/price_history.csv")
  message(glue("Top momentum: {summary$symbol[which.max(summary$momentum_score)]}"))
  summary
}

if (!exists("SOURCED_BY_MASTER")) momentum_data <- run_module2()
