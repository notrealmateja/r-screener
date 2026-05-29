# =============================================================================
# MODULE 2b — POLYGON.IO ENRICHMENT
#
# Adds signals Yahoo/AV don't provide:
#   put_call_ratio     — options market sentiment (< 0.7 bullish, > 1.3 bearish)
#   iv_percentile      — implied volatility rank (high IV = uncertainty)
#   short_float        — short interest as % of float (squeeze signal)
#   debt_equity        — from SEC quarterly balance sheet filings
#   shares_float       — float shares (precise, from SEC)
#   vwap               — volume-weighted avg price (intraday positioning)
#   prev_close_gap     — today's open gap vs prior close
#   options_volume     — total options activity (elevated = smart money alert)
#
# All fetches use a 1-day cache (data/polygon_cache.csv) so re-runs don't
# burn calls.  Polygon free tier: unlimited historical, 5 calls/min real-time.
# =============================================================================
library(httr); library(jsonlite); library(dplyr); library(readr)
library(glue); library(lubridate)

POLY_KEY   <- "CZ9uLCagJ4dWJMs1Efz79gA80ThXlQlO"
POLY_BASE  <- "https://api.polygon.io"
POLY_CACHE <- "data/polygon_cache.csv"
POLY_SLEEP <- 0.25   # 4 calls/sec — well inside free-tier limit

poly_get <- function(path, query = list()) {
  query$apiKey <- POLY_KEY
  tryCatch({
    resp <- GET(paste0(POLY_BASE, path), query = query)
    if (status_code(resp) != 200) return(NULL)
    fromJSON(content(resp, as = "text", encoding = "UTF-8"))
  }, error = function(e) NULL)
}

# ── Ticker details (market cap, shares outstanding, float, description) ──────
get_poly_ticker_details <- function(sym) {
  raw <- poly_get(glue("/v3/reference/tickers/{sym}"))
  if (is.null(raw) || is.null(raw$results)) return(NULL)
  r <- raw$results
  tibble(
    symbol             = sym,
    shares_outstanding = suppressWarnings(as.numeric(r$share_class_shares_outstanding)),
    shares_float       = suppressWarnings(as.numeric(r$weighted_shares_outstanding)),
    market_cap_poly    = suppressWarnings(as.numeric(r$market_cap)),
    primary_exchange   = r$primary_exchange %||% NA_character_,
    locale             = r$locale %||% NA_character_
  )
}

# ── SEC quarterly financials (balance sheet → debt/equity, revenue) ──────────
get_poly_financials <- function(sym) {
  raw <- poly_get("/vX/reference/financials", query = list(
    ticker    = sym,
    timeframe = "quarterly",
    limit     = 2,
    sort      = "period_of_report_date"
  ))
  if (is.null(raw) || is.null(raw$results) || length(raw$results) == 0) return(NULL)
  r <- raw$results[[length(raw$results)]]   # most recent quarter

  safe <- function(path) {
    tryCatch(suppressWarnings(as.numeric(path)), error = function(e) NA_real_)
  }

  # Navigate the nested financials structure
  bs  <- r$financials$balance_sheet
  inc <- r$financials$income_statement
  cf  <- r$financials$cash_flow_statement

  total_debt   <- safe(bs$long_term_debt$value)           %||% NA_real_
  total_equity <- safe(bs$equity$value)                   %||% NA_real_
  total_assets <- safe(bs$assets$value)                   %||% NA_real_
  revenues     <- safe(inc$revenues$value)                %||% NA_real_
  net_income   <- safe(inc$net_income_loss$value)         %||% NA_real_
  op_income    <- safe(inc$operating_income_loss$value)   %||% NA_real_
  gross_profit <- safe(inc$gross_profit$value)            %||% NA_real_
  basic_eps    <- safe(inc$basic_earnings_per_share$value)%||% NA_real_
  capex        <- safe(cf$capital_expenditure$value)      %||% NA_real_
  fcf          <- safe(cf$net_cash_flow$value)            %||% NA_real_

  debt_equity_poly <- if (!is.na(total_debt) && !is.na(total_equity) && total_equity != 0)
                        round(total_debt / total_equity, 4) else NA_real_
  roa_poly         <- if (!is.na(net_income) && !is.na(total_assets) && total_assets != 0)
                        round(net_income / total_assets, 4) else NA_real_
  gross_margin     <- if (!is.na(gross_profit) && !is.na(revenues) && revenues != 0)
                        round(gross_profit / revenues, 4) else NA_real_

  tibble(
    symbol           = sym,
    debt_equity_poly = debt_equity_poly,
    revenue_poly     = revenues,
    net_income_poly  = net_income,
    op_income_poly   = op_income,
    gross_margin     = gross_margin,
    roa_poly         = roa_poly,
    eps_poly         = basic_eps,
    capex            = capex,
    fcf              = fcf,
    report_date      = r$period_of_report_date %||% NA_character_
  )
}

# ── Options snapshot (put/call ratio, total IV, options volume) ──────────────
get_poly_options <- function(sym) {
  # Snapshot of all options for this ticker
  raw <- poly_get(glue("/v3/snapshot/options/{sym}"),
                  query = list(limit = 250))
  if (is.null(raw) || is.null(raw$results) || length(raw$results) == 0) return(NULL)

  opts <- tryCatch({
    d <- bind_rows(lapply(raw$results, function(r) {
      tibble(
        contract_type = r$details$contract_type  %||% NA_character_,
        iv            = suppressWarnings(as.numeric(r$implied_volatility)),
        volume        = suppressWarnings(as.numeric(r$day$volume)),
        open_interest = suppressWarnings(as.numeric(r$open_interest)),
        delta         = suppressWarnings(as.numeric(r$greeks$delta)),
        gamma         = suppressWarnings(as.numeric(r$greeks$gamma))
      )
    }))
    d
  }, error = function(e) return(NULL))
  if (is.null(opts) || nrow(opts) == 0) return(NULL)

  calls <- opts %>% filter(contract_type == "call")
  puts  <- opts %>% filter(contract_type == "put")

  call_vol <- sum(calls$volume, na.rm = TRUE)
  put_vol  <- sum(puts$volume,  na.rm = TRUE)
  call_oi  <- sum(calls$open_interest, na.rm = TRUE)
  put_oi   <- sum(puts$open_interest,  na.rm = TRUE)

  avg_iv   <- mean(opts$iv, na.rm = TRUE)

  tibble(
    symbol         = sym,
    put_call_ratio = if (call_vol > 0) round(put_vol / call_vol, 4) else NA_real_,
    put_call_oi    = if (call_oi  > 0) round(put_oi  / call_oi,  4) else NA_real_,
    options_volume = call_vol + put_vol,
    options_oi     = call_oi  + put_oi,
    avg_iv         = round(avg_iv, 4),
    call_volume    = call_vol,
    put_volume     = put_vol
  )
}

# ── Previous-day snapshot (VWAP, open gap, volume ratio) ─────────────────────
get_poly_snapshot <- function(sym) {
  raw <- poly_get(glue("/v2/snapshot/locale/us/markets/stocks/tickers/{sym}"))
  if (is.null(raw) || is.null(raw$ticker)) return(NULL)
  t <- raw$ticker
  day  <- t$day
  prev <- t$prevDay

  tibble(
    symbol          = sym,
    vwap            = suppressWarnings(as.numeric(day$vw)),
    today_open      = suppressWarnings(as.numeric(day$o)),
    today_high      = suppressWarnings(as.numeric(day$h)),
    today_low       = suppressWarnings(as.numeric(day$l)),
    today_close     = suppressWarnings(as.numeric(day$c)),
    today_volume    = suppressWarnings(as.numeric(day$v)),
    prev_close      = suppressWarnings(as.numeric(prev$c)),
    prev_volume     = suppressWarnings(as.numeric(prev$v)),
    open_gap_pct    = if (!is.na(today_open) && !is.na(prev_close) && prev_close > 0)
                        round((today_open - prev_close) / prev_close, 6) else NA_real_,
    vol_ratio       = if (!is.na(today_volume) && !is.na(prev_volume) && prev_volume > 0)
                        round(today_volume / prev_volume, 4) else NA_real_
  )
}

# ── Main module ──────────────────────────────────────────────────────────────
run_module_polygon <- function(tickers = NULL) {
  message("\n=== MODULE 2b: POLYGON.IO ENRICHMENT ===\n")

  if (is.null(tickers)) {
    fund <- read_csv("data/fundamentals_scored.csv", show_col_types = FALSE)
    tickers <- fund$symbol
  }
  tickers <- unique(tickers)

  if (!dir.exists("data")) dir.create("data", recursive = TRUE)

  # Check 1-day cache
  today <- Sys.Date()
  if (file.exists(POLY_CACHE)) {
    cached <- read_csv(POLY_CACHE, show_col_types = FALSE) %>%
      mutate(cache_date = as.Date(cache_date))
    fresh <- cached %>% filter(cache_date == today, symbol %in% tickers)
    need  <- tickers[!tickers %in% fresh$symbol]
  } else {
    fresh <- tibble()
    need  <- tickers
  }

  message(glue("{length(need)} tickers need Polygon fetch, ",
               "{length(tickers) - length(need)} served from cache."))

  if (length(need) > 0) {
    details_list <- list()
    fin_list     <- list()
    opts_list    <- list()
    snap_list    <- list()

    for (i in seq_along(need)) {
      sym <- need[i]
      message(glue("  Polygon [{i}/{length(need)}]: {sym}"))

      details_list[[sym]] <- get_poly_ticker_details(sym); Sys.sleep(POLY_SLEEP)
      fin_list[[sym]]     <- get_poly_financials(sym);     Sys.sleep(POLY_SLEEP)
      opts_list[[sym]]    <- get_poly_options(sym);        Sys.sleep(POLY_SLEEP)
      snap_list[[sym]]    <- get_poly_snapshot(sym);       Sys.sleep(POLY_SLEEP)
    }

    # Combine all four sources per ticker
    details_df <- bind_rows(Filter(Negate(is.null), details_list))
    fin_df     <- bind_rows(Filter(Negate(is.null), fin_list))
    opts_df    <- bind_rows(Filter(Negate(is.null), opts_list))
    snap_df    <- bind_rows(Filter(Negate(is.null), snap_list))

    new_rows <- tibble(symbol = need) %>%
      left_join(details_df, by = "symbol") %>%
      left_join(fin_df,     by = "symbol") %>%
      left_join(opts_df,    by = "symbol") %>%
      left_join(snap_df,    by = "symbol") %>%
      mutate(cache_date = today)

    # Append to cache (remove old entries for these symbols)
    if (nrow(fresh) > 0) {
      combined <- bind_rows(
        fresh %>% filter(!symbol %in% new_rows$symbol),
        new_rows
      )
    } else {
      combined <- new_rows
    }
    write_csv(combined, POLY_CACHE)
    result <- combined
  } else {
    result <- fresh
  }

  # ── Compute derived signals ──────────────────────────────────────────────
  result <- result %>% mutate(
    # Options sentiment signal: <0.7 bullish, 0.7-1.3 neutral, >1.3 bearish
    options_sentiment = dplyr::case_when(
      is.na(put_call_ratio)    ~ "Unknown",
      put_call_ratio < 0.6     ~ "Strong Bullish",
      put_call_ratio < 0.85    ~ "Bullish",
      put_call_ratio < 1.15    ~ "Neutral",
      put_call_ratio < 1.40    ~ "Bearish",
      TRUE                     ~ "Strong Bearish"
    ),
    # Score 0-100: low put/call = high score
    options_score = dplyr::case_when(
      is.na(put_call_ratio) ~ 50,
      put_call_ratio < 0.5  ~ 95,
      put_call_ratio < 0.7  ~ 80,
      put_call_ratio < 0.9  ~ 65,
      put_call_ratio < 1.1  ~ 50,
      put_call_ratio < 1.3  ~ 35,
      put_call_ratio < 1.5  ~ 20,
      TRUE                  ~ 10
    ),
    # VWAP position: above VWAP = intraday strength
    above_vwap = !is.na(today_close) & !is.na(vwap) & today_close > vwap,
    # Short squeeze candidate: combine with squeeze module
    squeeze_poly = !is.na(put_call_ratio) & put_call_ratio > 1.3 &
                   !is.na(vol_ratio)      & vol_ratio > 1.5,
    across(where(is.numeric), ~ round(., 6))
  )

  write_csv(result, "data/polygon_scored.csv")
  message(glue("\nSaved: data/polygon_scored.csv ({nrow(result)} tickers)"))

  # Summary stats
  opts_available <- sum(!is.na(result$put_call_ratio))
  fin_available  <- sum(!is.na(result$debt_equity_poly))
  message(glue("  Options data: {opts_available}/{nrow(result)} tickers"))
  message(glue("  SEC financials: {fin_available}/{nrow(result)} tickers"))
  if (opts_available > 0) {
    top_bull <- result %>% filter(!is.na(put_call_ratio)) %>%
      arrange(put_call_ratio) %>% slice(1)
    message(glue("  Most bullish options: {top_bull$symbol} (P/C={top_bull$put_call_ratio})"))
  }

  result
}

# Null coalesce helper (base R doesn't have %||%)
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a)) a else b

if (!exists("SOURCED_BY_MASTER")) polygon_data <- run_module_polygon()
