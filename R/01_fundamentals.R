# =============================================================================
# MODULE 1 — FUNDAMENTALS  (Yahoo Finance + Alpha Vantage enrichment)
#
# Alpha Vantage fills the fields Yahoo doesn't return:
#   pb_ratio, debt_equity, roe, profit_margin, market_cap, revenue,
#   earningsGrowth, pe_forward, analyst targets, beta
#
# Rate-limit strategy (free tier = 25 calls/day, 5/min):
#   - Maintain data/av_cache.csv with a last_fetched timestamp per ticker
#   - Each run refreshes the 20 stalest tickers (>7 days old first, then oldest)
#   - Sleep 13 s between calls to stay under 5/min
#   - Full 50-ticker universe rotates in ~3 days
# =============================================================================
library(quantmod); library(dplyr); library(readr); library(httr); library(jsonlite)

AV_KEY   <- Sys.getenv("AV_KEY", "4GQPMHS72JE36TT0")
AV_BASE  <- "https://www.alphavantage.co/query"
AV_CACHE <- "data/av_cache.csv"
AV_MAX_PER_RUN <- 20          # stay well under 25/day
AV_TTL_DAYS    <- 7           # refresh each ticker every 7 days
AV_SLEEP_SEC   <- 13          # 13 s = ~4.6 calls/min (under 5/min limit)

# ── Alpha Vantage OVERVIEW fetcher ───────────────────────────────────────────
fetch_av_overview <- function(sym) {
  tryCatch({
    resp <- GET(AV_BASE, query = list(
      `function` = "OVERVIEW",
      symbol     = sym,
      apikey     = AV_KEY
    ))
    if (status_code(resp) != 200) return(NULL)
    raw <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
    if (length(raw) == 0 || is.null(raw$Symbol)) return(NULL)

    safe_num <- function(x) suppressWarnings(as.numeric(x))

    tibble(
      symbol             = sym,
      last_fetched       = Sys.Date(),
      company_av         = raw$Name,
      sector_av          = raw$Sector,
      industry           = raw$Industry,
      market_cap         = safe_num(raw$MarketCapitalization),
      pe_ratio           = safe_num(raw$PERatio),
      pe_forward         = safe_num(raw$ForwardPE),
      pb_ratio           = safe_num(raw$PriceToBookRatio),
      eps                = safe_num(raw$EPS),
      eps_forward        = safe_num(raw$DilutedEPSTTM),
      roe                = safe_num(raw$ReturnOnEquityTTM),
      roa                = safe_num(raw$ReturnOnAssetsTTM),
      profit_margin      = safe_num(raw$ProfitMargin),
      operating_margin   = safe_num(raw$OperatingMarginTTM),
      revenue            = safe_num(raw$RevenueTTM),
      revenue_per_share  = safe_num(raw$RevenuePerShareTTM),
      ebitda             = safe_num(raw$EBITDA),
      ev_ebitda          = safe_num(raw$EVToEBITDA),
      earningsGrowth     = safe_num(raw$QuarterlyEarningsGrowthYOY),
      revenue_growth     = safe_num(raw$QuarterlyRevenueGrowthYOY),
      beta_av            = safe_num(raw$Beta),
      analyst_target     = safe_num(raw$AnalystTargetPrice),
      analyst_strong_buy = safe_num(raw$AnalystRatingStrongBuy),
      analyst_buy        = safe_num(raw$AnalystRatingBuy),
      analyst_hold       = safe_num(raw$AnalystRatingHold),
      analyst_sell       = safe_num(raw$AnalystRatingSell),
      analyst_strong_sell= safe_num(raw$AnalystRatingStrongSell),
      shares_outstanding = safe_num(raw$SharesOutstanding),
      dividend_yield     = safe_num(raw$DividendYield),
      peg_ratio          = safe_num(raw$PEGRatio),
      ev_revenue         = safe_num(raw$EVToRevenue),
      book_value         = safe_num(raw$BookValue),
      high_52w_av        = safe_num(raw$`52WeekHigh`),
      low_52w_av         = safe_num(raw$`52WeekLow`)
    )
  }, error = function(e) {
    message("  AV error for ", sym, ": ", e$message)
    NULL
  })
}

# ── Load or initialise AV cache ──────────────────────────────────────────────
load_av_cache <- function() {
  if (file.exists(AV_CACHE)) {
    cache <- read_csv(AV_CACHE, show_col_types = FALSE) %>%
      mutate(last_fetched = as.Date(last_fetched))
    return(cache)
  }
  tibble(symbol = character(), last_fetched = as.Date(character()))
}

# ── Decide which tickers to refresh this run ────────────────────────────────
tickers_to_refresh <- function(tickers, cache) {
  cutoff <- Sys.Date() - AV_TTL_DAYS
  in_cache <- tickers[tickers %in% cache$symbol]
  stale    <- cache %>% filter(symbol %in% in_cache, last_fetched < cutoff) %>%
    arrange(last_fetched) %>% pull(symbol)
  missing  <- tickers[!tickers %in% cache$symbol]
  # Priority: missing first, then oldest stale
  priority <- unique(c(missing, stale))
  head(priority, AV_MAX_PER_RUN)
}

# ── Main module ──────────────────────────────────────────────────────────────
run_module1 <- function(tickers = NULL) {
  message("\n\n=== MODULE 1: FUNDAMENTALS (Yahoo + Alpha Vantage) ===\n")

  if (is.null(tickers)) {
    tickers <- c("AAPL","MSFT","NVDA","AMZN","GOOGL","META","TSLA","JPM","V","UNH",
                 "XOM","LLY","JNJ","WMT","MA","PG","HD","MRK","ORCL","BAC",
                 "ABBV","KO","PEP","AVGO","CVX","COST","MCD","TMO","CRM","NFLX",
                 "ACN","LIN","DHR","TXN","NEE","PM","MS","RTX","AMGN","HON",
                 "UPS","QCOM","IBM","CAT","GE","INTU","SPGI","AMD","ISRG","BLK")
  }

  if (!dir.exists("data")) dir.create("data", recursive = TRUE)
  if (!dir.exists("app"))  dir.create("app",  recursive = TRUE)

  # ── Step 1: Yahoo Finance baseline (price, 52w range, basic PE/EPS) ────────
  get_yahoo_base <- function(sym) {
    tryCatch({
      env <- new.env()
      suppressWarnings(getSymbols(sym, src = "yahoo", env = env, auto.assign = TRUE))
      px    <- Cl(env[[sym]])
      price <- as.numeric(last(px))
      hi52  <- as.numeric(max(px, na.rm = TRUE))
      lo52  <- as.numeric(min(px, na.rm = TRUE))
      q     <- tryCatch(getQuote(sym), error = function(e) NULL)
      pe_y  <- if (!is.null(q) && "P/E Ratio" %in% names(q))
                 suppressWarnings(as.numeric(q[["P/E Ratio"]])) else NA_real_
      eps_y <- if (!is.null(q) && "EPS" %in% names(q))
                 suppressWarnings(as.numeric(q[["EPS"]])) else NA_real_

      tibble(symbol = sym, price = round(price, 2),
             high_52w = round(hi52, 2), low_52w = round(lo52, 2),
             pe_yahoo = pe_y, eps_yahoo = eps_y)
    }, error = function(e) {
      message("  Yahoo skip ", sym, ": ", e$message)
      tibble(symbol = sym, price = NA_real_, high_52w = NA_real_,
             low_52w = NA_real_, pe_yahoo = NA_real_, eps_yahoo = NA_real_)
    })
  }

  message("Fetching Yahoo Finance baseline for ", length(tickers), " stocks...")
  yahoo_data <- bind_rows(lapply(tickers, function(s) {
    message("  Yahoo: ", s)
    get_yahoo_base(s)
  }))

  # ── Step 2: Alpha Vantage enrichment (rotating cache) ─────────────────────
  message("\nLoading Alpha Vantage cache...")
  av_cache <- load_av_cache()
  to_fetch  <- tickers_to_refresh(tickers, av_cache)

  if (length(to_fetch) > 0) {
    message(glue::glue("Fetching Alpha Vantage OVERVIEW for {length(to_fetch)} tickers ",
                       "(up to {AV_MAX_PER_RUN}/run, 7-day cache)..."))
    fresh <- list()
    for (i in seq_along(to_fetch)) {
      sym <- to_fetch[i]
      message(glue::glue("  AV [{i}/{length(to_fetch)}]: {sym}"))
      result <- fetch_av_overview(sym)
      if (!is.null(result)) fresh[[sym]] <- result
      if (i < length(to_fetch)) Sys.sleep(AV_SLEEP_SEC)
    }

    if (length(fresh) > 0) {
      fresh_df <- bind_rows(fresh)
      # Update cache: remove old rows for fetched tickers, append fresh
      av_cache <- av_cache %>%
        filter(!symbol %in% fresh_df$symbol) %>%
        bind_rows(fresh_df) %>%
        arrange(symbol)
      write_csv(av_cache, AV_CACHE)
      message(glue::glue("AV cache updated: {nrow(av_cache)} tickers cached."))
    }
  } else {
    message("All tickers fresh in AV cache — no fetches needed.")
  }

  # ── Step 3: Merge Yahoo + AV cache ────────────────────────────────────────
  message("\nMerging Yahoo + Alpha Vantage data...")
  av_latest <- av_cache %>% filter(symbol %in% tickers)

  merged <- yahoo_data %>%
    left_join(av_latest, by = "symbol") %>%
    mutate(
      # Prefer AV where available, fall back to Yahoo
      pe_ratio      = dplyr::coalesce(pe_ratio,      pe_yahoo),
      eps           = dplyr::coalesce(eps,            eps_yahoo),
      high_52w      = dplyr::coalesce(high_52w_av,    high_52w),
      low_52w       = dplyr::coalesce(low_52w_av,     low_52w),

      # Fundamental scores using real data now (not NA fallbacks)
      pe_score = dplyr::case_when(
        is.na(pe_ratio)    ~ 50,
        pe_ratio < 0       ~ 20,   # negative earnings
        pe_ratio < 15      ~ 90,
        pe_ratio < 20      ~ 75,
        pe_ratio < 25      ~ 60,
        pe_ratio < 35      ~ 45,
        TRUE               ~ 25),

      pb_score = dplyr::case_when(
        is.na(pb_ratio)    ~ 50,
        pb_ratio < 0       ~ 20,
        pb_ratio < 1.5     ~ 90,
        pb_ratio < 3       ~ 75,
        pb_ratio < 5       ~ 60,
        pb_ratio < 10      ~ 40,
        TRUE               ~ 20),

      roe_score = dplyr::case_when(
        is.na(roe)         ~ 50,
        roe > 0.30         ~ 95,
        roe > 0.20         ~ 80,
        roe > 0.10         ~ 65,
        roe > 0.00         ~ 40,
        TRUE               ~ 15),

      margin_score = dplyr::case_when(
        is.na(profit_margin) ~ 50,
        profit_margin > 0.25 ~ 90,
        profit_margin > 0.15 ~ 75,
        profit_margin > 0.08 ~ 60,
        profit_margin > 0.00 ~ 40,
        TRUE                 ~ 20),

      # Analyst consensus score (new signal from AV)
      analyst_total = dplyr::coalesce(analyst_strong_buy, 0) +
                      dplyr::coalesce(analyst_buy,        0) +
                      dplyr::coalesce(analyst_hold,       0) +
                      dplyr::coalesce(analyst_sell,       0) +
                      dplyr::coalesce(analyst_strong_sell,0),
      analyst_score = dplyr::case_when(
        is.na(analyst_total) | analyst_total == 0 ~ 50,
        TRUE ~ round(
          (dplyr::coalesce(analyst_strong_buy, 0) * 100 +
           dplyr::coalesce(analyst_buy,        0) *  75 +
           dplyr::coalesce(analyst_hold,       0) *  50 +
           dplyr::coalesce(analyst_sell,       0) *  25 +
           dplyr::coalesce(analyst_strong_sell,0) *   0) / analyst_total, 1)),

      # Price vs analyst target upside
      analyst_upside = dplyr::case_when(
        is.na(analyst_target) | is.na(price) | price <= 0 ~ NA_real_,
        TRUE ~ (analyst_target - price) / price),

      fundamental_score = round(
        pe_score * 0.25 + pb_score * 0.20 +
        roe_score * 0.30 + margin_score * 0.15 +
        analyst_score * 0.10, 2)
    ) %>%
    select(-any_of(c("pe_yahoo", "eps_yahoo", "high_52w_av", "low_52w_av",
                      "company_av", "sector_av"))) %>%
    arrange(desc(fundamental_score))

  write_csv(merged, "data/fundamentals_scored.csv")
  write_csv(merged, "app/fundamentals_scored.csv")

  message("\nSaved: data/fundamentals_scored.csv (", nrow(merged), " stocks)")
  message("AV-enriched: ", sum(!is.na(merged$pb_ratio)), "/", nrow(merged),
          " stocks have full fundamentals")
  message("Top fundamental: ", merged$symbol[1],
          " (score=", merged$fundamental_score[1], ")")

  merged
}

if (!exists("SOURCED_BY_MASTER")) fund_data <- run_module1()
