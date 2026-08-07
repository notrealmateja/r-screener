# =============================================================================
# MODULE 1 — FUNDAMENTALS  (Yahoo Finance + Alpha Vantage enrichment)
#
# Alpha Vantage fills the fields Yahoo doesn't return:
#   pb_ratio, debt_equity, roe, profit_margin, market_cap, revenue,
#   earningsGrowth, pe_forward, analyst targets, beta
#
# Rate-limit strategy (free tier = 25 calls/day, 5/min):
#   - Maintain data/av_cache.csv with a last_fetched timestamp per ticker
#   - Each run refreshes the 22 stalest tickers (missing first, then oldest)
#   - Sleep 13 s between calls to stay under 5/min
#   - Full 195-ticker universe rotates in ~9 days, so newly added tickers have
#     no fundamentals until their turn comes up
# =============================================================================
library(quantmod); library(dplyr); library(readr); library(httr); library(jsonlite)

AV_KEY   <- Sys.getenv("AV_KEY")
AV_BASE  <- "https://www.alphavantage.co/query"
AV_CACHE <- "data/av_cache.csv"
# Free tier is 25 calls/day.  Module 3 needs 1 for news plus 1 for the earnings
# calendar every 3rd day, so 22 here leaves headroom.  At 195 tickers a full
# rotation takes ~9 days, so the TTL is set to match — a shorter TTL would just
# mark the whole universe permanently stale.
AV_MAX_PER_RUN <- 22
AV_TTL_DAYS    <- 10
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
    tickers <- c(
    # Mega / large cap (50)
      "AAPL", "MSFT", "NVDA", "AMZN", "GOOGL", "META", "TSLA", "JPM", "V", "UNH",
      "XOM", "LLY", "JNJ", "WMT", "MA", "PG", "HD", "MRK", "ORCL", "BAC",
      "ABBV", "KO", "PEP", "AVGO", "CVX", "COST", "MCD", "TMO", "CRM", "NFLX",
      "ACN", "LIN", "DHR", "TXN", "NEE", "PM", "MS", "RTX", "AMGN", "HON",
      "UPS", "QCOM", "IBM", "CAT", "GE", "INTU", "SPGI", "AMD", "ISRG", "BLK",
    # Mid cap (77)
      "ETSY", "ROKU", "DKNG", "PINS", "SNAP", "TWLO", "ZM", "DOCU", "OKTA", "NET",
      "DDOG", "CRWD", "ZS", "MDB", "TEAM", "HUBS", "VEEV", "WDAY", "PANW", "FTNT",
      "ANET", "KEYS", "GRMN", "TER", "MPWR", "SWKS", "QRVO", "MCHP", "ON", "ENTG",
      "RJF", "IBKR", "TROW", "NTRS", "ZION", "WAL", "WBS", "FHN", "CFR", "NTRA",
      "ALNY", "BMRN", "INCY", "JAZZ", "NBIX", "SRPT", "HALO", "MEDP", "AXON", "HUBB",
      "NDSN", "GGG", "MLI", "ATI", "CRS", "MTZ", "EXP", "WTS", "CROX", "DECK",
      "ONON", "FIVE", "OLLI", "DKS", "TXRH", "WING", "PLNT", "DUOL", "CAVA", "TOST",
      "RBLX", "U", "AFRM", "UPST", "SOFI", "HOOD", "COIN",
    # Small cap - these make the Unicorn screen meaningful (68)
      "AMPL", "ASAN", "BRZE", "FROG", "PD", "APPN", "VERX", "CXM", "ALRM", "INTA",
      "DOCN", "FSLY", "BBAI", "SOUN", "IONQ", "RGTI", "QBTS", "AI", "LSCC", "AMKR",
      "KRYS", "ANAB", "CRNX", "IDYA", "KYMR", "OLMA", "PTGX", "RXRX", "SANA", "VERA",
      "XENE", "ARWR", "IONS", "ACAD", "STRL", "IESC", "ROAD", "PRIM", "WLDN", "ORN",
      "LMB", "NPWR", "MYRG", "LOVE", "PRPL", "BARK", "HNST", "FIGS", "TDUP", "ARHS",
      "SG", "YETI", "CALM", "JBSS", "SENEA", "FIZZ", "UTZ", "BRCC", "CELH", "VITL",
      "SMPL", "MITK", "YEXT", "CGEM", "ERAS", "ZNTL", "RARE", "ALGM"
    )
  }

  if (!dir.exists("data")) dir.create("data", recursive = TRUE)
  if (!dir.exists("app"))  dir.create("app",  recursive = TRUE)

  # ── Step 1: Yahoo Finance baseline (price, 52w range, basic PE/EPS) ────────
  get_yahoo_base <- function(sym) {
    tryCatch({
      env <- new.env()
      suppressWarnings(getSymbols(sym, src = "yahoo", env = env, auto.assign = TRUE))
      px    <- Cl(env[[sym]])
      tibble(symbol   = sym,
             price    = round(as.numeric(last(px)), 2),
             high_52w = round(as.numeric(max(px, na.rm = TRUE)), 2),
             low_52w  = round(as.numeric(min(px, na.rm = TRUE)), 2))
    }, error = function(e) {
      message("  Yahoo skip ", sym, ": ", e$message)
      tibble(symbol = sym, price = NA_real_,
             high_52w = NA_real_, low_52w = NA_real_)
    })
  }

  message("Fetching Yahoo Finance baseline for ", length(tickers), " stocks...")
  yahoo_data <- bind_rows(lapply(tickers, function(s) {
    message("  Yahoo: ", s)
    get_yahoo_base(s)
  }))

  # Quote fields in one batched call rather than one per ticker.  The old code
  # called getQuote(sym) with default fields, which do NOT include P/E or EPS,
  # so pe_yahoo/eps_yahoo were always NA.  Market cap matters most: Alpha
  # Vantage only refreshes AV_MAX_PER_RUN tickers per run, so without this the
  # newly added names would have no market_cap for ~9 days and the Unicorn
  # screen could not evaluate them.
  message("Fetching Yahoo quote fields (market cap, P/E, EPS)...")
  fields <- yahooQF(c("Name", "Market Capitalization", "P/E Ratio", "Earnings/Share"))
  quote_rows <- list()
  chunks <- split(tickers, ceiling(seq_along(tickers) / 40))
  for (ch in chunks) {
    q <- tryCatch(getQuote(ch, what = fields), error = function(e) NULL)
    if (!is.null(q) && nrow(q) > 0) {
      quote_rows[[length(quote_rows) + 1]] <- tibble(
        symbol           = rownames(q),
        company_yahoo    = as.character(q[["Name"]]),
        market_cap_yahoo = suppressWarnings(as.numeric(q[["Market Capitalization"]])),
        pe_yahoo         = suppressWarnings(as.numeric(q[["P/E Ratio"]])),
        eps_yahoo        = suppressWarnings(as.numeric(q[["Earnings/Share"]]))
      )
    }
    Sys.sleep(0.5)
  }
  quotes <- if (length(quote_rows) > 0) bind_rows(quote_rows) else
    tibble(symbol = character(), company_yahoo = character(),
           market_cap_yahoo = numeric(), pe_yahoo = numeric(), eps_yahoo = numeric())
  message(glue::glue("  Quotes: {sum(!is.na(quotes$market_cap_yahoo))}/{length(tickers)} market cap, ",
                     "{sum(!is.na(quotes$company_yahoo))}/{length(tickers)} company name"))

  yahoo_data <- yahoo_data %>% left_join(quotes, by = "symbol")

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
    left_join(av_latest, by = "symbol")

  # av_latest can be missing these columns entirely on a cold cache, which would
  # make the coalesce below fail rather than simply fall back to Yahoo.
  # On a cold or lost av_cache.csv the join contributes no AV columns at all,
  # and the scoring block below references them directly — Module 1 aborted with
  # "object 'pb_ratio' not found".  Materialise every AV-derived column up front
  # so a missing cache degrades to neutral scores instead of killing the run.
  av_numeric <- c("market_cap","pe_ratio","pe_forward","pb_ratio","eps","eps_forward",
                  "roe","roa","profit_margin","operating_margin","revenue",
                  "revenue_per_share","ebitda","ev_ebitda","earningsGrowth",
                  "revenue_growth","beta_av","analyst_target","analyst_strong_buy",
                  "analyst_buy","analyst_hold","analyst_sell","analyst_strong_sell",
                  "shares_outstanding","dividend_yield","peg_ratio","ev_revenue",
                  "book_value","high_52w_av","low_52w_av")
  for (col in av_numeric) {
    if (!col %in% names(merged)) merged[[col]] <- NA_real_
  }
  for (col in c("company_av", "sector_av", "industry")) {
    if (!col %in% names(merged)) merged[[col]] <- NA_character_
  }

  merged <- merged %>%
    mutate(
      # Prefer AV where available, fall back to Yahoo
      pe_ratio      = dplyr::coalesce(pe_ratio,      pe_yahoo),
      eps           = dplyr::coalesce(eps,            eps_yahoo),
      market_cap    = dplyr::coalesce(market_cap,    market_cap_yahoo),

      # Carry name and sector through instead of discarding them.  These were
      # being dropped below, so the only source downstream was a hardcoded
      # 50-ticker table in Module 4 and every other name rendered blank.
      # Yahoo covers the full universe; AV is preferred where it has run.
      company       = dplyr::coalesce(company_av, company_yahoo),
      sector        = sector_av,
      high_52w      = dplyr::coalesce(high_52w_av,   high_52w),
      low_52w       = dplyr::coalesce(low_52w_av,    low_52w),

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
    select(-any_of(c("pe_yahoo", "eps_yahoo", "market_cap_yahoo", "company_yahoo",
                      "high_52w_av", "low_52w_av",
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
