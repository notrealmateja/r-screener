# =============================================================================
# MODULE 3 — SHORT INTEREST, MACRO DATA, EARNINGS CALENDAR, NEWS
#
# Data sources:
#   Macro (FRED)      — quantmod::getSymbols(src="FRED")  (no tidyquant needed)
#   Earnings calendar — Alpha Vantage EARNINGS_CALENDAR endpoint (CSV)
#   Market news       — Alpha Vantage NEWS_SENTIMENT endpoint (JSON)
#   Sector perf       — computed from Module 1 fundamentals (self-sourced)
#   WSB trending      — ApeWisdom API (free, no key needed)
#   Short interest    — stub (no reliable free API; squeeze scores use
#                       options data from Polygon in Module 2b instead)
# =============================================================================
library(quantmod); library(dplyr); library(tidyr); library(readr)
library(glue); library(httr); library(jsonlite); library(lubridate)

AV_KEY   <- Sys.getenv("AV_KEY", "4GQPMHS72JE36TT0")
AV_BASE  <- "https://www.alphavantage.co/query"

# ── Short Interest (stub — no reliable free source) ────────────────────────
get_short_interest <- function(tickers) {
  message("Building short interest stubs (no reliable free API)...")
  tibble(
    symbol             = tickers,
    short_percent_float = NA_real_,
    short_ratio        = NA_real_,
    shares_short       = NA_real_,
    short_trend        = "Unknown"
  )
}

# ── Macro Data via FRED (quantmod, not tidyquant) ──────────────────────────
get_macro_data <- function() {
  message("Pulling macro data (FRED via quantmod)...")
  series <- c(
    "DGS10"   = "10Y Treasury",
    "DGS2"    = "2Y Treasury",
    "DGS3MO"  = "3M Treasury",
    "CPIAUCSL" = "CPI",
    "UNRATE"  = "Unemployment",
    "FEDFUNDS" = "Fed Funds Rate",
    "T10Y2Y"  = "Yield Curve Spread"
  )
  macro_list <- list()
  for (sym in names(series)) {
    tryCatch({
      getSymbols(sym, src = "FRED", auto.assign = TRUE, env = environment())
      xts_data <- get(sym, envir = environment())
      d <- data.frame(date = index(xts_data), price = as.numeric(coredata(xts_data))) %>%
        filter(date >= Sys.Date() - 730) %>%
        mutate(series = series[[sym]], ticker = sym, value = price) %>%
        as_tibble()
      macro_list[[sym]] <- d
      Sys.sleep(0.3)
    }, error = function(e) message(glue("  Skipped {sym}: {e$message}")))
  }
  result <- bind_rows(macro_list)
  message(glue("  FRED: {nrow(result)} rows across {length(macro_list)} series"))
  result
}

# ── Earnings Calendar (Alpha Vantage — CSV endpoint) ───────────────────────
get_earnings_calendar <- function() {
  message("Pulling earnings calendar (Alpha Vantage)...")
  tryCatch({
    url <- glue("{AV_BASE}?function=EARNINGS_CALENDAR&horizon=3month&apikey={AV_KEY}")
    resp <- GET(url)
    if (status_code(resp) != 200) {
      message("  AV earnings returned status ", status_code(resp))
      return(tibble())
    }
    raw_text <- content(resp, as = "text", encoding = "UTF-8")
    if (nchar(raw_text) < 20) {
      message("  AV earnings returned empty response")
      return(tibble())
    }
    df <- read_csv(raw_text, show_col_types = FALSE)
    if (nrow(df) == 0) return(tibble())
    df <- df %>%
      rename(any_of(c(
        date = "reportDate",
        epsEstimated = "estimate",
        time = "timeOfTheDay"
      ))) %>%
      mutate(date = as.Date(date)) %>%
      filter(!is.na(date)) %>%
      arrange(date)
    message(glue("  Earnings: {nrow(df)} upcoming reports"))
    df
  }, error = function(e) {
    message("Earnings calendar failed: ", e$message)
    tibble()
  })
}

# ── Stock News (Alpha Vantage NEWS_SENTIMENT) ──────────────────────────────
get_market_news <- function(n = 50) {
  message("Pulling market news (Alpha Vantage)...")
  tryCatch({
    url <- glue("{AV_BASE}?function=NEWS_SENTIMENT&limit={n}&apikey={AV_KEY}")
    resp <- GET(url)
    if (status_code(resp) != 200) {
      message("  AV news returned status ", status_code(resp))
      return(tibble())
    }
    raw <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
    if (is.null(raw$feed) || length(raw$feed) == 0) {
      if (!is.null(raw$Note)) message("  AV rate limit: ", raw$Note)
      return(tibble())
    }
    df <- as_tibble(raw$feed) %>%
      head(n) %>%
      transmute(
        title           = title,
        url             = url,
        publishedDate   = as.POSIXct(time_published, format = "%Y%m%dT%H%M%S"),
        source          = source,
        summary         = summary,
        sentiment       = overall_sentiment_label,
        sentiment_score = overall_sentiment_score,
        symbol          = sapply(ticker_sentiment, function(ts) {
          if (is.data.frame(ts) && nrow(ts) > 0) ts$ticker[1] else NA_character_
        }),
        category        = sapply(topics, function(tp) {
          if (is.data.frame(tp) && nrow(tp) > 0) tp$topic[1] else ""
        })
      ) %>%
      arrange(desc(publishedDate))
    message(glue("  News: {nrow(df)} articles"))
    df
  }, error = function(e) {
    message("News failed: ", e$message)
    tibble()
  })
}

# ── Sector Performance (computed from fundamentals data) ───────────────────
get_sector_performance <- function() {
  message("Computing sector performance from fundamentals...")
  tryCatch({
    fund_path <- "data/fundamentals_scored.csv"
    mom_path  <- "data/momentum_scored.csv"
    if (!file.exists(fund_path)) return(tibble())
    fund <- read_csv(fund_path, show_col_types = FALSE)
    if (file.exists(mom_path)) {
      mom <- read_csv(mom_path, show_col_types = FALSE) %>%
        select(symbol, any_of(c("ret_1m", "ret_3m")))
      fund <- left_join(fund, mom, by = "symbol")
    }
    if (!"sector" %in% names(fund)) {
      # Try AV cache for sector info
      av_path <- "data/av_cache.csv"
      if (file.exists(av_path)) {
        av <- read_csv(av_path, show_col_types = FALSE) %>%
          select(symbol, sector_av) %>% filter(!is.na(sector_av))
        fund <- left_join(fund, av, by = "symbol") %>%
          mutate(sector = coalesce(sector, sector_av))
      }
    }
    if (!"ret_1m" %in% names(fund) || !"sector" %in% names(fund)) return(tibble())
    fund %>%
      filter(!is.na(sector), !is.na(ret_1m)) %>%
      group_by(sector) %>%
      summarize(
        changesPercentage = round(mean(ret_1m * 100, na.rm = TRUE), 2),
        n_stocks = n(),
        .groups = "drop"
      ) %>%
      arrange(changesPercentage)
  }, error = function(e) {
    message("Sector performance failed: ", e$message)
    tibble()
  })
}

# ── WSB / Reddit Trending (ApeWisdom) ──────────────────────────────────
get_wsb_trending <- function(n = 50) {
  message("Pulling WallStreetBets trending stocks (ApeWisdom)...")
  tryCatch({
    url <- "https://apewisdom.io/api/v1.0/filter/wallstreetbets/page/1"
    resp <- GET(url, add_headers(`User-Agent` = "EdgeScreener/1.0"))
    if (status_code(resp) != 200) {
      message("  WSB API returned status ", status_code(resp))
      return(tibble())
    }
    raw <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
    if (length(raw$results) == 0) return(tibble())
    as_tibble(raw$results) %>%
      head(n) %>%
      mutate(
        mentions_chg = mentions - mentions_24h_ago,
        rank_chg     = rank_24h_ago - rank,
        momentum     = case_when(
          rank_chg >= 5  ~ "Surging",
          rank_chg >= 2  ~ "Rising",
          rank_chg <= -5 ~ "Falling",
          rank_chg <= -2 ~ "Fading",
          TRUE           ~ "Steady"
        )
      )
  }, error = function(e) {
    message("WSB trending failed: ", e$message)
    tibble()
  })
}

# ── Squeeze Scoring ─────────────────────────────────────────────────────────
build_squeeze_score <- function(short_data, fund_data) {
  message("Building squeeze scores...")
  improving <- fund_data %>%
    mutate(
      rev_up    = FALSE,
      earn_up   = FALSE,
      margin_ok = FALSE,
      improvement_count = as.integer(rev_up)+as.integer(earn_up)+as.integer(margin_ok),
      fundamentals_improving = improvement_count >= 2
    ) %>%
    select(symbol, improvement_count, fundamentals_improving)
  short_data %>%
    left_join(improving, by="symbol") %>%
    mutate(
      sf_score   = ifelse(is.na(short_percent_float), 50, percent_rank(short_percent_float)*100),
      dtc_score  = ifelse(is.na(short_ratio), 50, percent_rank(short_ratio)*100),
      fi_score   = ifelse(is.na(improvement_count), 0, (improvement_count/3)*100),
      st_score   = case_when(short_trend=="Decreasing"~80, short_trend=="Increasing"~20, TRUE~40),
      squeeze_score_raw = sf_score*0.25 + dtc_score*0.20 + fi_score*0.40 + st_score*0.15,
      squeeze_mult  = ifelse(!is.na(short_percent_float) & short_percent_float>0.10 &
                               !is.na(fundamentals_improving) & fundamentals_improving, 1.20, 1.0),
      squeeze_score = pmin(squeeze_score_raw * squeeze_mult, 100),
      squeeze_tier  = case_when(
        squeeze_score>=80 ~ "High Conviction",
        squeeze_score>=60 ~ "Watch List",
        squeeze_score>=40 ~ "Low Signal",
        TRUE              ~ "No Signal"),
      short_float_pct = ifelse(is.na(short_percent_float), "N/A",
                               paste0(round(short_percent_float*100,1),"%")),
      across(where(is.numeric), ~round(.,2))
    ) %>%
    arrange(desc(squeeze_score))
}

run_module3 <- function(tickers=NULL) {
  message("\n=== MODULE 3: SHORT INTEREST, MACRO, EARNINGS, NEWS ===\n")
  if (is.null(tickers)) {
    fund <- read_csv("data/fundamentals_scored.csv", show_col_types=FALSE)
    tickers <- fund$symbol
  }
  fund_data <- read_csv("data/fundamentals_scored.csv", show_col_types=FALSE)

  short   <- get_short_interest(tickers)
  squeeze <- build_squeeze_score(short, fund_data)
  macro   <- get_macro_data()
  earn    <- get_earnings_calendar()
  news    <- get_market_news(50)
  sector  <- get_sector_performance()
  wsb     <- get_wsb_trending(50)

  # ── Data validation ──────────────────────────────────────────────────────
  if (nrow(earn) == 0)   warning("WARN: Earnings calendar is EMPTY — AV API may be rate-limited")
  if (nrow(news) == 0)   warning("WARN: Market news is EMPTY — AV API may be rate-limited")
  if (nrow(macro) == 0)  warning("WARN: Macro data is EMPTY — FRED may be unreachable")
  if (nrow(wsb) == 0)    warning("WARN: WSB trending is EMPTY — ApeWisdom may be down")

  if (!dir.exists("data")) dir.create("data", recursive=TRUE)
  write_csv(squeeze, "data/squeeze_scored.csv")
  write_csv(macro,   "data/macro_data.csv")
  write_csv(earn,    "data/earnings_calendar.csv")
  write_csv(news,    "data/market_news.csv")
  write_csv(sector,  "data/sector_performance.csv")
  write_csv(wsb,     "data/wsb_trending.csv")

  message(glue("Saved: squeeze({nrow(squeeze)}), macro({nrow(macro)}), ",
               "earnings({nrow(earn)}), news({nrow(news)}), ",
               "sector({nrow(sector)}), wsb({nrow(wsb)})"))
  list(squeeze=squeeze, macro=macro, earnings=earn, news=news,
       sector=sector, wsb=wsb)
}

if (!exists("SOURCED_BY_MASTER")) module3_data <- run_module3()
