# =============================================================================
# MODULE 3 — SHORT INTEREST, MACRO DATA, EARNINGS CALENDAR, NEWS
# =============================================================================
library(tidyquant); library(dplyr); library(tidyr); library(readr)
library(glue); library(httr); library(jsonlite); library(lubridate)

FMP_KEY  <- "WEe4bM0zNn8UagrZtzyijnANOJa6qrBK"
FMP_BASE <- "https://financialmodelingprep.com/api/v3"

# ── Short Interest ──────────────────────────────────────────────────────────
get_short_interest <- function(tickers) {
  message("Pulling short interest data...")
  results <- list()
  for (i in seq_along(tickers)) {
    t <- tickers[i]
    if (i%%20==0) message(glue("  Short interest: {i}/{length(tickers)}..."))
    tryCatch({
      resp <- GET(glue("{FMP_BASE}/key-metrics-ttm/{t}?apikey={FMP_KEY}"))
      row  <- tibble(symbol=t, short_percent_float=NA_real_,
                     short_ratio=NA_real_, shares_short=NA_real_,
                     short_trend="Unknown")
      if (status_code(resp)==200) {
        raw <- fromJSON(content(resp,as="text",encoding="UTF-8"))
        if (length(raw)>0) {
          d <- as_tibble(raw) %>% slice(1)
          if ("daysOfInventoryOnHandTTM" %in% names(d)) row$short_ratio <- d$daysOfInventoryOnHandTTM[1]
        }
      }
      results[[i]] <- row
      Sys.sleep(0.2)
    }, error=function(e) {
      results[[i]] <<- tibble(symbol=t, short_percent_float=NA_real_,
                               short_ratio=NA_real_, shares_short=NA_real_, short_trend="Unknown")
    })
  }
  bind_rows(results)
}

# ── Macro Data via FRED through tidyquant ───────────────────────────────────
get_macro_data <- function() {
  message("Pulling macro data (FRED)...")
  series <- c(
    "DGS10"  = "10Y Treasury",
    "DGS2"   = "2Y Treasury",
    "DGS3MO" = "3M Treasury",
    "CPIAUCSL"= "CPI",
    "UNRATE" = "Unemployment",
    "FEDFUNDS"= "Fed Funds Rate",
    "T10Y2Y" = "Yield Curve Spread"
  )
  macro_list <- list()
  for (sym in names(series)) {
    tryCatch({
      d <- tq_get(sym, get="economic.data", from=Sys.Date()-730, to=Sys.Date()) %>%
        mutate(series=series[[sym]], ticker=sym)
      macro_list[[sym]] <- d
      Sys.sleep(0.3)
    }, error=function(e) message(glue("  Skipped {sym}: {e$message}")))
  }
  bind_rows(macro_list)
}

# ── Earnings Calendar ───────────────────────────────────────────────────────
get_earnings_calendar <- function() {
  message("Pulling earnings calendar from FMP...")
  from_dt <- format(Sys.Date(), "%Y-%m-%d")
  to_dt   <- format(Sys.Date()+60, "%Y-%m-%d")
  tryCatch({
    url  <- glue("{FMP_BASE}/earning_calendar?from={from_dt}&to={to_dt}&apikey={FMP_KEY}")
    resp <- GET(url)
    if (status_code(resp)!=200) return(tibble())
    raw  <- fromJSON(content(resp,as="text",encoding="UTF-8"))
    if (length(raw)==0) return(tibble())
    as_tibble(raw) %>%
      select(any_of(c("symbol","date","eps","epsEstimated","revenue","revenueEstimated","time"))) %>%
      mutate(date=as.Date(date)) %>%
      filter(!is.na(date)) %>%
      arrange(date)
  }, error=function(e) { message("Earnings calendar failed: ", e$message); tibble() })
}

# ── Stock News ──────────────────────────────────────────────────────────────
get_market_news <- function(n=50) {
  message("Pulling market news...")
  tryCatch({
    url  <- glue("{FMP_BASE}/stock_news?limit={n}&apikey={FMP_KEY}")
    resp <- GET(url)
    if (status_code(resp)!=200) return(tibble())
    raw  <- fromJSON(content(resp,as="text",encoding="UTF-8"))
    if (length(raw)==0) return(tibble())
    as_tibble(raw) %>%
      select(any_of(c("symbol","publishedDate","title","text","url","site","image"))) %>%
      mutate(publishedDate=as.POSIXct(publishedDate)) %>%
      arrange(desc(publishedDate))
  }, error=function(e) { message("News failed: ", e$message); tibble() })
}

# ── Sector Performance ──────────────────────────────────────────────────────
get_sector_performance <- function() {
  message("Pulling sector performance...")
  tryCatch({
    url  <- glue("{FMP_BASE}/sectors-performance?apikey={FMP_KEY}")
    resp <- GET(url)
    if (status_code(resp)!=200) return(tibble())
    raw  <- fromJSON(content(resp,as="text",encoding="UTF-8"))
    as_tibble(raw)
  }, error=function(e) tibble())
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
                               isTRUE(fundamentals_improving), 1.20, 1.0),
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

  short  <- get_short_interest(tickers)
  squeeze <- build_squeeze_score(short, fund_data)
  macro  <- get_macro_data()
  earn   <- get_earnings_calendar()
  news   <- get_market_news(50)
  sector <- get_sector_performance()

  if (!dir.exists("data")) dir.create("data", recursive=TRUE)
  write_csv(squeeze, "data/squeeze_scored.csv")
  write_csv(macro,   "data/macro_data.csv")
  write_csv(earn,    "data/earnings_calendar.csv")
  write_csv(news,    "data/market_news.csv")
  write_csv(sector,  "data/sector_performance.csv")

  message("Saved: squeeze, macro, earnings, news, sector data")
  list(squeeze=squeeze, macro=macro, earnings=earn, news=news, sector=sector)
}

if (!exists("SOURCED_BY_MASTER")) module3_data <- run_module3()
