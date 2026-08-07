# =============================================================================
# MODULE 3 — SHORT INTEREST, MACRO DATA, EARNINGS CALENDAR, NEWS
#
# Data sources:
#   Macro (FRED)      — quantmod::getSymbols(src="FRED")  (no tidyquant needed)
#   Earnings calendar — Alpha Vantage EARNINGS_CALENDAR endpoint (CSV)
#   Market news       — Alpha Vantage NEWS_SENTIMENT endpoint (JSON)
#   Sector perf       — computed from Module 1 fundamentals (self-sourced)
#   WSB trending      — ApeWisdom API (free, no key needed)
#   StockTwits        — StockTwits API (free, no key needed)
#   Short interest    — stub (no reliable free API; squeeze scores use
#                       options data from Polygon in Module 2b instead)
# =============================================================================
library(quantmod); library(dplyr); library(tidyr); library(readr)
library(glue); library(httr); library(jsonlite); library(lubridate)

AV_KEY   <- Sys.getenv("AV_KEY")
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
EARN_CACHE    <- "data/earnings_calendar.csv"
EARN_TTL_DAYS <- 3

get_earnings_calendar <- function() {
  # This endpoint returns a 3-month horizon, so refetching it daily spends a
  # scarce free-tier AV call on data that barely moves — and it was crowding
  # out the news call. Reuse the cached copy until it is EARN_TTL_DAYS old.
  if (file.exists(EARN_CACHE)) {
    cached <- tryCatch(read_csv(EARN_CACHE, show_col_types = FALSE),
                       error = function(e) tibble())
    age <- as.numeric(difftime(Sys.time(), file.mtime(EARN_CACHE), units = "days"))
    if (nrow(cached) > 0 && age < EARN_TTL_DAYS) {
      message(glue("Earnings calendar: reusing cache ({nrow(cached)} rows, {round(age,1)}d old)"))
      return(cached)
    }
  }

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
    # When the daily quota is spent AV answers this CSV endpoint with a JSON
    # notice.  Feeding that to read_csv produced an opaque `as.Date(date)`
    # failure instead of a usable message.
    if (grepl("^\\s*[{\\[]", raw_text)) {
      message("  AV earnings returned a JSON notice, not CSV: ",
              substr(gsub("\\s+", " ", raw_text), 1, 220))
      return(tibble())
    }
    df <- read_csv(raw_text, show_col_types = FALSE)
    if (nrow(df) == 0) return(tibble())
    df <- df %>%
      rename(any_of(c(
        date = "reportDate",
        epsEstimated = "estimate",
        time = "timeOfTheDay"
      )))
    if (!"date" %in% names(df)) {
      message("  AV earnings CSV has no recognisable date column. Columns: ",
              paste(names(df), collapse = ", "))
      return(tibble())
    }
    df <- df %>%
      mutate(date = as.Date(date)) %>%
      filter(!is.na(date)) %>%
      arrange(date)
    message(glue("  Earnings: {nrow(df)} upcoming reports"))
    df
  }, error = function(e) {
    message("Earnings calendar failed: ", conditionMessage(e))
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
    txt <- content(resp, as = "text", encoding = "UTF-8")
    raw <- fromJSON(txt)
    f <- raw$feed
    if (is.null(f) || !is.data.frame(f) || nrow(f) == 0) {
      # Alpha Vantage reports throttling/errors under several different keys
      # depending on tier and endpoint. Checking only $Note made every one of
      # these look like a silent empty result.
      keys <- c("Note", "Information", "Error Message")
      hit  <- FALSE
      for (k in keys) if (!is.null(raw[[k]])) {
        message("  AV ", k, ": ", substr(as.character(raw[[k]]), 1, 250)); hit <- TRUE
      }
      if (!hit) message("  AV news returned no feed. Response head: ", substr(txt, 1, 250))
      return(tibble())
    }

    # Pull nested columns defensively — ticker_sentiment/topics are ragged
    # list-columns and their shape varies per article.
    nested_first <- function(col, field, default = NA_character_) {
      if (!col %in% names(f)) return(rep(default, nrow(f)))
      vapply(f[[col]], function(x) {
        if (is.data.frame(x) && nrow(x) > 0 && field %in% names(x))
          as.character(x[[field]][1]) else default
      }, character(1))
    }
    getcol <- function(nm) if (nm %in% names(f)) as.character(f[[nm]]) else rep(NA_character_, nrow(f))

    df <- tibble(
      title           = getcol("title"),
      url             = getcol("url"),
      publishedDate   = as.POSIXct(getcol("time_published"), format = "%Y%m%dT%H%M%S"),
      source          = getcol("source"),
      summary         = getcol("summary"),
      sentiment       = getcol("overall_sentiment_label"),
      sentiment_score = suppressWarnings(as.numeric(getcol("overall_sentiment_score"))),
      symbol          = nested_first("ticker_sentiment", "ticker"),
      category        = nested_first("topics", "topic", "")
    ) %>%
      head(n) %>%
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
      # Fall back to the AV cache for sector labels.  Note we are inside the
      # branch where `sector` does NOT exist, so it cannot be coalesced against
      # itself — doing so aborted this function on every run.
      av_path <- "data/av_cache.csv"
      if (file.exists(av_path)) {
        av <- read_csv(av_path, show_col_types = FALSE)
        if ("sector_av" %in% names(av)) {
          av <- av %>% select(symbol, sector_av) %>% filter(!is.na(sector_av))
          fund <- left_join(fund, av, by = "symbol") %>%
            mutate(sector = sector_av)
        }
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

# ── StockTwits Trending (free, no API key) ─────────────────────────────────
get_stocktwits_trending <- function() {
  message("Pulling StockTwits trending symbols...")
  tryCatch({
    sym_resp <- GET("https://api.stocktwits.com/api/2/trending/symbols.json",
                    add_headers(`User-Agent` = "EdgeScreener/1.0"))
    if (status_code(sym_resp) != 200) {
      message("  StockTwits symbols returned status ", status_code(sym_resp))
      return(tibble())
    }
    sym_raw <- fromJSON(content(sym_resp, as = "text", encoding = "UTF-8"))
    s <- sym_raw$symbols
    if (is.null(s) || !is.data.frame(s) || nrow(s) == 0) return(tibble())

    # Build from atomic vectors only.  The payload carries nested data.frame
    # columns (trends/features/fundamentals) whose shape varies between calls;
    # as_tibble() on the whole object intermittently aborts with
    # "Tibble columns must have compatible sizes".
    pick <- function(col, cast = as.character) {
      if (!col %in% names(s)) return(rep(NA, nrow(s)))
      v <- s[[col]]
      if (is.list(v) || length(v) != nrow(s)) return(rep(NA, nrow(s)))
      suppressWarnings(cast(v))
    }

    symbols <- tibble(
      symbol          = pick("symbol"),
      title           = pick("title"),
      watchlist_count = pick("watchlist_count", as.numeric),
      st_rank         = pick("rank",            as.numeric),
      trending_score  = pick("trending_score",  as.numeric)
    ) %>% filter(!is.na(symbol))
    if (nrow(symbols) == 0) return(tibble())

    Sys.sleep(1)

    msg_resp <- GET("https://api.stocktwits.com/api/2/streams/trending.json",
                    add_headers(`User-Agent` = "EdgeScreener/1.0"))
    if (status_code(msg_resp) != 200) {
      message("  StockTwits stream returned status ", status_code(msg_resp))
      return(symbols %>% mutate(messages = 0L, bullish = 0L, bearish = 0L))
    }
    msg_raw <- fromJSON(content(msg_resp, as = "text", encoding = "UTF-8"))
    msgs <- msg_raw$messages
    if (is.data.frame(msgs) && nrow(msgs) > 0 && "symbols" %in% names(msgs)) {
      msg_syms <- vapply(msgs$symbols, function(x) {
        if (is.data.frame(x) && nrow(x) > 0 && "symbol" %in% names(x))
          as.character(x$symbol[1]) else NA_character_
      }, character(1))
      ent <- msgs$entities
      msg_sent <- if (is.data.frame(ent) && "sentiment" %in% names(ent)) {
        sent <- ent$sentiment
        if (is.data.frame(sent) && "basic" %in% names(sent))
          as.character(sent$basic) else rep(NA_character_, nrow(msgs))
      } else rep(NA_character_, nrow(msgs))
      if (length(msg_sent) != length(msg_syms)) msg_sent <- rep(NA_character_, length(msg_syms))

      msg_df <- tibble(msg_symbol = msg_syms, sentiment = msg_sent) %>%
        filter(!is.na(msg_symbol)) %>%
        group_by(msg_symbol) %>%
        summarize(
          messages = n(),
          bullish  = sum(sentiment == "Bullish", na.rm = TRUE),
          bearish  = sum(sentiment == "Bearish", na.rm = TRUE),
          .groups  = "drop"
        )
      symbols <- symbols %>%
        left_join(msg_df, by = c("symbol" = "msg_symbol")) %>%
        mutate(across(c(messages, bullish, bearish), ~replace_na(.x, 0L)))
    } else {
      symbols <- symbols %>% mutate(messages = 0L, bullish = 0L, bearish = 0L)
    }
    symbols <- symbols %>%
      mutate(
        sentiment_label = case_when(
          bullish > bearish * 2 ~ "Very Bullish",
          bullish > bearish     ~ "Bullish",
          bearish > bullish * 2 ~ "Very Bearish",
          bearish > bullish     ~ "Bearish",
          TRUE                  ~ "Neutral"
        ),
        fetched_at = Sys.time()
      )
    message(glue("  StockTwits: {nrow(symbols)} trending symbols"))
    symbols
  }, error = function(e) {
    message("StockTwits trending failed: ", e$message)
    tibble()
  })
}

# A transient API failure must never destroy good data that is already on disk.
# An empty fetch used to overwrite the previous day's file with a 0-row CSV, so
# one bad response blanked a panel until the next good run.  At file scope so it
# is unit-testable.
write_or_keep <- function(df, path, label = basename(path)) {
  if (nrow(df) > 0) { write_csv(df, path); return(invisible(NULL)) }
  prev <- if (file.exists(path))
    tryCatch(nrow(read_csv(path, show_col_types = FALSE)), error = function(e) 0) else 0
  if (prev > 0) {
    message(glue("  {label}: fetch empty — keeping previous {prev} rows on disk"))
  } else {
    write_csv(df, path)
  }
  invisible(NULL)
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
  # News before earnings: both draw on the same Alpha Vantage daily quota that
  # Module 1 has already partly consumed, and news is the more time-sensitive
  # of the two (the earnings calendar returns a 3-month horizon).
  news    <- get_market_news(50)
  earn    <- get_earnings_calendar()
  sector  <- get_sector_performance()
  wsb     <- get_wsb_trending(50)
  stwits  <- get_stocktwits_trending()

  # ── Data validation ──────────────────────────────────────────────────────
  if (nrow(earn) == 0)   warning("WARN: Earnings calendar is EMPTY — AV API may be rate-limited")
  if (nrow(news) == 0)   warning("WARN: Market news is EMPTY — AV API may be rate-limited")
  if (nrow(macro) == 0)  warning("WARN: Macro data is EMPTY — FRED may be unreachable")
  if (nrow(wsb) == 0)    warning("WARN: WSB trending is EMPTY — ApeWisdom may be down")
  if (nrow(stwits) == 0) warning("WARN: StockTwits trending is EMPTY — API may be down")

  if (!dir.exists("data")) dir.create("data", recursive=TRUE)

  write_csv(squeeze, "data/squeeze_scored.csv")
  write_csv(macro,   "data/macro_data.csv")
  write_or_keep(earn,   "data/earnings_calendar.csv",   "earnings")
  write_or_keep(news,   "data/market_news.csv",         "news")
  write_or_keep(sector, "data/sector_performance.csv",  "sector")
  write_or_keep(wsb,    "data/wsb_trending.csv",        "wsb")
  write_or_keep(stwits, "data/stocktwits_trending.csv", "stocktwits")

  message(glue("Saved: squeeze({nrow(squeeze)}), macro({nrow(macro)}), ",
               "earnings({nrow(earn)}), news({nrow(news)}), ",
               "sector({nrow(sector)}), wsb({nrow(wsb)}), ",
               "stocktwits({nrow(stwits)})"))
  list(squeeze=squeeze, macro=macro, earnings=earn, news=news,
       sector=sector, wsb=wsb, stocktwits=stwits)
}

if (!exists("SOURCED_BY_MASTER")) module3_data <- run_module3()
