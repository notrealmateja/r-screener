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
#   Per-ticker news   — Yahoo Finance RSS (free, no key, per symbol)
#   SEC filings       — EDGAR submissions API (official, free, no key)
#   Short interest    — FINRA consolidated (all venues, no key), with Yahoo
#                       used only to refine the float denominator
# =============================================================================
library(quantmod); library(dplyr); library(tidyr); library(readr)
library(glue); library(httr); library(jsonlite); library(lubridate)

AV_KEY   <- Sys.getenv("AV_KEY")
AV_BASE  <- "https://www.alphavantage.co/query"

# ── Short Interest ─────────────────────────────────────────────────────────
# Short interest used to be a stub returning all-NA on the assumption that no
# free source exists. Three do. Because the inputs were NA, every squeeze score
# defaulted to 50 and all 195 stocks read "No Signal" — real data revives that
# module.
#
# FINRA is the primary source and is strictly better than the alternatives:
#   FINRA   — every US venue, no auth, 100% universe coverage in ~5 paginated
#             calls, and carries the prior period so the trend is real
#   Nasdaq  — no auth but only covers Nasdaq-listed names (NYSE returns empty)
#   Yahoo   — the only free source for true float, but needs a cookie+crumb
#             handshake and rate-limits under load
#
# Yahoo is used solely to refine the denominator. Where it fails, shares
# outstanding stands in; float is a subset of outstanding, so that understates
# the percentage and errs toward calling a stock less squeezed, never more.
FINRA_SHORT_URL <- paste0("https://api.finra.org/data/group/otcMarket/name/",
                          "consolidatedShortInterest")

finra_post <- function(body) {
  tryCatch({
    r <- POST(FINRA_SHORT_URL, body = body, encode = "json",
              add_headers(`User-Agent` = "EdgeScreener/1.0", Accept = "application/json"),
              timeout(90))
    if (status_code(r) != 200) return(NULL)
    out <- fromJSON(content(r, as = "text", encoding = "UTF-8"))
    if (!is.data.frame(out) || nrow(out) == 0) return(NULL)
    out
  }, error = function(e) NULL)
}

# Sorting requires settlementDate as an EQUAL filter, so the newest date is
# found by pulling one liquid symbol's history and taking its maximum.
finra_latest_settlement <- function(probe = "AAPL") {
  d <- finra_post(list(limit = 5000,
        compareFilters = list(list(fieldName = "symbolCode",
                                   fieldValue = probe, compareType = "EQUAL"))))
  if (is.null(d) || !"settlementDate" %in% names(d)) return(NULL)
  max(d$settlementDate, na.rm = TRUE)
}

finra_short_bulk <- function(settle_date, page = 5000, max_pages = 8) {
  pages <- list()
  for (i in seq_len(max_pages)) {
    d <- finra_post(list(limit = page, offset = (i - 1) * page,
          compareFilters = list(list(fieldName = "settlementDate",
                                     fieldValue = settle_date, compareType = "EQUAL"))))
    if (is.null(d)) break
    pages[[length(pages) + 1]] <- d
    if (nrow(d) < page) break
  }
  if (length(pages) == 0) return(NULL)
  bind_rows(pages)
}

get_short_interest <- function(tickers, fundamentals = NULL) {
  message("Pulling short interest (FINRA consolidated)...")
  num <- function(x) suppressWarnings(as.numeric(x))

  settle <- finra_latest_settlement()
  finra <- if (!is.null(settle)) finra_short_bulk(settle) else NULL

  if (is.null(finra)) {
    message("  FINRA unavailable — short interest will be empty this run.")
    df <- tibble(symbol = tickers, shares_short = NA_real_, shares_short_prior = NA_real_,
                 short_ratio = NA_real_, float_shares = NA_real_,
                 short_percent_float = NA_real_, short_source = "none",
                 settlement_date = NA_character_)
  } else {
    message(glue("  FINRA settlement {settle}: {nrow(finra)} securities"))
    df <- tibble(symbol = tickers) %>%
      left_join(
        finra %>%
          transmute(symbol             = as.character(symbolCode),
                    shares_short       = num(currentShortPositionQuantity),
                    shares_short_prior = num(previousShortPositionQuantity),
                    short_ratio        = num(daysToCoverQuantity),
                    settlement_date    = as.character(settlementDate)) %>%
          distinct(symbol, .keep_all = TRUE),
        by = "symbol") %>%
      mutate(float_shares = NA_real_, short_percent_float = NA_real_,
             short_source = ifelse(is.na(shares_short), "none", "finra"))
  }

  # Optional: refine the denominator with Yahoo's true float. Failure here only
  # costs precision, never the metric.
  auth <- tryCatch(yahoo_handle(), error = function(e) NULL)
  if (!is.null(auth)) {
    message("  Yahoo crumb acquired — refining float where possible.")
    for (i in which(!is.na(df$shares_short))) {
      y <- short_from_yahoo(df$symbol[i], auth)
      if (!is.null(y)) {
        if (!is.na(y$float_shares))        df$float_shares[i]        <- y$float_shares
        if (!is.na(y$short_percent_float)) df$short_percent_float[i] <- y$short_percent_float
      }
      Sys.sleep(0.3)
    }
  } else {
    message("  No Yahoo crumb — using shares outstanding as the denominator.")
  }

  if (!is.null(fundamentals) && "shares_outstanding" %in% names(fundamentals)) {
    df <- df %>% left_join(
      fundamentals %>% select(symbol, shares_outstanding) %>% distinct(symbol, .keep_all = TRUE),
      by = "symbol")
  } else df$shares_outstanding <- NA_real_

  df <- df %>%
    mutate(
      short_pct_basis = case_when(
        !is.na(short_percent_float)                                               ~ "float",
        !is.na(shares_short) & !is.na(float_shares) & float_shares > 0            ~ "float",
        !is.na(shares_short) & !is.na(shares_outstanding) & shares_outstanding > 0 ~ "shares outstanding",
        TRUE                                                                      ~ "unavailable"),
      short_percent_float = case_when(
        !is.na(short_percent_float)                                               ~ short_percent_float,
        !is.na(shares_short) & !is.na(float_shares) & float_shares > 0            ~ shares_short / float_shares,
        !is.na(shares_short) & !is.na(shares_outstanding) & shares_outstanding > 0 ~ shares_short / shares_outstanding,
        TRUE                                                                      ~ NA_real_),
      # A real trend now, rather than the constant "Unknown" the stub returned
      short_trend = case_when(
        is.na(shares_short) | is.na(shares_short_prior) ~ "Unknown",
        shares_short > shares_short_prior * 1.05        ~ "Increasing",
        shares_short < shares_short_prior * 0.95        ~ "Decreasing",
        TRUE                                            ~ "Stable"))

  got <- sum(!is.na(df$shares_short)); pf <- sum(!is.na(df$short_percent_float))
  message(glue("  Short interest: {got}/{nrow(df)} tickers, {pf} with a usable percentage"))
  message(glue("  Basis: {paste(names(table(df$short_pct_basis)), table(df$short_pct_basis), sep='=', collapse=', ')}"))
  if (got == 0) warning("WARN: short interest empty - squeeze scores will be neutral")
  df
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
  # Cache age must be read from a value stored INSIDE the file. Using
  # Modification time silently disabled this cache: actions/checkout rewrites every
  # file at checkout time, so in CI the cache always measured 0 days old and the
  # calendar never refreshed once after the TTL was introduced. A file with no
  # fetched_on column predates this fix and is treated as stale.
  if (file.exists(EARN_CACHE)) {
    cached <- tryCatch(read_csv(EARN_CACHE, show_col_types = FALSE),
                       error = function(e) tibble())
    age <- if (nrow(cached) > 0 && "fetched_on" %in% names(cached)) {
      f <- suppressWarnings(max(as.Date(cached$fetched_on), na.rm = TRUE))
      if (is.finite(as.numeric(f))) as.numeric(Sys.Date() - f) else Inf
    } else Inf
    if (nrow(cached) > 0 && age < EARN_TTL_DAYS) {
      message(glue("Earnings calendar: reusing cache ({nrow(cached)} rows, {age}d old)"))
      return(cached)
    }
    if (nrow(cached) > 0)
      message(glue("Earnings cache is {ifelse(is.finite(age), paste0(age,'d'), 'un-dated')} ",
                   "— refetching."))
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
    # AV answers this endpoint with a notice rather than data when it refuses
    # the request.  Checking only for a leading { missed it: the payload can
    # arrive without JSON framing, and read_csv then shreds it character by
    # character across the expected header — the row came back as
    # "I | n | NA | o | r | m | a", i.e. "Informa...", with the lone f typed as
    # logical FALSE, which is what made as.Date() abort. Match on the notice
    # keywords in the raw text instead of on the format.
    notice <- regmatches(raw_text,
      regexpr("(?i)(information|note|error message|premium|thank you for using)",
              raw_text, perl = TRUE))
    # A notice is a notice regardless of length. The 2000-char guard existed to
    # avoid false positives on real CSV, but real CSV for this endpoint is tens
    # of thousands of characters and carries the expected header, so key off
    # that instead.
    looks_like_csv <- grepl("^\\s*symbol\\s*,", raw_text)
    if (length(notice) > 0 && !looks_like_csv) {
      message("  AV earnings returned a notice, not data: ",
              substr(gsub("\\s+", " ", raw_text), 1, 300))
      return(tibble())
    }
    if (grepl("^\\s*[{\\[]", raw_text)) {
      message("  AV earnings returned JSON, not CSV: ",
              substr(gsub("\\s+", " ", raw_text), 1, 300))
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
    # Parse defensively.  Bare as.Date() aborts two different ways on a
    # malformed response — "do not know how to convert 'x' to class Date" for a
    # logical or list column, and "character string is not in a standard
    # unambiguous format" for unparseable text — and neither is suppressible.
    # An explicit format yields NA instead, so any shape AV sends is reported
    # rather than crashing the fetch.
    df <- df %>%
      mutate(date = suppressWarnings(as.Date(as.character(date), format = "%Y-%m-%d")))
    if (all(is.na(df$date))) {
      # Log what actually arrived. Without this the failure is undiagnosable:
      # the calendar sat 17 days stale reporting only "no parseable dates",
      # which does not say whether AV sent a notice, changed the date format,
      # or returned something else entirely.
      message("  AV earnings returned ", nrow(df), " rows but no parseable dates.")
      message("    columns: ", paste(names(df), collapse = ", "))
      message("    raw response (first 400 chars): ",
              substr(gsub("\\s+", " ", raw_text), 1, 400))
      message("    columns: ", paste(names(df), collapse = ", "))
      message("    first row: ",
              paste(utils::head(unlist(lapply(df[1, ], as.character)), 8), collapse = " | "))
      # Parsed fields are unreliable here by definition, so show the response
      # verbatim rather than leaving the shape to be inferred from them.
      message("    raw response: ", substr(gsub("\\s+", " ", raw_text), 1, 300))
      return(tibble())
    }
    df <- df %>%
      filter(!is.na(date)) %>%
      arrange(date) %>%
      mutate(fetched_on = Sys.Date())   # stamps the cache so the TTL survives CI
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

# ── Per-ticker news (Yahoo Finance RSS) ────────────────────────────────────
# The Alpha Vantage news feed returns ~50 articles a day for the whole market,
# so on a typical day only about 6 of 195 stocks get any coverage at all. RSS
# is per-ticker, needs no key, and is designed for exactly this kind of polling
# — stable schema, no cookies, no blocking.
NEWS_RSS_PER_TICKER <- 6

fetch_rss_items <- function(url, max_items) {
  tryCatch({
    r <- GET(url, add_headers(`User-Agent` = "EdgeScreener/1.0"), timeout(20))
    if (status_code(r) != 200) return(NULL)
    txt <- content(r, as = "text", encoding = "UTF-8")
    if (!nzchar(txt) || !grepl("<item", txt, fixed = TRUE)) return(NULL)
    doc   <- xml2::read_xml(txt)
    items <- xml2::xml_find_all(doc, ".//item")
    if (length(items) == 0) return(NULL)
    items <- utils::head(items, max_items)
    pick <- function(node, tag) {
      v <- xml2::xml_text(xml2::xml_find_first(node, tag))
      if (length(v) == 0 || is.na(v)) NA_character_ else trimws(v)
    }
    bind_rows(lapply(items, function(it) tibble(
      title     = pick(it, "./title"),
      url       = pick(it, "./link"),
      published = pick(it, "./pubDate"),
      publisher = pick(it, "./source"))))
  }, error = function(e) NULL)
}

# Yahoo's per-ticker feed mixes in general market stories: a Nvidia earnings
# preview showed up under both AAPL and AMZN. Keep a story when the headline
# names the ticker or the company, but fall back to the unfiltered feed if that
# would leave the stock with nothing — a strict filter is worse than a little
# noise when the alternative is an empty panel.
news_is_relevant <- function(title, sym, company) {
  if (is.na(title) || !nzchar(title)) return(FALSE)
  t <- toupper(title)
  if (grepl(paste0("\\b", sym, "\\b"), t)) return(TRUE)
  if (!is.na(company) && nzchar(company)) {
    # match the distinctive first word ("Apple" from "Apple Inc.") and skip
    # generic leaders that would match half the market
    w <- toupper(gsub("[^A-Za-z ].*$", "", company))
    w <- strsplit(trimws(w), " ")[[1]][1]
    if (!is.na(w) && nchar(w) >= 3 &&
        !w %in% c("THE", "FIRST", "GENERAL", "AMERICAN", "NATIONAL", "GLOBAL", "UNITED")) {
      if (grepl(paste0("\\b", w, "\\b"), t)) return(TRUE)
    }
  }
  FALSE
}

get_stock_news <- function(tickers, fundamentals = NULL) {
  message(glue("Pulling per-ticker news (Yahoo RSS) for {length(tickers)} stocks..."))
  names_map <- if (!is.null(fundamentals) && all(c("symbol", "company") %in% names(fundamentals)))
                 setNames(fundamentals$company, fundamentals$symbol) else character(0)
  out <- vector("list", length(tickers))
  for (i in seq_along(tickers)) {
    sym <- tickers[i]
    d <- fetch_rss_items(
      glue("https://feeds.finance.yahoo.com/rss/2.0/headline?s={sym}&region=US&lang=en-US"),
      NEWS_RSS_PER_TICKER)
    if (!is.null(d) && nrow(d) > 0) {
      comp <- if (sym %in% names(names_map)) names_map[[sym]] else NA_character_
      d$relevant <- vapply(d$title, news_is_relevant, logical(1), sym = sym, company = comp)
      keep <- if (any(d$relevant)) d[d$relevant, , drop = FALSE] else d
      out[[i]] <- keep %>% mutate(symbol = sym)
    }
    Sys.sleep(0.25)
    if (i %% 50 == 0) message(glue("    {i}/{length(tickers)}"))
  }
  df <- bind_rows(out)
  if (nrow(df) == 0) {
    message("  No per-ticker news returned.")
    return(tibble())
  }
  df <- df %>%
    mutate(published_parsed = suppressWarnings(
             as.POSIXct(published, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")),
           publisher = ifelse(is.na(publisher) | !nzchar(publisher), "Yahoo Finance", publisher)) %>%
    filter(!is.na(title), nzchar(title)) %>%
    distinct(symbol, title, .keep_all = TRUE) %>%
    select(symbol, title, url, publisher, published, published_parsed)
  message(glue("  Stock news: {nrow(df)} articles across ",
               "{dplyr::n_distinct(df$symbol)}/{length(tickers)} tickers"))
  df
}

# ── SEC EDGAR filings ──────────────────────────────────────────────────────
# Official, free, no key. SEC asks for a User-Agent identifying the requester
# and caps traffic at 10 requests/second.
#
# Form 4 is an insider transaction, which is the closest honest answer to "what
# has this company been buying" that a free source provides. S-4 and SC 13D
# signal merger activity and large stake-building respectively.
SEC_UA <- "EdgeScreener research (jackmateja@icloud.com)"
SEC_NOTABLE <- c("8-K", "10-K", "10-Q", "S-4", "DEF 14A")

sec_cik_map <- function() {
  tryCatch({
    r <- GET("https://www.sec.gov/files/company_tickers.json",
             add_headers(`User-Agent` = SEC_UA), timeout(30))
    if (status_code(r) != 200) return(NULL)
    d <- fromJSON(content(r, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
    m <- vapply(d, function(x) as.character(x$cik_str), character(1))
    names(m) <- vapply(d, function(x) as.character(x$ticker), character(1))
    m
  }, error = function(e) NULL)
}

sec_filings_for <- function(sym, cik) {
  tryCatch({
    padded <- sprintf("CIK%010d", as.integer(cik))
    r <- GET(glue("https://data.sec.gov/submissions/{padded}.json"),
             add_headers(`User-Agent` = SEC_UA), timeout(30))
    if (status_code(r) != 200) return(NULL)
    d <- fromJSON(content(r, as = "text", encoding = "UTF-8"))
    rec <- d$filings$recent
    if (is.null(rec) || length(rec$form) == 0) return(NULL)

    f <- tibble(form = as.character(rec$form),
                filed = suppressWarnings(as.Date(rec$filingDate)),
                accession = as.character(rec$accessionNumber),
                doc = as.character(rec$primaryDocument),
                description = as.character(rec$primaryDocDescription)) %>%
      filter(!is.na(filed))

    insider_90d <- sum(f$form == "4" & f$filed >= Sys.Date() - 90, na.rm = TRUE)
    merger_flag <- any(f$form %in% c("S-4") & f$filed >= Sys.Date() - 365, na.rm = TRUE)
    events_1y   <- sum(f$form == "8-K" & f$filed >= Sys.Date() - 365, na.rm = TRUE)

    notable <- f %>%
      filter(form %in% SEC_NOTABLE) %>%
      arrange(desc(filed)) %>%
      utils::head(5) %>%
      mutate(symbol = sym,
             url = glue("https://www.sec.gov/Archives/edgar/data/{as.integer(cik)}/",
                        "{gsub('-', '', accession)}/{doc}"),
             insider_filings_90d = insider_90d,
             merger_activity_1y  = merger_flag,
             material_events_1y  = events_1y) %>%
      select(symbol, form, filed, description, url,
             insider_filings_90d, merger_activity_1y, material_events_1y)
    if (nrow(notable) == 0) return(NULL)
    notable
  }, error = function(e) NULL)
}

# ── SEC XBRL financials (DCF inputs) ────────────────────────────────────────
# The DCF panel had no data behind it: fcf and debt_equity were empty for all
# 195 names and compute_dcf() was never defined anywhere, so the panel rendered
# a permanent "N/A" next to a hardcoded "WACC Used 10.0%". Polygon's financials
# endpoint returned nothing for any ticker.
#
# SEC's XBRL "frames" API answers one concept for every filer in a single
# request, so the whole universe costs ~30 requests rather than 195 downloads of
# 3-4 MB companyfacts each.
SEC_FRAMES <- "https://data.sec.gov/api/xbrl/frames/us-gaap"

sec_frame <- function(concept, frame) {
  tryCatch({
    r <- GET(glue("{SEC_FRAMES}/{concept}/USD/{frame}.json"),
             add_headers(`User-Agent` = SEC_UA), timeout(60))
    if (status_code(r) != 200) return(NULL)
    d <- fromJSON(content(r, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
    if (is.null(d$data) || length(d$data) == 0) return(NULL)
    ciks <- vapply(d$data, function(x) as.character(x$cik), character(1))
    vals <- vapply(d$data, function(x) suppressWarnings(as.numeric(x$val)), numeric(1))
    v <- vals[!duplicated(ciks)]
    names(v) <- ciks[!duplicated(ciks)]
    v
  }, error = function(e) NULL)
}

# Filers have different fiscal year ends, so no single calendar frame covers
# everyone. Walk the frames most-recent-first and keep the first value found for
# each company.
sec_frame_merge <- function(concept, frames) {
  out <- numeric(0)
  for (f in frames) {
    v <- sec_frame(concept, f)
    Sys.sleep(0.15)
    if (is.null(v)) next
    new <- setdiff(names(v), names(out))
    out <- c(out, v[new])
  }
  out
}

# The current year's annual frame is not complete until well after year end.
sec_annual_frames <- function(n = 3) {
  y <- as.integer(format(Sys.Date(), "%Y"))
  paste0("CY", seq(y - 1, y - n))
}

sec_instant_frames <- function(n = 5) {
  d <- Sys.Date()
  q <- (as.integer(format(d, "%m")) - 1) %/% 3 + 1
  y <- as.integer(format(d, "%Y"))
  out <- character(0)
  for (i in seq_len(n)) {
    q <- q - 1
    if (q < 1) { q <- 4; y <- y - 1 }
    out <- c(out, sprintf("CY%dQ%dI", y, q))
  }
  out
}

# Companies tag the same line item differently, so each figure has fallbacks.
sec_pick <- function(ciks, ...) {
  srcs <- list(...)
  out <- rep(NA_real_, length(ciks))
  for (v in srcs) {
    if (is.null(v) || length(v) == 0) next
    hit <- is.na(out) & ciks %in% names(v)
    if (any(hit)) out[hit] <- unname(v[ciks[hit]])
  }
  out
}

get_sec_financials <- function(tickers) {
  message(glue("Pulling SEC XBRL financials for {length(tickers)} stocks..."))
  cikmap <- sec_cik_map()
  if (is.null(cikmap)) {
    message("  SEC ticker map unavailable - skipping financials.")
    return(tibble())
  }
  have <- tickers[tickers %in% names(cikmap)]
  if (length(have) == 0) return(tibble())
  # frames key on the bare integer CIK, not the zero-padded form
  ciks <- as.character(as.integer(cikmap[have]))

  af <- sec_annual_frames()
  qf <- sec_instant_frames()

  # Free cash flow year by year rather than a single snapshot. A company in a
  # heavy investment cycle has temporarily depressed FCF — Amazon's latest year
  # nets to ~$8B against a $259 share price — so valuing off one year produces a
  # meaningless number. Averaging the available years normalises the capex cycle.
  #
  # Amazon and other large filers tag capex as PaymentsToAcquireProductiveAssets
  # rather than the more common PP&E tag; without that fallback capex read as
  # zero and free cash flow came out as roughly operating cash flow.
  fcf_years <- lapply(af, function(f) {
    o  <- sec_frame("NetCashProvidedByUsedInOperatingActivities", f); Sys.sleep(0.15)
    ca <- sec_frame("PaymentsToAcquirePropertyPlantAndEquipment", f); Sys.sleep(0.15)
    cb <- sec_frame("PaymentsToAcquireProductiveAssets", f);          Sys.sleep(0.15)
    ov <- sec_pick(ciks, o)
    cv <- sec_pick(ciks, ca, cb)
    ifelse(is.na(ov) | is.na(cv), NA_real_, ov - abs(cv))
  })
  fcf_mat <- do.call(cbind, fcf_years)

  ocf    <- sec_frame_merge("NetCashProvidedByUsedInOperatingActivities", af)
  capex_a <- sec_frame_merge("PaymentsToAcquirePropertyPlantAndEquipment", af)
  capex_b <- sec_frame_merge("PaymentsToAcquireProductiveAssets", af)
  gp     <- sec_frame_merge("GrossProfit", af)
  rev_a  <- sec_frame_merge("Revenues", af)
  rev_b  <- sec_frame_merge("RevenueFromContractWithCustomerExcludingAssessedTax", af)
  eq_a   <- sec_frame_merge("StockholdersEquity", qf)
  eq_b   <- sec_frame_merge("StockholdersEquityIncludingPortionAttributableToNoncontrollingInterest", qf)
  ltd_nc <- sec_frame_merge("LongTermDebtNoncurrent", qf)
  ltd_cu <- sec_frame_merge("LongTermDebtCurrent", qf)
  ltd_b  <- sec_frame_merge("LongTermDebt", qf)
  ltd_c  <- sec_frame_merge("LongTermDebtAndCapitalLeaseObligations", qf)
  ltd_d  <- sec_frame_merge("DebtLongtermAndShorttermCombinedAmount", qf)
  cash_a <- sec_frame_merge("CashAndCashEquivalentsAtCarryingValue", qf)

  # Revenue for the two most recent complete years, to derive a real growth rate
  # instead of reusing quarterly EPS growth as a revenue proxy.
  y <- as.integer(format(Sys.Date(), "%Y"))
  rev_now  <- sec_pick(ciks, sec_frame_merge("Revenues", paste0("CY", y - 1)),
                             sec_frame_merge("RevenueFromContractWithCustomerExcludingAssessedTax",
                                             paste0("CY", y - 1)))
  rev_prev <- sec_pick(ciks, sec_frame_merge("Revenues", paste0("CY", y - 3)),
                             sec_frame_merge("RevenueFromContractWithCustomerExcludingAssessedTax",
                                             paste0("CY", y - 3)))

  ocf_v   <- sec_pick(ciks, ocf)
  capex_v <- sec_pick(ciks, capex_a, capex_b)
  rev_v   <- sec_pick(ciks, rev_a, rev_b)
  gp_v    <- sec_pick(ciks, gp)
  eq_v    <- sec_pick(ciks, eq_a, eq_b)
  cash_v  <- sec_pick(ciks, cash_a)

  # Total long-term debt: the non-current balance plus any current portion,
  # falling back to whichever combined tag the filer used.
  nc_v <- sec_pick(ciks, ltd_nc)
  cu_v <- sec_pick(ciks, ltd_cu)
  debt_split <- ifelse(is.na(nc_v), NA_real_, nc_v + ifelse(is.na(cu_v), 0, cu_v))
  debt_v <- sec_pick(ciks, setNames(debt_split, ciks), ltd_b, ltd_c, ltd_d)

  # Capex is reported as a positive outflow, so subtract its magnitude. When
  # capex is genuinely unavailable leave FCF missing rather than defaulting it
  # to zero, which would silently report operating cash flow as free cash flow.
  fcf_latest <- ifelse(is.na(ocf_v) | is.na(capex_v), NA_real_, ocf_v - abs(capex_v))
  fcf_v <- apply(fcf_mat, 1, function(r)
    if (all(is.na(r))) NA_real_ else mean(r, na.rm = TRUE))
  fcf_n <- apply(fcf_mat, 1, function(r) sum(!is.na(r)))
  fcf_v <- ifelse(is.na(fcf_v), fcf_latest, fcf_v)

  # Two-year revenue CAGR, only where both endpoints are real and positive.
  cagr <- ifelse(!is.na(rev_now) & !is.na(rev_prev) & rev_prev > 0 & rev_now > 0,
                 (rev_now / rev_prev)^(1/2) - 1, NA_real_)

  df <- tibble(
    symbol            = have,
    fcf_sec           = fcf_v,          # normalised: mean of available years
    fcf_latest_sec    = fcf_latest,
    fcf_years_sec     = fcf_n,
    ocf_sec           = ocf_v,
    capex_sec         = capex_v,
    revenue_sec       = rev_v,
    gross_margin_sec  = ifelse(!is.na(gp_v) & !is.na(rev_v) & rev_v != 0,
                               round(gp_v / rev_v, 4), NA_real_),
    equity_sec        = eq_v,
    total_debt_sec    = debt_v,
    cash_sec          = cash_v,
    debt_equity_sec   = ifelse(!is.na(debt_v) & !is.na(eq_v) & eq_v > 0,
                               round(debt_v / eq_v, 4), NA_real_),
    rev_cagr_sec      = round(cagr, 4)
  )

  message(glue("  SEC financials: fcf {sum(!is.na(df$fcf_sec))}/{nrow(df)} ",
               "(mean {round(mean(df$fcf_years_sec),1)} yrs), ",
               "debt/equity {sum(!is.na(df$debt_equity_sec))}/{nrow(df)}, ",
               "rev CAGR {sum(!is.na(df$rev_cagr_sec))}/{nrow(df)}"))
  df
}

get_sec_filings <- function(tickers) {
  message(glue("Pulling SEC EDGAR filings for {length(tickers)} stocks..."))
  cik <- sec_cik_map()
  if (is.null(cik)) {
    message("  SEC ticker map unavailable — skipping filings.")
    return(tibble())
  }
  message(glue("  CIK map: {length(cik)} tickers; ",
               "{sum(tickers %in% names(cik))}/{length(tickers)} of the universe matched"))

  out <- vector("list", length(tickers))
  for (i in seq_along(tickers)) {
    sym <- tickers[i]
    if (!sym %in% names(cik)) next
    out[[i]] <- sec_filings_for(sym, cik[[sym]])
    Sys.sleep(0.15)   # SEC caps at 10 req/sec
    if (i %% 50 == 0) message(glue("    {i}/{length(tickers)}"))
  }
  df <- bind_rows(out)
  if (nrow(df) == 0) {
    message("  No filings returned.")
    return(tibble())
  }
  message(glue("  SEC filings: {nrow(df)} rows across ",
               "{dplyr::n_distinct(df$symbol)} tickers; ",
               "{sum(df$merger_activity_1y[!duplicated(df$symbol)], na.rm=TRUE)} with M&A activity"))
  df
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

# ── Sentiment history (accumulator) ────────────────────────────────────────
# The three qualitative feeds — AV news, WSB, StockTwits — only ever serve the
# present moment. Unlike prices, they cannot be re-fetched for a past date, so
# there is no way to backtest a sentiment signal retroactively. The only route
# to ever testing one is to start recording snapshots now.
#
# One row per symbol per day. Nothing consumes this yet by design: a signal
# should not enter the score before it can be validated, and validation needs
# roughly a quarter of observations to be worth anything.
SENTIMENT_HISTORY <- "data/sentiment_history.csv"
SENTIMENT_KEEP_DAYS <- 1120

append_sentiment_history <- function(news, wsb, stwits) {
  message("Recording sentiment snapshot...")
  today <- Sys.Date()

  num <- function(x) suppressWarnings(as.numeric(x))

  # News: average sentiment score per ticker, plus article count
  news_part <- if (is.data.frame(news) && nrow(news) > 0 &&
                   all(c("symbol", "sentiment_score") %in% names(news))) {
    news %>%
      filter(!is.na(symbol)) %>%
      group_by(symbol) %>%
      summarize(news_sentiment = mean(num(sentiment_score), na.rm = TRUE),
                news_articles  = n(), .groups = "drop")
  } else tibble(symbol = character(), news_sentiment = numeric(),
                news_articles = integer())

  # WSB: mentions and how far the ticker moved up the board in 24h
  wsb_part <- if (is.data.frame(wsb) && nrow(wsb) > 0 && "ticker" %in% names(wsb)) {
    wsb %>%
      transmute(symbol       = as.character(ticker),
                wsb_mentions = num(mentions),
                wsb_rank     = num(rank),
                wsb_rank_chg = num(rank_24h_ago) - num(rank)) %>%
      filter(!is.na(symbol))
  } else tibble(symbol = character(), wsb_mentions = numeric(),
                wsb_rank = numeric(), wsb_rank_chg = numeric())

  # StockTwits: bull/bear split, normalised so it is comparable across days
  stwits_part <- if (is.data.frame(stwits) && nrow(stwits) > 0 &&
                     "symbol" %in% names(stwits)) {
    stwits %>%
      transmute(symbol      = as.character(symbol),
                st_bullish  = num(bullish),
                st_bearish  = num(bearish),
                st_watchers = num(watchlist_count),
                st_bull_ratio = ifelse((num(bullish) + num(bearish)) > 0,
                                       num(bullish) / (num(bullish) + num(bearish)),
                                       NA_real_)) %>%
      filter(!is.na(symbol))
  } else tibble(symbol = character(), st_bullish = numeric(), st_bearish = numeric(),
                st_watchers = numeric(), st_bull_ratio = numeric())

  snapshot <- full_join(news_part, wsb_part, by = "symbol") %>%
    full_join(stwits_part, by = "symbol") %>%
    mutate(date = today) %>%
    select(date, symbol, everything()) %>%
    filter(!is.na(symbol), symbol != "")

  if (nrow(snapshot) == 0) {
    message("  No sentiment data to record this run.")
    return(invisible(NULL))
  }

  if (file.exists(SENTIMENT_HISTORY)) {
    prior <- tryCatch(
      read_csv(SENTIMENT_HISTORY, show_col_types = FALSE) %>%
        mutate(date = as.Date(date)),
      error = function(e) NULL)
    # Re-running on the same day replaces that day rather than duplicating it
    if (!is.null(prior) && nrow(prior) > 0)
      snapshot <- bind_rows(prior %>% filter(date != today), snapshot)
  }

  snapshot <- snapshot %>%
    distinct(symbol, date, .keep_all = TRUE) %>%
    filter(date >= Sys.Date() - SENTIMENT_KEEP_DAYS) %>%
    arrange(symbol, date)

  write_csv(snapshot, SENTIMENT_HISTORY)
  days <- dplyr::n_distinct(snapshot$date)
  message(glue("  sentiment_history: {nrow(snapshot)} rows across {days} day(s), ",
               "{dplyr::n_distinct(snapshot$symbol)} symbols"))
  if (days < 63)
    message(glue("  Not yet testable — needs ~63 trading days, has {days}."))
  invisible(snapshot)
}

run_module3 <- function(tickers=NULL) {
  message("\n=== MODULE 3: SHORT INTEREST, MACRO, EARNINGS, NEWS ===\n")
  if (is.null(tickers)) {
    fund <- read_csv("data/fundamentals_scored.csv", show_col_types=FALSE)
    tickers <- fund$symbol
  }
  fund_data <- read_csv("data/fundamentals_scored.csv", show_col_types=FALSE)

  short   <- get_short_interest(tickers, fund_data)
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
  # Per-ticker coverage. The market-wide AV feed reaches only ~6 of 195 stocks
  # on a given day; RSS and EDGAR are per-symbol and need no key.
  stocknews <- tryCatch(get_stock_news(tickers, fund_data),
                        error = function(e) { message("Stock news skipped: ", conditionMessage(e)); tibble() })
  secfil    <- tryCatch(get_sec_filings(tickers),
                        error = function(e) { message("SEC filings skipped: ", conditionMessage(e)); tibble() })
  secfin    <- tryCatch(get_sec_financials(tickers),
                        error = function(e) { message("SEC financials skipped: ", conditionMessage(e)); tibble() })

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
  write_or_keep(stocknews, "data/stock_news.csv",   "stock news")
  write_or_keep(secfil,    "data/sec_filings.csv",  "sec filings")
  write_or_keep(secfin,    "data/sec_financials.csv", "sec financials")

  # Accumulate the qualitative feeds into a testable series. Non-fatal: this is
  # a recording step for future analysis and must never break the refresh.
  tryCatch(append_sentiment_history(news, wsb, stwits),
           error = function(e) message("Sentiment history skipped: ", conditionMessage(e)))

  message(glue("Saved: squeeze({nrow(squeeze)}), macro({nrow(macro)}), ",
               "earnings({nrow(earn)}), news({nrow(news)}), ",
               "sector({nrow(sector)}), wsb({nrow(wsb)}), ",
               "stocktwits({nrow(stwits)}), stock_news({nrow(stocknews)}), ",
               "sec_filings({nrow(secfil)}), sec_financials({nrow(secfin)})"))
  list(squeeze=squeeze, macro=macro, earnings=earn, news=news,
       sector=sector, wsb=wsb, stocktwits=stwits,
       stock_news=stocknews, sec_filings=secfil, sec_financials=secfin)
}

if (!exists("SOURCED_BY_MASTER")) module3_data <- run_module3()
