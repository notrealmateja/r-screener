# =============================================================================
# 01_fundamentals.R
# EdgeScreener — Multi-Source Data Pipeline
# Sources: Yahoo Finance, FMP, Alpha Vantage, Finnhub, FRED,
#          Polygon, Tiingo, Twelve Data, SEC EDGAR
# Rolling cache: fetches 50 stocks/day across 3-day rotation
# =============================================================================

run_module1 <- function(tickers = NULL, force_refresh = FALSE) {
  message("\n=== MODULE 1: MULTI-SOURCE DATA PIPELINE ===\n")

  library(quantmod)
  library(dplyr)
  library(readr)
  library(jsonlite)
  library(httr)
  library(TTR)
  library(lubridate)

  # ── API KEYS ──────────────────────────────────────────────────────────────────
  FMP_KEY    <- "WEe4bM0zNn8UagrZtzyijnANOJa6qrBK"
  AV_KEY     <- "GCL8H0RSOMX48AWU"
  FH_KEY     <- "d6sdol1r01qj447b9nngd6sdol1r01qj447b9no0"
  POLYGON_KEY<- "CZ9uLCagJ4dWJMs1Efz79gA80ThXlQlO"
  TIINGO_KEY <- "8d2c7e68b5d95ecf39897ec76884bf84f65cf15d"
  TWELVE_KEY <- "46415134d68a44cb9467d9b1a45e3eea"
  NASDAQ_KEY <- "CazxRCZ6cf1maD5aXGzm"

  # ── DIRECTORIES ───────────────────────────────────────────────────────────────
  if (!dir.exists("data")) dir.create("data", recursive = TRUE)
  if (!dir.exists("app"))  dir.create("app",  recursive = TRUE)

  # ── FULL TICKER UNIVERSE (150 stocks) ────────────────────────────────────────
  if (is.null(tickers)) {
    tickers <- unique(c(
      # Mega cap tech
      "AAPL","MSFT","NVDA","AMZN","GOOGL","META","TSLA","AVGO","ORCL","CRM",
      "ADBE","INTU","AMD","QCOM","TXN","IBM","AMAT","MU","LRCX","KLAC",
      "SNPS","CDNS","MRVL","NXPI","ON","HPQ","DELL","CSCO","INTC","ACN",
      # Financials
      "JPM","BAC","WFC","GS","MS","BLK","SCHW","AXP","V","MA",
      "SPGI","MCO","ICE","CME","CB","PGR","MET","PRU","TRV","AIG",
      # Healthcare
      "UNH","JNJ","LLY","ABBV","MRK","TMO","ABT","DHR","BMY","AMGN",
      "GILD","REGN","VRTX","ISRG","BSX","EW","BDX","IQV","ZBH","BAX",
      # Consumer
      "WMT","COST","TGT","HD","LOW","MCD","SBUX","NKE","TJX","ROST",
      "BKNG","MAR","HLT","YUM","CMG","LULU","KO","PEP","PG","CL",
      # Energy
      "XOM","CVX","COP","EOG","SLB","MPC","PSX","VLO","OXY","HES",
      # Industrials
      "GE","HON","CAT","DE","RTX","LMT","NOC","GD","BA","UPS",
      "FDX","CSX","UNP","NSC","EMR","ETN","PH","ROK","ITW","MMM",
      # Communication
      "NFLX","DIS","CMCSA","T","VZ","TMUS",
      # Utilities
      "NEE","DUK","SO","D","AEP","EXC",
      # High Growth / Unicorn Candidates (market cap < $5B focus)
      "SNOW","DDOG","ZS","CRWD","PANW","FTNT","NET","OKTA","MDB",
      "BILL","HUBS","VEEV","WDAY","NOW","TEAM","ZM","DOCU","TTD","ROKU",
      "SPOT","UBER","LYFT","ABNB","DASH","COIN","HOOD","SOFI","AFRM","UPST",
      "PLTR","AI","PATH","S","GTLB","CFLT","DKNG","RBLX","U","APP"
    ))
  }

  # ── ROLLING CACHE SYSTEM ─────────────────────────────────────────────────────
  # Splits 150 tickers into 3 batches of 50, rotates daily
  # Ensures no API limit is ever exceeded
  cache_file  <- "data/fundamentals_scored.csv"
  batch_file  <- "data/batch_tracker.rds"
  today       <- as.integer(format(Sys.Date(), "%j")) # day of year

  if (!force_refresh && file.exists(cache_file)) {
    cached <- read_csv(cache_file, show_col_types = FALSE)
    # Check if today's batch has already been fetched
    if (file.exists(batch_file)) {
      tracker <- readRDS(batch_file)
      if (tracker$last_run == Sys.Date()) {
        message("Today's batch already fetched. Using cache.")
        return(invisible(cached))
      }
    }
  } else {
    cached <- NULL
  }

  # Determine today's batch (0, 1, or 2)
  batch_idx  <- today %% 3
  batch_size <- ceiling(length(tickers) / 3)
  start_i    <- batch_idx * batch_size + 1
  end_i      <- min(start_i + batch_size - 1, length(tickers))
  today_tickers <- tickers[start_i:end_i]

  message("Batch ", batch_idx + 1, "/3 — fetching ", length(today_tickers),
          " stocks (", today_tickers[1], " → ", tail(today_tickers, 1), ")")

  # ── SOURCE 1: FRED — Macro Regime ────────────────────────────────────────────
  message("\n[1/9] FRED macro data...")
  get_fred <- function(series_id) {
    tryCatch({
      url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series_id)
      dat <- read_csv(url, show_col_types = FALSE)
      vals <- na.omit(as.numeric(dat[[2]]))
      list(current = tail(vals, 1), prev = tail(vals, 2)[1],
           trend = tail(vals, 1) - tail(vals, 2)[1])
    }, error = function(e) list(current = NA, prev = NA, trend = NA))
  }

  fed      <- get_fred("FEDFUNDS")
  cpi      <- get_fred("CPIAUCSL")
  unemp    <- get_fred("UNRATE")
  t10y     <- get_fred("DGS10")
  t2y      <- get_fred("DGS2")
  vix_fred <- get_fred("VIXCLS")

  # Yield curve spread (10Y - 2Y): negative = inverted = recession signal
  yield_spread <- if (!is.na(t10y$current) && !is.na(t2y$current))
    t10y$current - t2y$current else NA

  # Regime detection (scalar logic — use if/else chain not case_when)
  fed_rate <- fed$current
  regime <- if (!is.na(vix_fred$current) && vix_fred$current > 30) "RISK_OFF" else
            if (!is.na(yield_spread)     && yield_spread < 0)       "INVERTED" else
            if (!is.na(fed_rate)         && fed_rate > 5)           "HIGH_RATE" else
            if (!is.na(fed_rate)         && fed_rate < 2)           "LOW_RATE"  else
            "NEUTRAL"

  # Macro adjustment score (used in scoring engine)
  macro_env <- list(
    fed_rate     = fed_rate,
    yield_spread = yield_spread,
    vix          = vix_fred$current,
    inflation    = cpi$current,
    unemployment = unemp$current,
    regime       = regime
  )
  message("  Regime: ", regime, " | Fed: ", fed_rate,
          " | Spread: ", ifelse(is.na(yield_spread), "NA", round(yield_spread, 3)),
          " | VIX: ", vix_fred$current)

  # ── SOURCE 2: YAHOO FINANCE — 252-Day Price History ──────────────────────────
  message("\n[2/9] Yahoo Finance — pulling 252-day price history...")
  get_yahoo <- function(sym) {
    tryCatch({
      env <- new.env()
      suppressWarnings(
        getSymbols(sym, src = "yahoo", env = env,
                   auto.assign = TRUE, from = Sys.Date() - 380)
      )
      sym_clean <- gsub("\\.", "", sym)  # BRK.B → BRKB in env
      px_obj <- if (exists(sym, envir=env)) get(sym, envir=env) else
                if (exists(sym_clean, envir=env)) get(sym_clean, envir=env) else NULL
      if (is.null(px_obj)) return(NULL)
      px  <- Cl(px_obj)
      vol <- Vo(px_obj)

      # Ensure we have at least 60 days
      if (nrow(px) < 60) return(NULL)

      price  <- as.numeric(last(px))
      hi52   <- as.numeric(max(tail(px, 252), na.rm = TRUE))
      lo52   <- as.numeric(min(tail(px, 252), na.rm = TRUE))
      n      <- nrow(px)

      # Returns
      ret1d  <- as.numeric((px[n] / px[n-1]  - 1) * 100)
      ret5d  <- as.numeric((px[n] / px[max(1,n-5)]  - 1) * 100)
      ret1m  <- as.numeric((px[n] / px[max(1,n-21)] - 1) * 100)
      ret3m  <- as.numeric((px[n] / px[max(1,n-63)] - 1) * 100)
      ret6m  <- as.numeric((px[n] / px[max(1,n-126)]- 1) * 100)
      ret1y  <- as.numeric((px[n] / px[1]           - 1) * 100)

      # Technicals
      rsi14  <- as.numeric(last(RSI(px, n = 14)))
      sma20  <- as.numeric(last(SMA(px, n = 20)))
      sma50  <- as.numeric(last(SMA(px, n = 50)))
      sma200 <- as.numeric(last(SMA(px, n = 200)))
      macd_r <- tryCatch(as.numeric(last(MACD(px)$macd)), error=function(e) NA)
      macd_s <- tryCatch(as.numeric(last(MACD(px)$signal)), error=function(e) NA)
      bb     <- tryCatch(last(BBands(px)), error=function(e) NULL)
      bb_pct <- if (!is.null(bb)) as.numeric((price - bb[,"dn"]) / (bb[,"up"] - bb[,"dn"])) else NA

      # Volatility (annualized)
      daily_rets <- diff(log(as.numeric(px)))
      vol_20d    <- sd(tail(daily_rets, 20),  na.rm=TRUE) * sqrt(252) * 100
      vol_60d    <- sd(tail(daily_rets, 60),  na.rm=TRUE) * sqrt(252) * 100
      vol_252d   <- sd(daily_rets,            na.rm=TRUE) * sqrt(252) * 100

      # Average daily volume
      avg_vol_20d <- mean(as.numeric(tail(vol, 20)), na.rm=TRUE)

      # Trend strength: % of last 20 days that were positive
      trend_strength <- mean(tail(daily_rets, 20) > 0, na.rm=TRUE) * 100

      # Price position in 52w range
      range_pct <- if (!is.na(hi52) && hi52 != lo52)
        (price - lo52) / (hi52 - lo52) * 100 else 50

      # Golden cross flag
      golden_cross <- !is.na(sma50) && !is.na(sma200) && sma50 > sma200

      list(
        price          = round(price,   2),
        high_52w       = round(hi52,    2),
        low_52w        = round(lo52,    2),
        ret_1d         = round(ret1d,   3),
        ret_5d         = round(ret5d,   3),
        ret_1m         = round(ret1m,   3),
        ret_3m         = round(ret3m,   3),
        ret_6m         = round(ret6m,   3),
        ret_1y         = round(ret1y,   3),
        rsi14          = round(rsi14,   2),
        sma20          = round(sma20,   2),
        sma50          = round(sma50,   2),
        sma200         = round(sma200,  2),
        macd           = round(macd_r,  4),
        macd_signal    = round(macd_s,  4),
        bb_pct         = round(bb_pct,  3),
        vol_20d        = round(vol_20d, 2),
        vol_60d        = round(vol_60d, 2),
        vol_252d       = round(vol_252d,2),
        avg_vol_20d    = round(avg_vol_20d, 0),
        trend_strength = round(trend_strength, 1),
        range_pct      = round(range_pct, 1),
        above_ma20     = !is.na(sma20)  && price > sma20,
        above_ma50     = !is.na(sma50)  && price > sma50,
        above_ma200    = !is.na(sma200) && price > sma200,
        golden_cross   = golden_cross,
        # Store raw price series for backtesting
        px_series      = as.numeric(px)
      )
    }, error = function(e) {
      message("  Yahoo error [", sym, "]: ", e$message)
      NULL
    })
  }

  # ── SOURCE 3: FMP — Deep Fundamentals ────────────────────────────────────────
  message("\n[3/9] FMP fundamentals...")
  get_fmp <- function(sym) {
    safe_get <- function(url) {
      tryCatch({
        r <- GET(url)
        if (status_code(r) != 200) return(NULL)
        fromJSON(content(r, "text", encoding="UTF-8"))
      }, error=function(e) NULL)
    }
    safe_num <- function(df, col) {
      if (is.null(df) || length(df)==0 || !col %in% names(df)) return(NA)
      suppressWarnings(as.numeric(df[[col]][1]))
    }
    safe_chr <- function(df, col) {
      if (is.null(df) || length(df)==0 || !col %in% names(df)) return(NA)
      as.character(df[[col]][1])
    }

    ratios  <- safe_get(paste0("https://financialmodelingprep.com/api/v3/ratios-ttm/",       sym, "?apikey=", FMP_KEY))
    profile <- safe_get(paste0("https://financialmodelingprep.com/api/v3/profile/",           sym, "?apikey=", FMP_KEY))
    growth  <- safe_get(paste0("https://financialmodelingprep.com/api/v3/financial-growth/",  sym, "?limit=1&apikey=", FMP_KEY))
    dcf     <- safe_get(paste0("https://financialmodelingprep.com/api/v3/discounted-cash-flow/", sym, "?apikey=", FMP_KEY))
    Sys.sleep(0.25)

    list(
      pe_ratio        = safe_num(ratios,  "peRatioTTM"),
      pb_ratio        = safe_num(ratios,  "priceToBookRatioTTM"),
      ps_ratio        = safe_num(ratios,  "priceToSalesRatioTTM"),
      peg_ratio       = safe_num(ratios,  "priceEarningsToGrowthRatioTTM"),
      roe             = safe_num(ratios,  "returnOnEquityTTM"),
      roa             = safe_num(ratios,  "returnOnAssetsTTM"),
      roic            = safe_num(ratios,  "returnOnCapitalEmployedTTM"),
      profit_margin   = safe_num(ratios,  "netProfitMarginTTM"),
      gross_margin    = safe_num(ratios,  "grossProfitMarginTTM"),
      operating_margin= safe_num(ratios,  "operatingProfitMarginTTM"),
      debt_equity     = safe_num(ratios,  "debtEquityRatioTTM"),
      current_ratio   = safe_num(ratios,  "currentRatioTTM"),
      interest_coverage=safe_num(ratios,  "interestCoverageTTM"),
      revenue_growth  = safe_num(growth,  "revenueGrowth"),
      earnings_growth = safe_num(growth,  "netIncomeGrowth"),
      fcf_growth      = safe_num(growth,  "freeCashFlowGrowth"),
      eps_growth      = safe_num(growth,  "epsgrowth"),
      market_cap      = safe_num(profile, "mktCap"),
      sector          = safe_chr(profile, "sector"),
      industry        = safe_chr(profile, "industry"),
      company         = safe_chr(profile, "companyName"),
      revenue         = safe_num(profile, "revenue"),
      dcf_value       = safe_num(dcf,     "dcf"),
      dcf_price       = safe_num(dcf,     "Stock Price")
    )
  }

  # ── SOURCE 4: ALPHA VANTAGE — Earnings Surprise ───────────────────────────────
  message("\n[4/9] Alpha Vantage earnings...")
  # AV free = 25 req/day — only fetch for stocks with highest momentum
  get_av <- function(sym) {
    tryCatch({
      url <- paste0("https://www.alphavantage.co/query?function=EARNINGS&symbol=",
                    sym, "&apikey=", AV_KEY)
      dat <- fromJSON(content(GET(url), "text", encoding="UTF-8"))
      q   <- dat$quarterlyEarnings
      if (is.null(q) || nrow(q) == 0) return(list(eps=NA, eps_est=NA, surprise_pct=NA, beat_count=NA))

      eps_actual    <- suppressWarnings(as.numeric(q$reportedEPS[1]))
      eps_estimated <- suppressWarnings(as.numeric(q$estimatedEPS[1]))
      surprise_pct  <- if (!is.na(eps_estimated) && eps_estimated != 0)
        (eps_actual - eps_estimated) / abs(eps_estimated) * 100 else NA

      # How many of last 4 quarters did stock beat estimates?
      beat_count <- sum(suppressWarnings(
        as.numeric(q$reportedEPS[1:min(4,nrow(q))]) >
        as.numeric(q$estimatedEPS[1:min(4,nrow(q))])
      ), na.rm=TRUE)

      list(eps=eps_actual, eps_est=eps_estimated,
           surprise_pct=round(surprise_pct,2), beat_count=beat_count)
    }, error=function(e) list(eps=NA, eps_est=NA, surprise_pct=NA, beat_count=NA))
  }

  # ── SOURCE 5: FINNHUB — Analyst Ratings + Sentiment ──────────────────────────
  message("\n[5/9] Finnhub analyst data...")
  get_finnhub <- function(sym) {
    tryCatch({
      rec    <- fromJSON(content(GET(paste0(
        "https://finnhub.io/api/v1/stock/recommendation?symbol=", sym,
        "&token=", FH_KEY)), "text", encoding="UTF-8"))
      metric <- fromJSON(content(GET(paste0(
        "https://finnhub.io/api/v1/stock/metric?symbol=", sym,
        "&metric=all&token=", FH_KEY)), "text", encoding="UTF-8"))
      sentiment <- tryCatch(fromJSON(content(GET(paste0(
        "https://finnhub.io/api/v1/news-sentiment?symbol=", sym,
        "&token=", FH_KEY)), "text", encoding="UTF-8")), error=function(e) NULL)
      Sys.sleep(0.1)

      strong_buy  <- if (length(rec)>0 && "strongBuy"  %in% names(rec)) rec$strongBuy[1]  else 0
      buy         <- if (length(rec)>0 && "buy"        %in% names(rec)) rec$buy[1]        else 0
      hold        <- if (length(rec)>0 && "hold"       %in% names(rec)) rec$hold[1]       else 0
      sell        <- if (length(rec)>0 && "sell"       %in% names(rec)) rec$sell[1]       else 0
      strong_sell <- if (length(rec)>0 && "strongSell" %in% names(rec)) rec$strongSell[1] else 0
      total       <- strong_buy + buy + hold + sell + strong_sell

      analyst_score <- if (total > 0)
        round(((strong_buy*2 + buy*1 + hold*0 + sell*(-1) + strong_sell*(-2)) / (total*2)) * 50 + 50, 1)
      else 50

      beta          <- tryCatch(as.numeric(metric$metric$beta),             error=function(e) NA)
      short_ratio   <- tryCatch(as.numeric(metric$metric$shortRatio),       error=function(e) NA)
      short_pct     <- tryCatch(as.numeric(metric$metric$shortInterestPercentageFreeFloat), error=function(e) NA)
      news_score    <- tryCatch(as.numeric(sentiment$buzz$articlesInLastWeek), error=function(e) NA)
      sentiment_score <- tryCatch(as.numeric(sentiment$sentiment$bearishPercent), error=function(e) NA)

      list(analyst_score=analyst_score, beta=beta,
           strong_buy=strong_buy, buy=buy, hold=hold, sell=sell,
           total_analysts=total, short_ratio=short_ratio,
           short_pct=short_pct, news_score=news_score,
           sentiment_score=sentiment_score)
    }, error=function(e) {
      list(analyst_score=50, beta=NA, strong_buy=0, buy=0, hold=0,
           sell=0, total_analysts=0, short_ratio=NA, short_pct=NA,
           news_score=NA, sentiment_score=NA)
    })
  }

  # ── SOURCE 6: POLYGON — Pre-market + News ────────────────────────────────────
  message("\n[6/9] Polygon.io...")
  get_polygon <- function(sym) {
    tryCatch({
      # Previous day OHLCV
      prev_url <- paste0("https://api.polygon.io/v2/aggs/ticker/", sym,
                         "/prev?adjusted=true&apiKey=", POLYGON_KEY)
      prev <- fromJSON(content(GET(prev_url), "text", encoding="UTF-8"))

      # Ticker details
      detail_url <- paste0("https://api.polygon.io/v3/reference/tickers/", sym,
                           "?apiKey=", POLYGON_KEY)
      detail <- fromJSON(content(GET(detail_url), "text", encoding="UTF-8"))
      Sys.sleep(0.12)

      open  <- if (!is.null(prev$results)) prev$results$o[1] else NA
      close <- if (!is.null(prev$results)) prev$results$c[1] else NA
      high  <- if (!is.null(prev$results)) prev$results$h[1] else NA
      low   <- if (!is.null(prev$results)) prev$results$l[1] else NA
      pvol  <- if (!is.null(prev$results)) prev$results$v[1] else NA
      vwap  <- if (!is.null(prev$results)) prev$results$vw[1] else NA
      shares_out <- tryCatch(as.numeric(detail$results$share_class_shares_outstanding), error=function(e) NA)

      list(prev_open=open, prev_close=close, prev_high=high,
           prev_low=low, prev_volume=pvol, vwap=vwap,
           shares_outstanding=shares_out)
    }, error=function(e) {
      list(prev_open=NA, prev_close=NA, prev_high=NA,
           prev_low=NA, prev_volume=NA, vwap=NA, shares_outstanding=NA)
    })
  }

  # ── SOURCE 7: TIINGO — EOD Cross-check + Fundamentals ───────────────────────
  message("\n[7/9] Tiingo...")
  get_tiingo <- function(sym) {
    tryCatch({
      url <- paste0("https://api.tiingo.com/tiingo/daily/", sym,
                    "/prices?startDate=", Sys.Date()-30,
                    "&resampleFreq=daily&token=", TIINGO_KEY)
      dat <- fromJSON(content(GET(url, add_headers(
                              "Content-Type"  = "application/json",
                              "Authorization" = paste("Token", TIINGO_KEY))),
                              "text", encoding="UTF-8"))
      Sys.sleep(0.1)
      if (is.null(dat) || length(dat)==0) return(list(adj_close=NA, tiingo_ret1m=NA))

      closes <- suppressWarnings(as.numeric(dat$adjClose))
      closes <- closes[!is.na(closes)]
      if (length(closes) < 2) return(list(adj_close=NA, tiingo_ret1m=NA))

      list(
        adj_close    = round(tail(closes,1), 2),
        tiingo_ret1m = round((tail(closes,1)/closes[1]-1)*100, 3)
      )
    }, error=function(e) list(adj_close=NA, tiingo_ret1m=NA))
  }

  # ── SOURCE 8: TWELVE DATA — Technical Indicators ─────────────────────────────
  message("\n[8/9] Twelve Data technicals...")
  get_twelve <- function(sym) {
    tryCatch({
      # RSI
      rsi_url <- paste0("https://api.twelvedata.com/rsi?symbol=", sym,
                        "&interval=1day&outputsize=1&apikey=", TWELVE_KEY)
      rsi_dat <- fromJSON(content(GET(rsi_url), "text", encoding="UTF-8"))

      # STOCH
      stoch_url <- paste0("https://api.twelvedata.com/stoch?symbol=", sym,
                          "&interval=1day&outputsize=1&apikey=", TWELVE_KEY)
      stoch_dat <- fromJSON(content(GET(stoch_url), "text", encoding="UTF-8"))

      # ADX (trend strength)
      adx_url <- paste0("https://api.twelvedata.com/adx?symbol=", sym,
                        "&interval=1day&outputsize=1&apikey=", TWELVE_KEY)
      adx_dat <- fromJSON(content(GET(adx_url), "text", encoding="UTF-8"))
      Sys.sleep(0.15)

      list(
        rsi_12    = tryCatch(as.numeric(rsi_dat$values$rsi[1]),   error=function(e) NA),
        stoch_k   = tryCatch(as.numeric(stoch_dat$values$slow_k[1]), error=function(e) NA),
        stoch_d   = tryCatch(as.numeric(stoch_dat$values$slow_d[1]), error=function(e) NA),
        adx       = tryCatch(as.numeric(adx_dat$values$adx[1]),   error=function(e) NA)
      )
    }, error=function(e) list(rsi_12=NA, stoch_k=NA, stoch_d=NA, adx=NA))
  }

  # ── SOURCE 9: SEC EDGAR — Verified Filing Data ────────────────────────────────
  message("\n[9/9] SEC EDGAR...")
  get_edgar <- function(sym) {
    tryCatch({
      # Get CIK from ticker
      cik_url <- paste0("https://efts.sec.gov/LATEST/search-index?q=%22",
                        sym, "%22&dateRange=custom&startdt=2020-01-01&forms=10-K")
      # Use the ticker→CIK mapping endpoint
      map_url <- "https://www.sec.gov/files/company_tickers.json"
      mapping <- tryCatch(fromJSON(content(GET(map_url), "text", encoding="UTF-8")),
                          error=function(e) NULL)
      if (is.null(mapping)) return(list(sec_revenue=NA, sec_net_income=NA, sec_eps=NA))

      # Find CIK for this ticker
      tickers_df <- tryCatch(
        do.call(rbind, lapply(mapping, function(x) data.frame(
          cik_str = x$cik_str, ticker = x$ticker, title = x$title,
          stringsAsFactors = FALSE))),
        error = function(e) NULL)
      if (is.null(tickers_df)) return(list(sec_revenue=NA, sec_net_income=NA, sec_eps=NA))
      match_row <- tickers_df[toupper(tickers_df$ticker) == toupper(sym), ]
      if (nrow(match_row) == 0) return(list(sec_revenue=NA, sec_net_income=NA, sec_eps=NA))

      cik <- formatC(match_row$cik_str[1], width=10, flag="0")

      # Get company facts
      facts_url <- paste0("https://data.sec.gov/api/xbrl/companyfacts/CIK", cik, ".json")
      facts <- tryCatch(fromJSON(content(GET(facts_url), "text", encoding="UTF-8")),
                        error=function(e) NULL)
      Sys.sleep(0.1)

      if (is.null(facts)) return(list(sec_revenue=NA, sec_net_income=NA, sec_eps=NA))

      # Extract revenue (Revenues or RevenueFromContractWithCustomerExcludingAssessedTax)
      rev_node <- tryCatch(facts$facts$`us-gaap`$Revenues$units$USD, error=function(e) NULL)
      if (is.null(rev_node)) rev_node <- tryCatch(
        facts$facts$`us-gaap`$RevenueFromContractWithCustomerExcludingAssessedTax$units$USD,
        error=function(e) NULL)

      sec_rev <- if (!is.null(rev_node)) {
        annual <- rev_node[rev_node$form == "10-K", ]
        if (nrow(annual) > 0) tail(annual$val, 1) else NA
      } else NA

      # Net income
      ni_node <- tryCatch(facts$facts$`us-gaap`$NetIncomeLoss$units$USD, error=function(e) NULL)
      sec_ni <- if (!is.null(ni_node)) {
        annual <- ni_node[ni_node$form == "10-K", ]
        if (nrow(annual) > 0) tail(annual$val, 1) else NA
      } else NA

      list(sec_revenue=sec_rev, sec_net_income=sec_ni, sec_eps=NA)
    }, error=function(e) list(sec_revenue=NA, sec_net_income=NA, sec_eps=NA))
  }

  # ── DATA CONFIDENCE SCORER ────────────────────────────────────────────────────
  calc_data_confidence <- function(yah, fmp, av, fh, poly, tiingo, twelve, edgar) {
    # Score 0-100 based on how many data points we have
    checks <- c(
      !is.na(yah$price),           # Yahoo price
      !is.na(yah$rsi14),           # Yahoo technicals
      !is.na(fmp$pe_ratio),        # FMP valuation
      !is.na(fmp$revenue_growth),  # FMP growth
      !is.na(fmp$roe),             # FMP quality
      !is.na(av$surprise_pct),     # AV earnings surprise
      !is.na(fh$beta),             # Finnhub beta
      fh$total_analysts > 0,       # Finnhub analyst coverage
      !is.na(poly$vwap),           # Polygon VWAP
      !is.na(tiingo$adj_close),    # Tiingo cross-check
      !is.na(twelve$adx),          # Twelve ADX
      !is.na(edgar$sec_revenue)    # SEC verified revenue
    )
    round(mean(checks, na.rm=TRUE) * 100, 1)
  }

  # ── MAIN FETCH LOOP ───────────────────────────────────────────────────────────
  message("\nFetching data for ", length(today_tickers), " stocks...\n")

  # Pre-fetch all Yahoo data first (no rate limit)
  message("Pre-fetching Yahoo price history for all tickers...")
  yahoo_cache <- lapply(today_tickers, function(sym) {
    message("  Yahoo: ", sym)
    get_yahoo(sym)
  })
  names(yahoo_cache) <- today_tickers

  # Determine top 25 by 3m return for Alpha Vantage allocation
  returns_3m <- sapply(today_tickers, function(s) {
    if (!is.null(yahoo_cache[[s]])) yahoo_cache[[s]]$ret_3m else -999
  })
  av_priority <- today_tickers[order(returns_3m, decreasing=TRUE)][1:min(25, length(today_tickers))]

  # Main loop
  today_results <- lapply(seq_along(today_tickers), function(i) {
    sym <- today_tickers[i]
    message("\n  [", i, "/", length(today_tickers), "] Processing ", sym, "...")

    yah <- yahoo_cache[[sym]]
    if (is.null(yah) || is.na(yah$price)) {
      message("    Skipping — no Yahoo data")
      return(NULL)
    }

    fmp    <- get_fmp(sym)
    av     <- if (sym %in% av_priority) {
                r <- get_av(sym); Sys.sleep(13); r
              } else list(eps=NA, eps_est=NA, surprise_pct=NA, beat_count=NA)
    fh     <- get_finnhub(sym)
    poly   <- get_polygon(sym)
    tiingo <- get_tiingo(sym)
    twelve <- get_twelve(sym)
    edgar  <- get_edgar(sym)

    confidence <- calc_data_confidence(yah, fmp, av, fh, poly, tiingo, twelve, edgar)

    # DCF upside/downside
    dcf_upside <- if (!is.na(fmp$dcf_value) && !is.na(yah$price) && yah$price > 0)
      round((fmp$dcf_value / yah$price - 1) * 100, 1) else NA

    data.frame(
      # Identity
      symbol            = sym,
      company           = fmp$company %||% sym,
      sector            = fmp$sector  %||% NA,
      industry          = fmp$industry %||% NA,
      # Price
      price             = yah$price,
      prev_close        = poly$prev_close,
      vwap              = poly$vwap,
      market_cap        = fmp$market_cap,
      shares_outstanding= poly$shares_outstanding,
      # Returns
      ret_1d            = yah$ret_1d,
      ret_5d            = yah$ret_5d,
      ret_1m            = yah$ret_1m,
      ret_3m            = yah$ret_3m,
      ret_6m            = yah$ret_6m,
      ret_1y            = yah$ret_1y,
      # Technicals
      rsi14             = yah$rsi14,
      rsi_12d           = twelve$rsi_12,
      stoch_k           = twelve$stoch_k,
      stoch_d           = twelve$stoch_d,
      adx               = twelve$adx,
      macd              = yah$macd,
      macd_signal       = yah$macd_signal,
      bb_pct            = yah$bb_pct,
      sma20             = yah$sma20,
      sma50             = yah$sma50,
      sma200            = yah$sma200,
      above_ma20        = yah$above_ma20,
      above_ma50        = yah$above_ma50,
      above_ma200       = yah$above_ma200,
      golden_cross      = yah$golden_cross,
      trend_strength    = yah$trend_strength,
      range_pct         = yah$range_pct,
      # Volatility
      vol_20d           = yah$vol_20d,
      vol_60d           = yah$vol_60d,
      vol_252d          = yah$vol_252d,
      avg_vol_20d       = yah$avg_vol_20d,
      # Fundamentals (FMP)
      pe_ratio          = fmp$pe_ratio,
      pb_ratio          = fmp$pb_ratio,
      ps_ratio          = fmp$ps_ratio,
      peg_ratio         = fmp$peg_ratio,
      roe               = fmp$roe,
      roa               = fmp$roa,
      roic              = fmp$roic,
      profit_margin     = fmp$profit_margin,
      gross_margin      = fmp$gross_margin,
      operating_margin  = fmp$operating_margin,
      debt_equity       = fmp$debt_equity,
      current_ratio     = fmp$current_ratio,
      interest_coverage = fmp$interest_coverage,
      revenue_growth    = fmp$revenue_growth,
      earnings_growth   = fmp$earnings_growth,
      fcf_growth        = fmp$fcf_growth,
      eps_growth        = fmp$eps_growth,
      dcf_value         = fmp$dcf_value,
      dcf_upside        = dcf_upside,
      # Earnings (Alpha Vantage)
      eps               = av$eps,
      eps_estimate      = av$eps_est,
      eps_surprise_pct  = av$surprise_pct,
      earnings_beat_count= av$beat_count,
      # Analyst (Finnhub)
      analyst_score     = fh$analyst_score,
      strong_buy        = fh$strong_buy,
      buy               = fh$buy,
      hold              = fh$hold,
      sell              = fh$sell,
      total_analysts    = fh$total_analysts,
      beta              = fh$beta,
      short_ratio       = fh$short_ratio,
      short_pct         = fh$short_pct,
      news_score        = fh$news_score,
      # Tiingo cross-check
      adj_close_tiingo  = tiingo$adj_close,
      tiingo_ret1m      = tiingo$tiingo_ret1m,
      # SEC EDGAR verified
      sec_revenue       = edgar$sec_revenue,
      sec_net_income    = edgar$sec_net_income,
      # 52W
      high_52w          = yah$high_52w,
      low_52w           = yah$low_52w,
      # Macro
      fed_rate          = macro_env$fed_rate,
      yield_spread      = macro_env$yield_spread,
      vix               = macro_env$vix,
      regime            = macro_env$regime,
      # Data quality
      data_confidence   = confidence,
      last_updated      = as.character(Sys.Date()),
      stringsAsFactors  = FALSE
    )
  })

  today_results <- bind_rows(Filter(Negate(is.null), today_results))

  # ── MERGE WITH CACHED DATA ────────────────────────────────────────────────────
  if (!is.null(cached) && nrow(cached) > 0) {
    # Remove today's tickers from cache, replace with fresh data
    old_data <- cached[!cached$symbol %in% today_tickers, ]
    final    <- bind_rows(old_data, today_results)
  } else {
    final <- today_results
  }

  final <- final %>% arrange(symbol)

  # ── SAVE ─────────────────────────────────────────────────────────────────────
  write_csv(final, "data/fundamentals_scored.csv")
  write_csv(final, "app/fundamentals_scored.csv")
  saveRDS(list(last_run=Sys.Date(), batch=batch_idx+1),
          "data/batch_tracker.rds")
  # Also save raw price series for backtesting engine
  saveRDS(yahoo_cache, "data/price_series_cache.rds")

  message("\n=== MODULE 1 COMPLETE ===")
  message("Stocks in universe: ", nrow(final))
  message("Fetched today: ",      nrow(today_results))
  message("Regime: ",             macro_env$regime)
  message("Batch: ",              batch_idx+1, "/3")

  final
}

# Null coalescing helper
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

if (!exists("SOURCED_BY_MASTER")) fund_data <- run_module1()
