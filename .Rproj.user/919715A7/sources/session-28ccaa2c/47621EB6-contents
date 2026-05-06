# =============================================================================
# 01_fundamentals.R - EdgeScreener Multi-Source Pipeline (CLEAN BUILD)
# =============================================================================

run_module1 <- function(tickers = NULL, force_refresh = FALSE) {
  message("\n=== MODULE 1: MULTI-SOURCE DATA PIPELINE ===\n")

  library(quantmod); library(dplyr); library(readr)
  library(jsonlite); library(httr); library(TTR); library(lubridate)

  FMP_KEY    <- "WEe4bM0zNn8UagrZtzyijnANOJa6qrBK"
  AV_KEY     <- "GCL8H0RSOMX48AWU"
  FH_KEY     <- "d6sdol1r01qj447b9nngd6sdol1r01qj447b9no0"
  POLYGON_KEY <- "CZ9uLCagJ4dWJMs1Efz79gA80ThXlQlO"
  TIINGO_KEY <- "8d2c7e68b5d95ecf39897ec76884bf84f65cf15d"
  TWELVE_KEY <- "46415134d68a44cb9467d9b1a45e3eea"

  if (!dir.exists("data")) dir.create("data", recursive=TRUE)
  if (!dir.exists("app"))  dir.create("app",  recursive=TRUE)

  # Safe scalar: forces any value to exactly length 1
  s1 <- function(x, default=NA) {
    if (is.null(x) || length(x) == 0) return(default)
    r <- x[[1]]
    if (is.null(r) || length(r) == 0) return(default)
    r
  }

  if (is.null(tickers)) {
    tickers <- unique(c(
      "AAPL","MSFT","NVDA","AMZN","GOOGL","META","TSLA","AVGO","ORCL","CRM",
      "ADBE","INTU","AMD","QCOM","TXN","IBM","AMAT","MU","LRCX","KLAC",
      "SNPS","CDNS","MRVL","NXPI","ON","HPQ","DELL","CSCO","INTC","ACN",
      "JPM","BAC","WFC","GS","MS","BLK","SCHW","AXP","V","MA",
      "SPGI","MCO","ICE","CME","CB","PGR","MET","PRU","TRV","AIG",
      "UNH","JNJ","LLY","ABBV","MRK","TMO","ABT","DHR","BMY","AMGN",
      "GILD","REGN","VRTX","ISRG","BSX","EW","BDX","IQV","ZBH","BAX",
      "WMT","COST","TGT","HD","LOW","MCD","SBUX","NKE","TJX","ROST",
      "BKNG","MAR","HLT","YUM","CMG","LULU","KO","PEP","PG","CL",
      "XOM","CVX","COP","EOG","SLB","MPC","PSX","VLO","OXY","HES",
      "GE","HON","CAT","DE","RTX","LMT","NOC","GD","BA","UPS",
      "FDX","CSX","UNP","NSC","EMR","ETN","PH","ROK","ITW","MMM",
      "NFLX","DIS","CMCSA","T","VZ","TMUS","NEE","DUK","SO","D","AEP","EXC",
      "SNOW","DDOG","ZS","CRWD","PANW","FTNT","NET","OKTA","MDB",
      "BILL","HUBS","VEEV","WDAY","NOW","TEAM","ZM","DOCU","TTD","ROKU",
      "SPOT","UBER","LYFT","ABNB","DASH","COIN","HOOD","SOFI","AFRM","UPST",
      "PLTR","AI","PATH","S","GTLB","CFLT","DKNG","RBLX","U","APP"
    ))
  }

  # Rolling cache
  cache_file <- "data/fundamentals_scored.csv"
  batch_file <- "data/batch_tracker.rds"
  today_num  <- as.integer(format(Sys.Date(), "%j"))

  if (!force_refresh && file.exists(cache_file) && file.exists(batch_file)) {
    tracker <- readRDS(batch_file)
    if (tracker$last_run == Sys.Date()) {
      message("Cache current. Using cached data.")
      return(invisible(read_csv(cache_file, show_col_types=FALSE)))
    }
  }
  cached <- if (file.exists(cache_file)) read_csv(cache_file, show_col_types=FALSE) else NULL

  batch_idx   <- today_num %% 3
  batch_size  <- ceiling(length(tickers) / 3)
  start_i     <- batch_idx * batch_size + 1
  end_i       <- min(start_i + batch_size - 1, length(tickers))
  today_tickers <- tickers[start_i:end_i]
  message("Batch ", batch_idx+1, "/3: ", length(today_tickers), " stocks")

  # FRED
  get_fred <- function(id) {
    tryCatch({
      d <- read_csv(paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=",id), show_col_types=FALSE)
      as.numeric(tail(na.omit(d[[2]]), 1))
    }, error=function(e) NA)
  }
  fed_rate     <- get_fred("FEDFUNDS")
  yield_spread <- tryCatch(get_fred("DGS10") - get_fred("DGS2"), error=function(e) NA)
  vix_val      <- get_fred("VIXCLS")
  regime <- if (!is.na(vix_val) && vix_val > 30)      "RISK_OFF" else
            if (!is.na(yield_spread) && yield_spread < 0) "INVERTED" else
            if (!is.na(fed_rate) && fed_rate > 5)      "HIGH_RATE" else
            if (!is.na(fed_rate) && fed_rate < 2)      "LOW_RATE"  else "NEUTRAL"
  message("Regime: ", regime, " | Fed: ", fed_rate, " | VIX: ", vix_val)

  # Yahoo Finance
  get_yahoo <- function(sym) {
    tryCatch({
      env <- new.env()
      suppressWarnings(getSymbols(sym, src="yahoo", env=env, auto.assign=TRUE, from=Sys.Date()-380))
      sym_clean <- gsub("\\.", "", sym)
      px_obj <- if (exists(sym, envir=env)) get(sym, envir=env) else
                if (exists(sym_clean, envir=env)) get(sym_clean, envir=env) else NULL
      if (is.null(px_obj)) return(NULL)
      px  <- Cl(px_obj); vol <- Vo(px_obj)
      if (nrow(px) < 60) return(NULL)
      price <- as.numeric(last(px))
      n     <- nrow(px)
      hi52  <- as.numeric(max(tail(px,252), na.rm=TRUE))
      lo52  <- as.numeric(min(tail(px,252), na.rm=TRUE))
      ret1d <- as.numeric((px[n]/px[max(1,n-1)]-1)*100)
      ret5d <- as.numeric((px[n]/px[max(1,n-5)]-1)*100)
      ret1m <- as.numeric((px[n]/px[max(1,n-21)]-1)*100)
      ret3m <- as.numeric((px[n]/px[max(1,n-63)]-1)*100)
      ret6m <- as.numeric((px[n]/px[max(1,n-126)]-1)*100)
      ret1y <- as.numeric((px[n]/px[1]-1)*100)
      rsi14 <- as.numeric(last(RSI(px, n=14)))
      sma20 <- as.numeric(last(SMA(px, n=20)))
      sma50 <- as.numeric(last(SMA(px, n=50)))
      sma200<- as.numeric(last(SMA(px, n=200)))
      macd_r <- tryCatch(as.numeric(last(MACD(px)$macd)),   error=function(e) NA)
      macd_s <- tryCatch(as.numeric(last(MACD(px)$signal)), error=function(e) NA)
      bb    <- tryCatch(last(BBands(px)), error=function(e) NULL)
      bb_pct<- if(!is.null(bb)) as.numeric((price-bb[,"dn"])/(bb[,"up"]-bb[,"dn"])) else NA
      dr    <- diff(log(as.numeric(px)))
      vol20 <- sd(tail(dr,20), na.rm=TRUE)*sqrt(252)*100
      vol60 <- sd(tail(dr,60), na.rm=TRUE)*sqrt(252)*100
      vol252<- sd(dr, na.rm=TRUE)*sqrt(252)*100
      avol  <- mean(as.numeric(tail(vol,20)), na.rm=TRUE)
      trend <- mean(tail(dr,20)>0, na.rm=TRUE)*100
      rpct  <- if(!is.na(hi52)&&hi52!=lo52) (price-lo52)/(hi52-lo52)*100 else 50
      list(
        price=round(price,2), high_52w=round(hi52,2), low_52w=round(lo52,2),
        ret_1d=round(ret1d,3), ret_5d=round(ret5d,3), ret_1m=round(ret1m,3),
        ret_3m=round(ret3m,3), ret_6m=round(ret6m,3), ret_1y=round(ret1y,3),
        rsi14=round(rsi14,2), sma20=round(sma20,2), sma50=round(sma50,2),
        sma200=round(sma200,2), macd=round(macd_r,4), macd_signal=round(macd_s,4),
        bb_pct=round(bb_pct,3), vol_20d=round(vol20,2), vol_60d=round(vol60,2),
        vol_252d=round(vol252,2), avg_vol_20d=round(avol,0),
        trend_strength=round(trend,1), range_pct=round(rpct,1),
        above_ma20=!is.na(sma20)&&price>sma20,
        above_ma50=!is.na(sma50)&&price>sma50,
        above_ma200=!is.na(sma200)&&price>sma200,
        golden_cross=!is.na(sma50)&&!is.na(sma200)&&sma50>sma200,
        px_series=as.numeric(px)
      )
    }, error=function(e) { message("  Yahoo error [",sym,"]: ",e$message); NULL })
  }

  # FMP
  get_fmp <- function(sym) {
    sg <- function(url) tryCatch({ r<-GET(url); if(status_code(r)!=200) return(NULL); fromJSON(content(r,"text",encoding="UTF-8")) }, error=function(e) NULL)
    sn <- function(df,col) { if(is.null(df)||length(df)==0||!col%in%names(df)) return(NA); suppressWarnings(as.numeric(df[[col]][1])) }
    sc <- function(df,col) { if(is.null(df)||length(df)==0||!col%in%names(df)) return(NA); as.character(df[[col]][1]) }
    rat <- sg(paste0("https://financialmodelingprep.com/api/v3/ratios-ttm/",sym,"?apikey=",FMP_KEY))
    pro <- sg(paste0("https://financialmodelingprep.com/api/v3/profile/",sym,"?apikey=",FMP_KEY))
    grw <- sg(paste0("https://financialmodelingprep.com/api/v3/financial-growth/",sym,"?limit=1&apikey=",FMP_KEY))
    dcf <- sg(paste0("https://financialmodelingprep.com/api/v3/discounted-cash-flow/",sym,"?apikey=",FMP_KEY))
    Sys.sleep(0.25)
    list(pe_ratio=sn(rat,"peRatioTTM"), pb_ratio=sn(rat,"priceToBookRatioTTM"),
         ps_ratio=sn(rat,"priceToSalesRatioTTM"), peg_ratio=sn(rat,"priceEarningsToGrowthRatioTTM"),
         roe=sn(rat,"returnOnEquityTTM"), roa=sn(rat,"returnOnAssetsTTM"),
         roic=sn(rat,"returnOnCapitalEmployedTTM"), profit_margin=sn(rat,"netProfitMarginTTM"),
         gross_margin=sn(rat,"grossProfitMarginTTM"), operating_margin=sn(rat,"operatingProfitMarginTTM"),
         debt_equity=sn(rat,"debtEquityRatioTTM"), current_ratio=sn(rat,"currentRatioTTM"),
         interest_coverage=sn(rat,"interestCoverageTTM"),
         revenue_growth=sn(grw,"revenueGrowth"), earnings_growth=sn(grw,"netIncomeGrowth"),
         fcf_growth=sn(grw,"freeCashFlowGrowth"), eps_growth=sn(grw,"epsgrowth"),
         market_cap=sn(pro,"mktCap"), sector=sc(pro,"sector"),
         industry=sc(pro,"industry"), company=sc(pro,"companyName"),
         dcf_value=sn(dcf,"dcf"))
  }

  # Alpha Vantage
  get_av <- function(sym) {
    tryCatch({
      d <- fromJSON(content(GET(paste0("https://www.alphavantage.co/query?function=EARNINGS&symbol=",sym,"&apikey=",AV_KEY)),"text",encoding="UTF-8"))
      q <- d$quarterlyEarnings
      if(is.null(q)||nrow(q)==0) return(list(eps=NA,eps_est=NA,surprise_pct=NA,beat_count=NA))
      ea <- suppressWarnings(as.numeric(q$reportedEPS[1]))
      ee <- suppressWarnings(as.numeric(q$estimatedEPS[1]))
      sp <- if(!is.na(ee)&&ee!=0) (ea-ee)/abs(ee)*100 else NA
      bc <- sum(suppressWarnings(as.numeric(q$reportedEPS[1:min(4,nrow(q))])>as.numeric(q$estimatedEPS[1:min(4,nrow(q))])),na.rm=TRUE)
      list(eps=ea, eps_est=ee, surprise_pct=round(sp,2), beat_count=bc)
    }, error=function(e) list(eps=NA,eps_est=NA,surprise_pct=NA,beat_count=NA))
  }

  # Finnhub
  get_finnhub <- function(sym) {
    tryCatch({
      rec <- fromJSON(content(GET(paste0("https://finnhub.io/api/v1/stock/recommendation?symbol=",sym,"&token=",FH_KEY)),"text",encoding="UTF-8"))
      met <- fromJSON(content(GET(paste0("https://finnhub.io/api/v1/stock/metric?symbol=",sym,"&metric=all&token=",FH_KEY)),"text",encoding="UTF-8"))
      Sys.sleep(0.1)
      sb <- if(length(rec)>0&&"strongBuy"%in%names(rec)) rec$strongBuy[1] else 0
      b  <- if(length(rec)>0&&"buy"%in%names(rec)) rec$buy[1] else 0
      h  <- if(length(rec)>0&&"hold"%in%names(rec)) rec$hold[1] else 0
      s  <- if(length(rec)>0&&"sell"%in%names(rec)) rec$sell[1] else 0
      ss <- if(length(rec)>0&&"strongSell"%in%names(rec)) rec$strongSell[1] else 0
      tot <- sb+b+h+s+ss
      as_score <- if(tot>0) round(((sb*2+b*1+h*0+s*(-1)+ss*(-2))/(tot*2))*50+50,1) else 50
      beta <- tryCatch(as.numeric(met$metric$beta), error=function(e) NA)
      sr   <- tryCatch(as.numeric(met$metric$shortRatio), error=function(e) NA)
      sp   <- tryCatch(as.numeric(met$metric$shortInterestPercentageFreeFloat), error=function(e) NA)
      list(analyst_score=as_score, beta=beta, strong_buy=sb, buy=b, hold=h, sell=s, total_analysts=tot, short_ratio=sr, short_pct=sp, news_score=NA)
    }, error=function(e) list(analyst_score=50,beta=NA,strong_buy=0,buy=0,hold=0,sell=0,total_analysts=0,short_ratio=NA,short_pct=NA,news_score=NA))
  }

  # Polygon
  get_polygon <- function(sym) {
    tryCatch({
      prev <- fromJSON(content(GET(paste0("https://api.polygon.io/v2/aggs/ticker/",sym,"/prev?adjusted=true&apiKey=",POLYGON_KEY)),"text",encoding="UTF-8"))
      Sys.sleep(0.12)
      list(prev_close=if(!is.null(prev$results))prev$results$c[1] else NA,
           vwap=if(!is.null(prev$results))prev$results$vw[1] else NA,
           prev_volume=if(!is.null(prev$results))prev$results$v[1] else NA,
           shares_outstanding=NA)
    }, error=function(e) list(prev_close=NA,vwap=NA,prev_volume=NA,shares_outstanding=NA))
  }

  # Tiingo
  get_tiingo <- function(sym) {
    tryCatch({
      url <- paste0("https://api.tiingo.com/tiingo/daily/",sym,"/prices?startDate=",Sys.Date()-30,"&resampleFreq=daily&token=",TIINGO_KEY)
      d <- fromJSON(content(GET(url,add_headers("Content-Type"="application/json","Authorization"=paste("Token",TIINGO_KEY))),"text",encoding="UTF-8"))
      Sys.sleep(0.1)
      if(is.null(d)||length(d)==0) return(list(adj_close=NA,tiingo_ret1m=NA))
      closes <- suppressWarnings(as.numeric(d$adjClose)); closes <- closes[!is.na(closes)]
      if(length(closes)<2) return(list(adj_close=NA,tiingo_ret1m=NA))
      list(adj_close=round(tail(closes,1),2), tiingo_ret1m=round((tail(closes,1)/closes[1]-1)*100,3))
    }, error=function(e) list(adj_close=NA,tiingo_ret1m=NA))
  }

  # Twelve Data
  get_twelve <- function(sym) {
    tryCatch({
      adx <- fromJSON(content(GET(paste0("https://api.twelvedata.com/adx?symbol=",sym,"&interval=1day&outputsize=1&apikey=",TWELVE_KEY)),"text",encoding="UTF-8"))
      Sys.sleep(0.15)
      list(rsi_12=NA, stoch_k=NA, stoch_d=NA, adx=tryCatch(as.numeric(adx$values$adx[1]),error=function(e)NA))
    }, error=function(e) list(rsi_12=NA,stoch_k=NA,stoch_d=NA,adx=NA))
  }

  # SEC EDGAR
  get_edgar <- function(sym) {
    tryCatch({
      mapping <- tryCatch(fromJSON(content(GET("https://www.sec.gov/files/company_tickers.json"),"text",encoding="UTF-8")),error=function(e)NULL)
      if(is.null(mapping)) return(list(sec_revenue=NA,sec_net_income=NA))
      tdf <- do.call(rbind, lapply(mapping, function(x) data.frame(cik_str=x$cik_str,ticker=x$ticker,stringsAsFactors=FALSE)))
      mr  <- tdf[toupper(tdf$ticker)==toupper(sym),]
      if(nrow(mr)==0) return(list(sec_revenue=NA,sec_net_income=NA))
      cik <- formatC(mr$cik_str[1], width=10, flag="0")
      facts <- tryCatch(fromJSON(content(GET(paste0("https://data.sec.gov/api/xbrl/companyfacts/CIK",cik,".json")),"text",encoding="UTF-8")),error=function(e)NULL)
      Sys.sleep(0.1)
      if(is.null(facts)) return(list(sec_revenue=NA,sec_net_income=NA))
      rn <- tryCatch(facts$facts$`us-gaap`$Revenues$units$USD, error=function(e)NULL)
      if(is.null(rn)) rn <- tryCatch(facts$facts$`us-gaap`$RevenueFromContractWithCustomerExcludingAssessedTax$units$USD,error=function(e)NULL)
      sr <- if(!is.null(rn)){ an<-rn[rn$form=="10-K",]; if(nrow(an)>0) tail(an$val,1) else NA } else NA
      ni <- tryCatch(facts$facts$`us-gaap`$NetIncomeLoss$units$USD, error=function(e)NULL)
      sn <- if(!is.null(ni)){ an<-ni[ni$form=="10-K",]; if(nrow(an)>0) tail(an$val,1) else NA } else NA
      list(sec_revenue=sr, sec_net_income=sn)
    }, error=function(e) list(sec_revenue=NA,sec_net_income=NA))
  }

  # Data confidence
  calc_conf <- function(yah,fmp,av,fh,poly,tiingo,twelve,edgar) {
    checks <- c(!is.na(s1(yah$price)),!is.na(s1(yah$rsi14)),!is.na(fmp$pe_ratio),
                !is.na(fmp$revenue_growth),!is.na(fmp$roe),!is.na(av$surprise_pct),
                !is.na(fh$beta),fh$total_analysts>0,!is.na(poly$vwap),
                !is.na(tiingo$adj_close),!is.na(twelve$adx),!is.na(edgar$sec_revenue))
    round(mean(checks,na.rm=TRUE)*100,1)
  }

  # Pre-fetch Yahoo for all tickers
  message("Fetching Yahoo data...")
  yahoo_cache <- lapply(today_tickers, function(sym) { message("  Yahoo: ",sym); get_yahoo(sym) })
  names(yahoo_cache) <- today_tickers

  # Get 3m returns as numeric vector safely
  ret3m_vec <- vapply(today_tickers, function(s) {
    yc <- yahoo_cache[[s]]
    if (is.null(yc) || !is.list(yc) || is.null(yc$ret_3m)) return(-999)
    v <- yc$ret_3m
    if (length(v) == 0 || !is.numeric(v)) return(-999)
    as.numeric(v[1])
  }, numeric(1))

  av_priority <- today_tickers[order(ret3m_vec, decreasing=TRUE)][1:min(25,length(today_tickers))]

  # Main loop
  message("\nProcessing ", length(today_tickers), " stocks across all sources...")
  today_results <- lapply(seq_along(today_tickers), function(i) {
    sym <- today_tickers[i]
    message("  [",i,"/",length(today_tickers),"] ",sym)
    yah <- yahoo_cache[[sym]]
    if (is.null(yah) || is.na(s1(yah$price))) { message("    Skip - no price"); return(NULL) }
    fmp    <- get_fmp(sym)
    av     <- if(sym %in% av_priority) { r<-get_av(sym); Sys.sleep(13); r } else list(eps=NA,eps_est=NA,surprise_pct=NA,beat_count=NA)
    fh     <- get_finnhub(sym)
    poly   <- get_polygon(sym)
    tiingo <- get_tiingo(sym)
    twelve <- get_twelve(sym)
    edgar  <- get_edgar(sym)
    conf   <- calc_conf(yah,fmp,av,fh,poly,tiingo,twelve,edgar)
    dcf_up <- if(!is.na(fmp$dcf_value)&&!is.na(s1(yah$price))&&s1(yah$price)>0) round((fmp$dcf_value/s1(yah$price)-1)*100,1) else NA
    tryCatch(data.frame(
      symbol=sym, company=s1(fmp$company,sym), sector=s1(fmp$sector),
      industry=s1(fmp$industry), price=s1(yah$price),
      prev_close=s1(poly$prev_close), vwap=s1(poly$vwap),
      market_cap=s1(fmp$market_cap), shares_outstanding=s1(poly$shares_outstanding),
      ret_1d=s1(yah$ret_1d), ret_5d=s1(yah$ret_5d), ret_1m=s1(yah$ret_1m),
      ret_3m=s1(yah$ret_3m), ret_6m=s1(yah$ret_6m), ret_1y=s1(yah$ret_1y),
      rsi14=s1(yah$rsi14), adx=s1(twelve$adx),
      macd=s1(yah$macd), macd_signal=s1(yah$macd_signal), bb_pct=s1(yah$bb_pct),
      sma20=s1(yah$sma20), sma50=s1(yah$sma50), sma200=s1(yah$sma200),
      above_ma20=s1(yah$above_ma20,FALSE), above_ma50=s1(yah$above_ma50,FALSE),
      above_ma200=s1(yah$above_ma200,FALSE), golden_cross=s1(yah$golden_cross,FALSE),
      trend_strength=s1(yah$trend_strength), range_pct=s1(yah$range_pct),
      vol_20d=s1(yah$vol_20d), vol_60d=s1(yah$vol_60d), vol_252d=s1(yah$vol_252d),
      avg_vol_20d=s1(yah$avg_vol_20d),
      pe_ratio=s1(fmp$pe_ratio), pb_ratio=s1(fmp$pb_ratio),
      ps_ratio=s1(fmp$ps_ratio), peg_ratio=s1(fmp$peg_ratio),
      roe=s1(fmp$roe), roa=s1(fmp$roa), roic=s1(fmp$roic),
      profit_margin=s1(fmp$profit_margin), gross_margin=s1(fmp$gross_margin),
      operating_margin=s1(fmp$operating_margin), debt_equity=s1(fmp$debt_equity),
      current_ratio=s1(fmp$current_ratio), interest_coverage=s1(fmp$interest_coverage),
      revenue_growth=s1(fmp$revenue_growth), earnings_growth=s1(fmp$earnings_growth),
      fcf_growth=s1(fmp$fcf_growth), eps_growth=s1(fmp$eps_growth),
      dcf_value=s1(fmp$dcf_value), dcf_upside=s1(dcf_up),
      eps=s1(av$eps), eps_estimate=s1(av$eps_est),
      eps_surprise_pct=s1(av$surprise_pct), earnings_beat_count=s1(av$beat_count),
      analyst_score=s1(fh$analyst_score,50), strong_buy=s1(fh$strong_buy,0),
      buy=s1(fh$buy,0), hold=s1(fh$hold,0), sell=s1(fh$sell,0),
      total_analysts=s1(fh$total_analysts,0), beta=s1(fh$beta),
      short_ratio=s1(fh$short_ratio), short_pct=s1(fh$short_pct),
      adj_close_tiingo=s1(tiingo$adj_close), tiingo_ret1m=s1(tiingo$tiingo_ret1m),
      sec_revenue=s1(edgar$sec_revenue), sec_net_income=s1(edgar$sec_net_income),
      high_52w=s1(yah$high_52w), low_52w=s1(yah$low_52w),
      fed_rate=fed_rate, yield_spread=yield_spread, vix=vix_val,
      regime=regime, data_confidence=conf,
      last_updated=as.character(Sys.Date()),
      stringsAsFactors=FALSE
    ), error=function(e) { message("    data.frame error [",sym,"]: ",e$message); NULL })
  })

  today_results <- bind_rows(Filter(Negate(is.null), today_results))

  final <- if (!is.null(cached) && nrow(cached) > 0) {
    bind_rows(cached[!cached$symbol %in% today_tickers, ], today_results)
  } else { today_results }

  final <- final %>% arrange(symbol)
  write_csv(final, "data/fundamentals_scored.csv")
  write_csv(final, "app/fundamentals_scored.csv")
  saveRDS(list(last_run=Sys.Date(), batch=batch_idx+1), "data/batch_tracker.rds")
  saveRDS(yahoo_cache, "data/price_series_cache.rds")

  message("\n=== MODULE 1 COMPLETE ===")
  message("Stocks: ", nrow(final), " | Fetched today: ", nrow(today_results), " | Regime: ", regime)
  final
}

if (!exists("SOURCED_BY_MASTER")) fund_data <- run_module1()
