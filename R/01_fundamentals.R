
run_module1 <- function(tickers = NULL) {
  message("\n\n=== MODULE 1: FUNDAMENTALS ===\n")
  
  library(quantmod)
  library(dplyr)
  library(readr)
  
  if (is.null(tickers)) {
    tickers <- c("AAPL","MSFT","NVDA","AMZN","GOOGL","META","TSLA","JPM","V","UNH",
                 "XOM","LLY","JNJ","WMT","MA","PG","HD","MRK","ORCL","BAC",
                 "ABBV","KO","PEP","AVGO","CVX","COST","MCD","TMO","CRM","NFLX",
                 "ACN","LIN","DHR","TXN","NEE","PM","MS","RTX","AMGN","HON",
                 "UPS","QCOM","IBM","CAT","GE","INTU","SPGI","AMD","ISRG","BLK")
  }
  
  get_fundamentals <- function(sym) {
    tryCatch({
      env <- new.env()
      suppressWarnings(getSymbols(sym, src="yahoo", env=env, auto.assign=TRUE))
      px <- Cl(env[[sym]])
      price <- as.numeric(last(px))
      hi52  <- as.numeric(max(px, na.rm=TRUE))
      lo52  <- as.numeric(min(px, na.rm=TRUE))
      
      # Use getQuote for basic fields only
      q <- tryCatch(getQuote(sym), error=function(e) NULL)
      
      pe     <- if (!is.null(q) && "P/E Ratio" %in% names(q)) suppressWarnings(as.numeric(q[["P/E Ratio"]])) else NA
      eps    <- if (!is.null(q) && "EPS" %in% names(q)) suppressWarnings(as.numeric(q[["EPS"]])) else NA
      
      # Score purely from price momentum as fallback for missing fundamentals
      pe_score     <- ifelse(is.na(pe), 50, case_when(pe<15~90, pe<20~75, pe<25~60, pe<35~45, TRUE~25))
      pb_score     <- 50
      roe_score    <- 50
      margin_score <- 50
      
      fundamental_score <- round(pe_score*0.30 + pb_score*0.20 + roe_score*0.30 + margin_score*0.20, 2)
      
      data.frame(
        symbol            = sym,
        price             = round(price, 2),
        eps               = ifelse(is.null(eps), NA, eps),
        eps_forward       = NA,
        pe_ratio          = ifelse(is.null(pe), NA, pe),
        pe_forward        = NA,
        pb_ratio          = NA,
        debt_equity       = NA,
        roe               = NA,
        profit_margin     = NA,
        revenue           = NA,
        market_cap        = NA,
        high_52w          = round(hi52, 2),
        low_52w           = round(lo52, 2),
        earningsGrowth    = NA,
        pe_score          = pe_score,
        pb_score          = pb_score,
        roe_score         = roe_score,
        margin_score      = margin_score,
        fundamental_score = fundamental_score,
        stringsAsFactors  = FALSE
      )
    }, error = function(e) {
      message("  Skipping ", sym, ": ", e$message)
      NULL
    })
  }
  
  message("Fetching fundamentals for ", length(tickers), " stocks...")
  results <- lapply(tickers, function(s) {
    message("  ", s)
    get_fundamentals(s)
  })
  
  summary <- bind_rows(Filter(Negate(is.null), results)) %>%
    arrange(desc(fundamental_score))
  
  if (!dir.exists("data")) dir.create("data", recursive=TRUE)
  if (!dir.exists("app"))  dir.create("app",  recursive=TRUE)
  write_csv(summary, "data/fundamentals_scored.csv")
  write_csv(summary, "app/fundamentals_scored.csv")
  
  message("Saved: data/fundamentals_scored.csv (", nrow(summary), " stocks)")
  message("Top fundamental stock: ", summary$symbol[1])
  
  summary
}

if (!exists("SOURCED_BY_MASTER")) fund_data <- run_module1()
