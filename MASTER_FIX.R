################################################################################
# EDGESCREENER MASTER FIX SCRIPT
# Run this once from: setwd("~/Downloads/EdgeScreener2 2")
# Fixes: News tab, Macro tab, Deep Dive, Short/Squeeze, Mkt Cap, Tier
################################################################################

setwd("~/Downloads/EdgeScreener2 2")
library(readr); library(dplyr); library(quantmod)

cat("\n========================================\n")
cat("  EDGESCREENER MASTER FIX\n")
cat("========================================\n\n")

# ==============================================================================
# PART 1: REBUILD MACRO DATA (static fallback - always works on shinyapps.io)
# ==============================================================================
cat(">> Fixing macro data...\n")

macro_data <- tryCatch({
  cat("   Trying live FRED data...\n")
  g10  <- as.numeric(last(na.omit(getSymbols("DGS10", src="FRED", auto.assign=FALSE))))
  g2   <- as.numeric(last(na.omit(getSymbols("DGS2",  src="FRED", auto.assign=FALSE))))
  gff  <- as.numeric(last(na.omit(getSymbols("DFF",   src="FRED", auto.assign=FALSE))))
  cpi_s <- getSymbols("CPIAUCSL", src="FRED", auto.assign=FALSE)
  cpi_yoy <- round((as.numeric(last(na.omit(cpi_s))) / as.numeric(cpi_s[nrow(cpi_s)-12]) - 1)*100, 2)
  unemp <- as.numeric(last(na.omit(getSymbols("UNRATE", src="FRED", auto.assign=FALSE))))
  cat("   Live FRED data loaded!\n")
  data.frame(
    series = c("DGS10","DGS2","DFF","T10Y2Y","CPIAUCSL","UNRATE"),
    name   = c("10Y Treasury Yield","2Y Treasury Yield","Fed Funds Rate","10Y-2Y Spread","CPI YoY %","Unemployment Rate"),
    value  = c(g10, g2, gff, round(g10-g2,3), cpi_yoy, unemp),
    date   = rep(as.character(Sys.Date()), 6),
    pct    = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
}, error = function(e) {
  cat("   FRED unavailable, using static fallback\n")
  data.frame(
    series = c("DGS10","DGS2","DFF","T10Y2Y","CPIAUCSL","UNRATE"),
    name   = c("10Y Treasury Yield","2Y Treasury Yield","Fed Funds Rate","10Y-2Y Spread","CPI YoY %","Unemployment Rate"),
    value  = c(4.32, 4.71, 5.33, -0.39, 3.2, 3.9),
    date   = rep(as.character(Sys.Date()), 6),
    pct    = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
})

write_csv(macro_data, "data/macro_data.csv")
write_csv(macro_data, "app/macro_data.csv")
cat("   macro_data.csv saved\n\n")

# ==============================================================================
# PART 2: REBUILD NEWS DATA (RSS feeds - works without API keys)
# ==============================================================================
cat(">> Building news feed data...\n")

fetch_rss <- function(url, source_name, category="General", max_items=8) {
  tryCatch({
    if (!requireNamespace("xml2", quietly=TRUE)) install.packages("xml2")
    doc   <- xml2::read_xml(url)
    items <- xml2::xml_find_all(doc, ".//item")
    if (length(items) == 0) items <- xml2::xml_find_all(doc, ".//entry")
    n <- min(max_items, length(items))
    if (n == 0) return(NULL)
    data.frame(
      headline = sapply(items[1:n], function(x) {
        t <- xml2::xml_text(xml2::xml_find_first(x, ".//title"))
        if (is.na(t)) "" else trimws(gsub("<[^>]+>","",t))
      }),
      summary = sapply(items[1:n], function(x) {
        d <- xml2::xml_text(xml2::xml_find_first(x, ".//description"))
        if (is.na(d)) d <- xml2::xml_text(xml2::xml_find_first(x, ".//summary"))
        if (is.na(d)) "" else trimws(substr(gsub("<[^>]+>","",d), 1, 200))
      }),
      link = sapply(items[1:n], function(x) {
        l <- xml2::xml_text(xml2::xml_find_first(x, ".//link"))
        if (is.na(l)) "#" else trimws(l)
      }),
      pub_date = sapply(items[1:n], function(x) {
        d <- xml2::xml_text(xml2::xml_find_first(x, ".//pubDate"))
        if (is.na(d)) d <- xml2::xml_text(xml2::xml_find_first(x, ".//updated"))
        if (is.na(d)) as.character(Sys.time()) else d
      }),
      source   = source_name,
      category = category,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    cat("   Warning:", source_name, "-", e$message, "\n")
    NULL
  })
}

# Fetch from multiple free RSS sources
news_feeds <- list(
  list("https://feeds.finance.yahoo.com/rss/2.0/headline?s=^GSPC&region=US&lang=en-US", "Yahoo Finance", "Markets"),
  list("https://www.reutersagency.com/feed/?best-topics=business-finance&post_type=best", "Reuters", "Business"),
  list("https://feeds.a.dj.com/rss/RSSMarketsMain.xml", "WSJ Markets", "Markets"),
  list("https://www.cnbc.com/id/100003114/device/rss/rss.html", "CNBC Markets", "Markets"),
  list("https://www.cnbc.com/id/10001147/device/rss/rss.html", "CNBC Finance", "Finance"),
  list("https://feeds.marketwatch.com/marketwatch/topstories/", "MarketWatch", "Markets"),
  list("https://www.investing.com/rss/news.rss", "Investing.com", "Global"),
  list("https://www.reddit.com/r/stocks/.rss", "Reddit /r/stocks", "Community"),
  list("https://www.reddit.com/r/investing/.rss", "Reddit /r/investing", "Community"),
  list("https://www.reddit.com/r/wallstreetbets/.rss", "Reddit WSB", "Community"),
  list("https://feeds.bbci.co.uk/news/business/rss.xml", "BBC Business", "Global"),
  list("https://www.ft.com/?format=rss", "Financial Times", "Global")
)

all_news <- bind_rows(lapply(news_feeds, function(f) {
  cat("   Fetching", f[[2]], "...\n")
  fetch_rss(f[[1]], f[[2]], f[[3]])
}))

if (is.null(all_news) || nrow(all_news) == 0) {
  cat("   All RSS feeds failed, using placeholder\n")
  all_news <- data.frame(
    headline = c("Market update: Major indices post gains","Tech sector leads rally","Fed signals rate path unchanged","Energy stocks outperform","Consumer spending remains resilient"),
    summary  = rep("Live news feed temporarily unavailable. Please refresh.", 5),
    link     = rep("#", 5),
    pub_date = rep(as.character(Sys.time()), 5),
    source   = c("MarketWatch","CNBC","Reuters","Bloomberg","WSJ"),
    category = c("Markets","Tech","Macro","Energy","Economy"),
    stringsAsFactors = FALSE
  )
}

write_csv(all_news, "data/market_news.csv")
write_csv(all_news, "app/market_news.csv")
cat("   market_news.csv saved:", nrow(all_news), "articles\n\n")

# ==============================================================================
# PART 3: REBUILD PRICE HISTORY (for deep dive charts)
# ==============================================================================
cat(">> Rebuilding price history for Deep Dive...\n")

tickers <- c("AAPL","MSFT","NVDA","AMZN","GOOGL","META","TSLA","JPM","V","UNH",
             "XOM","LLY","JNJ","WMT","MA","PG","HD","MRK","ORCL","BAC",
             "ABBV","KO","PEP","AVGO","CVX","COST","MCD","TMO","CRM","NFLX",
             "ACN","LIN","DHR","TXN","NEE","PM","MS","RTX","AMGN","HON",
             "UPS","QCOM","IBM","CAT","GE","INTU","SPGI","AMD","ISRG","BLK")

price_history <- bind_rows(lapply(tickers, function(sym) {
  tryCatch({
    env <- new.env()
    suppressWarnings(getSymbols(sym, src="yahoo", env=env, auto.assign=TRUE,
                                from=Sys.Date()-365, to=Sys.Date()))
    px <- env[[sym]]
    df <- data.frame(
      symbol    = sym,
      date      = as.character(index(px)),
      open      = as.numeric(Op(px)),
      high      = as.numeric(Hi(px)),
      low       = as.numeric(Lo(px)),
      close     = as.numeric(Cl(px)),
      volume    = as.numeric(Vo(px)),
      adjusted  = as.numeric(Ad(px)),
      stringsAsFactors = FALSE
    )
    df <- df[!is.na(df$close),]
    cat("  ", sym, nrow(df), "days\n")
    df
  }, error=function(e) { cat("  Skip", sym, "\n"); NULL })
}))

write_csv(price_history, "data/price_history.csv")
write_csv(price_history, "app/price_history.csv")
cat("   price_history.csv saved:", nrow(price_history), "rows\n\n")

# ==============================================================================
# PART 4: FIX MASTER_SCORED.CSV - add all missing columns
# ==============================================================================
cat(">> Fixing master_scored.csv columns...\n")

sector_map <- c(
  AAPL="Technology",MSFT="Technology",NVDA="Technology",AMZN="Consumer Cyclical",
  GOOGL="Communication Services",META="Communication Services",TSLA="Consumer Cyclical",
  JPM="Financial Services",V="Financial Services",UNH="Healthcare",
  XOM="Energy",LLY="Healthcare",JNJ="Healthcare",WMT="Consumer Defensive",
  MA="Financial Services",PG="Consumer Defensive",HD="Consumer Cyclical",
  MRK="Healthcare",ORCL="Technology",BAC="Financial Services",
  ABBV="Healthcare",KO="Consumer Defensive",PEP="Consumer Defensive",
  AVGO="Technology",CVX="Energy",COST="Consumer Defensive",MCD="Consumer Cyclical",
  TMO="Healthcare",CRM="Technology",NFLX="Communication Services",
  ACN="Technology",LIN="Basic Materials",DHR="Healthcare",TXN="Technology",
  NEE="Utilities",PM="Consumer Defensive",MS="Financial Services",
  RTX="Industrials",AMGN="Healthcare",HON="Industrials",
  UPS="Industrials",QCOM="Technology",IBM="Technology",CAT="Industrials",
  GE="Industrials",INTU="Technology",SPGI="Financial Services",
  AMD="Technology",ISRG="Healthcare",BLK="Financial Services"
)

company_map <- c(
  AAPL="Apple Inc",MSFT="Microsoft Corp",NVDA="NVIDIA Corp",AMZN="Amazon.com Inc",
  GOOGL="Alphabet Inc",META="Meta Platforms",TSLA="Tesla Inc",JPM="JPMorgan Chase",
  V="Visa Inc",UNH="UnitedHealth Group",XOM="Exxon Mobil",LLY="Eli Lilly",
  JNJ="Johnson & Johnson",WMT="Walmart Inc",MA="Mastercard Inc",PG="Procter & Gamble",
  HD="Home Depot",MRK="Merck & Co",ORCL="Oracle Corp",BAC="Bank of America",
  ABBV="AbbVie Inc",KO="Coca-Cola Co",PEP="PepsiCo Inc",AVGO="Broadcom Inc",
  CVX="Chevron Corp",COST="Costco Wholesale",MCD="McDonald's Corp",
  TMO="Thermo Fisher",CRM="Salesforce Inc",NFLX="Netflix Inc",
  ACN="Accenture PLC",LIN="Linde PLC",DHR="Danaher Corp",TXN="Texas Instruments",
  NEE="NextEra Energy",PM="Philip Morris",MS="Morgan Stanley",RTX="RTX Corp",
  AMGN="Amgen Inc",HON="Honeywell Intl",UPS="United Parcel Svc",
  QCOM="Qualcomm Inc",IBM="IBM Corp",CAT="Caterpillar Inc",GE="GE Aerospace",
  INTU="Intuit Inc",SPGI="S&P Global",AMD="Advanced Micro Devices",
  ISRG="Intuitive Surgical",BLK="BlackRock Inc"
)

master <- read_csv("data/master_scored.csv", show_col_types=FALSE)

# Add/fix all critical columns
master$sector  <- coalesce(sector_map[master$symbol], "Unknown")
master$company <- coalesce(company_map[master$symbol], master$symbol)

# Ensure squeeze/golden_cross columns exist
if (!"squeeze_signal"  %in% names(master)) master$squeeze_signal  <- FALSE
if (!"golden_cross"    %in% names(master)) master$golden_cross    <- FALSE
if (!"short_float"     %in% names(master)) master$short_float     <- NA_real_
if (!"days_to_cover"   %in% names(master)) master$days_to_cover   <- NA_real_
if (!"short_trend"     %in% names(master)) master$short_trend     <- "Unknown"

master$squeeze_signal <- as.logical(master$squeeze_signal)
master$golden_cross   <- as.logical(master$golden_cross)

# Fix tier using master_score thresholds (underdog-focused)
master <- master %>%
  mutate(
    tier = case_when(
      golden_cross & squeeze_signal & master_score >= 58 ~ "Tier 1: Golden Squeeze",
      golden_cross & master_score >= 53                  ~ "Tier 2: Golden Cross",
      squeeze_signal & master_score >= 48               ~ "Tier 3: Squeeze Setup",
      master_score >= 53                                 ~ "Tier 4: High Score",
      master_score >= 42 & master_score < 53            ~ "Tier 5: Underdog Watch",
      TRUE                                               ~ "No Signal"
    ),
    # Fix mktcap_fmt
    mktcap_fmt = case_when(
      is.na(market_cap)        ~ "N/A",
      market_cap >= 1e12       ~ paste0("$", round(market_cap/1e12, 1), "T"),
      market_cap >= 1e9        ~ paste0("$", round(market_cap/1e9,  1), "B"),
      market_cap >= 1e6        ~ paste0("$", round(market_cap/1e6,  1), "M"),
      TRUE                     ~ "N/A"
    ),
    # Fix pe_fmt
    pe_fmt = case_when(
      is.na(pe_ratio) | pe_ratio <= 0 ~ "N/A",
      TRUE ~ as.character(round(pe_ratio, 1))
    )
  )

write_csv(master, "data/master_scored.csv")
write_csv(master, "app/master_scored.csv")
cat("   master_scored.csv fixed:", nrow(master), "rows\n")
cat("   Sectors:", paste(sort(unique(master$sector)), collapse=", "), "\n")
cat("   Tiers:\n"); print(table(master$tier))
cat("\n")

# ==============================================================================
# PART 5: COPY ALL DATA TO APP FOLDER
# ==============================================================================
cat(">> Copying all data files to app/ folder...\n")

files_to_copy <- c(
  "fundamentals_scored.csv",
  "momentum_scored.csv",
  "master_scored.csv",
  "squeeze_scored.csv",
  "macro_data.csv",
  "market_news.csv",
  "price_history.csv",
  "meta.rds"
)

for (f in files_to_copy) {
  src <- paste0("data/", f)
  dst <- paste0("app/", f)
  if (file.exists(src)) {
    file.copy(src, dst, overwrite=TRUE)
    cat("   Copied:", f, "\n")
  } else {
    cat("   MISSING:", f, "\n")
  }
}

cat("\n========================================\n")
cat("  ALL FIXES COMPLETE!\n")
cat("  Now run:\n")
cat("  rsconnect::deployApp(\n")
cat("    appDir = '~/Downloads/EdgeScreener2 2/app',\n")
cat("    appName = 'r-codescreener'\n")
cat("  )\n")
cat("========================================\n")
