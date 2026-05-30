# global.R
# NOTE: Do NOT load tidyquant here — it pulls in Bioconductor's 'recipes'
# package which breaks rsconnect manifest parsing on shinyapps.io.
# The app only needs quantmod (for live price fallback) and zoo (for indexing).
library(shiny); library(dplyr); library(tidyr); library(ggplot2)
library(plotly); library(DT); library(quantmod); library(scales)
library(glue); library(readr); library(lubridate); library(TTR); library(zoo)

load_csv <- function(p) if (file.exists(p)) read_csv(p, show_col_types=FALSE) else NULL

master_data    <- load_csv("master_scored.csv")
price_history  <- load_csv("price_history.csv")
macro_data     <- load_csv("macro_data.csv")
earnings_data  <- load_csv("earnings_calendar.csv")
news_data      <- load_csv("market_news.csv")
sector_perf    <- load_csv("sector_performance.csv")
wsb_data       <- load_csv("wsb_trending.csv")

meta <- tryCatch(readRDS("meta.rds"),
                 error=function(e) list(last_updated="Not yet run", n_stocks=0))

all_sectors <- c("All", if (!is.null(master_data)) sort(unique(na.omit(master_data$sector))) else character(0))
all_ratings <- c("All","Strong Buy","Buy","Hold","Underperform","Avoid")

ORANGE  <- "#FF6B00"
ORANGE2 <- "#FF8C00"
BG      <- "#0A0A0A"
SURFACE <- "#111111"
SURF2   <- "#1A1A1A"
BORDER  <- "#2A2A2A"
TEXT    <- "#E8E8E8"
MUTED   <- "#666666"
GREEN   <- "#00C853"
RED     <- "#FF3D00"
YELLOW  <- "#FFD600"

fmt_mktcap <- function(x) {
  case_when(is.na(x)~"N/A", x>=1e12~paste0("$",round(x/1e12,2),"T"),
            x>=1e9~paste0("$",round(x/1e9,1),"B"), x>=1e6~paste0("$",round(x/1e6,1),"M"), TRUE~"N/A")
}
fmt_pct <- function(x, d=1) ifelse(is.na(x),"N/A", paste0(round(x*100,d),"%"))
fmt_chg <- function(x) {
  if (is.na(x)) return("N/A")
  col <- if(x>=0) GREEN else RED
  paste0('<span style="color:',col,'">',ifelse(x>=0,"+",""),round(x,2),'%</span>')
}
price_data <- price_history
