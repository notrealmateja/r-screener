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
stwits_data    <- load_csv("stocktwits_trending.csv")
bt_summary     <- load_csv("backtest_summary.csv")
bt_headline    <- load_csv("backtest_headline.csv")
bt_ic          <- load_csv("backtest_ic.csv")
bt_components  <- load_csv("backtest_components.csv")
bt_reversal    <- load_csv("reversal_results.csv")
stock_news     <- load_csv("stock_news.csv")
sec_filings    <- load_csv("sec_filings.csv")

meta <- tryCatch(readRDS("meta.rds"),
                 error=function(e) list(last_updated="Not yet run", n_stocks=0))

all_sectors <- c("All", if (!is.null(master_data)) sort(unique(na.omit(master_data$sector))) else character(0))
all_ratings <- c("All","Very Strong","Strong","Neutral","Weak","Very Weak")

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
# Signed percent for return columns. ret_1m/ret_3m/ret_6m are stored as decimal
# fractions (0.0501 = 5.01%), so they must be scaled by 100 before display.
fmt_ret <- function(x, d=1) ifelse(is.na(x), "N/A",
                                   paste0(ifelse(x>=0,"+",""), round(x*100, d), "%"))
fmt_chg <- function(x) {
  if (is.na(x)) return("N/A")
  col <- if(x>=0) GREEN else RED
  paste0('<span style="color:',col,'">',ifelse(x>=0,"+",""),round(x,2),'%</span>')
}
price_data <- price_history

# ── Discounted cash flow ────────────────────────────────────────────────────
# compute_dcf() was called by the Deep Dive panel but never defined anywhere in
# the repo, so its tryCatch caught "could not find function" on every render and
# the panel showed a permanent N/A beside a hardcoded "WACC Used 10.0%".
#
# Inputs now come from SEC XBRL filings (free cash flow, debt, cash), the
# fundamentals feed (beta, shares outstanding), and the live 10Y Treasury for
# the risk-free rate. Anything missing yields N/A rather than a fabricated
# number — a valuation built on defaults is worse than no valuation.
DCF_ERP        <- 0.05    # equity risk premium over the risk-free rate
DCF_TERMINAL_G <- 0.025   # long-run growth, held below the discount rate
DCF_TAX        <- 0.21    # US federal statutory rate
DCF_YEARS      <- 10   # two-stage: growth held through DCF_STAGE1, then fades
DCF_STAGE1     <- 5
DCF_DEBT_PREM  <- 0.02    # cost of debt approximated as risk-free + spread

dcf_risk_free <- function() {
  fallback <- 0.04
  if (is.null(macro_data) || !all(c("series","value","date") %in% names(macro_data)))
    return(fallback)
  v <- macro_data %>%
    filter(series == "10Y Treasury", !is.na(value)) %>%
    arrange(desc(date)) %>% slice(1) %>% pull(value)
  if (length(v) == 0 || is.na(v)) return(fallback)
  as.numeric(v) / 100
}

compute_dcf <- function(s, rf = dcf_risk_free()) {
  na_out <- list(intrinsic_value = NA_real_, upside = NA_real_, dcf_rating = "N/A",
                 wacc = NA_real_, growth = NA_real_, terminal_growth = NA_real_,
                 rf = rf, basis = "insufficient data")
  num <- function(x) {
    if (is.null(x)) return(NA_real_)
    v <- suppressWarnings(as.numeric(x[1]))
    if (length(v) == 0) NA_real_ else v
  }

  fcf    <- num(s$fcf)
  shares <- num(s$shares_outstanding)
  price  <- num(s$close)
  # A DCF on negative free cash flow has no meaningful terminal value, so those
  # names get N/A instead of a nonsense figure.
  if (is.na(fcf) || fcf <= 0 || is.na(shares) || shares <= 0 || is.na(price) || price <= 0)
    return(na_out)

  beta <- num(s$beta)
  if (is.na(beta) || beta <= 0) beta <- 1
  beta <- min(max(beta, 0.5), 2.5)

  debt <- num(s$total_debt); if (is.na(debt) || debt < 0) debt <- 0
  cash <- num(s$cash);       if (is.na(cash) || cash < 0) cash <- 0
  mcap <- num(s$market_cap)
  if (is.na(mcap) || mcap <= 0) mcap <- price * shares

  re   <- rf + beta * DCF_ERP
  rd   <- rf + DCF_DEBT_PREM
  cap  <- mcap + debt
  wacc <- if (cap > 0) (mcap / cap) * re + (debt / cap) * rd * (1 - DCF_TAX) else re
  wacc <- min(max(wacc, 0.06), 0.18)

  g <- num(s$rev_cagr)
  if (is.na(g)) g <- num(s$revenue_growth)
  if (is.na(g)) g <- 0.03
  # No company compounds at its trailing rate for five years; cap the starting
  # growth so a high-growth name cannot run away with the terminal value.
  g <- min(max(g, -0.05), 0.20)

  tg <- min(DCF_TERMINAL_G, wacc - 0.01)

  yrs  <- seq_len(DCF_YEARS)
  # Two-stage: hold the observed growth rate through stage one, then fade it
  # linearly to the terminal rate. Fading from year one instead — as this first
  # did — collapses growth almost immediately and understates every company.
  fade <- pmax(0, yrs - DCF_STAGE1) / (DCF_YEARS - DCF_STAGE1)
  gy   <- g + (tg - g) * fade
  fcfs <- fcf * cumprod(1 + gy)
  pv   <- fcfs / (1 + wacc)^yrs
  tv   <- fcfs[DCF_YEARS] * (1 + tg) / (wacc - tg)
  pvtv <- tv / (1 + wacc)^DCF_YEARS

  ev  <- sum(pv) + pvtv
  eqv <- ev - debt + cash
  if (!is.finite(eqv) || eqv <= 0) return(na_out)

  iv  <- eqv / shares
  ups <- iv / price - 1
  if (!is.finite(ups)) return(na_out)

  # Descriptive bands, not recommendations — same convention as the score bands.
  rating <- if (ups >= 0.25) "Materially above price"
       else if (ups >= 0.05) "Above price"
       else if (ups > -0.05) "In line with price"
       else if (ups > -0.25) "Below price"
       else                  "Materially below price"

  list(intrinsic_value = round(iv, 2), upside = ups, dcf_rating = rating,
       wacc = wacc, growth = g, terminal_growth = tg, rf = rf,
       basis = if (!is.na(num(s$rev_cagr))) "SEC XBRL" else "estimated growth")
}
