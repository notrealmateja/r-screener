# global.R
# NOTE: Do NOT load tidyquant here — it pulls in Bioconductor's 'recipes'
# package which breaks rsconnect manifest parsing on shinyapps.io.
# The app only needs quantmod (for live price fallback) and zoo (for indexing).
library(shiny); library(dplyr); library(tidyr); library(ggplot2)
library(plotly); library(DT); library(quantmod); library(scales)
library(glue); library(readr); library(lubridate); library(TTR); library(zoo)
library(tibble); library(xml2)   # xml2: live RSS news polling

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
DCF_DEFAULT_G  <- 0.03    # used only when filings give no revenue CAGR

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
    if (is.null(x) || length(x) == 0) return(NA_real_)
    v <- suppressWarnings(as.numeric(x[1]))
    if (length(v) == 0) NA_real_ else v
  }
  # Indexing a tibble with $ for an absent column warns and returns NULL, so
  # read by name and check existence first.
  n <- function(nm) if (nm %in% names(s)) num(s[[nm]]) else NA_real_

  fcf    <- n("fcf")
  shares <- n("shares_outstanding")
  price  <- n("close")
  # A DCF on negative free cash flow has no meaningful terminal value, so those
  # names get N/A instead of a nonsense figure.
  if (is.na(fcf) || fcf <= 0 || is.na(shares) || shares <= 0 || is.na(price) || price <= 0)
    return(na_out)

  beta <- n("beta")
  if (is.na(beta) || beta <= 0) beta <- 1
  beta <- min(max(beta, 0.5), 2.5)

  debt <- n("total_debt"); if (is.na(debt) || debt < 0) debt <- 0
  cash <- n("cash");       if (is.na(cash) || cash < 0) cash <- 0
  mcap <- n("market_cap")
  if (is.na(mcap) || mcap <= 0) mcap <- price * shares

  re   <- rf + beta * DCF_ERP
  rd   <- rf + DCF_DEBT_PREM
  cap  <- mcap + debt
  wacc <- if (cap > 0) (mcap / cap) * re + (debt / cap) * rd * (1 - DCF_TAX) else re
  wacc <- min(max(wacc, 0.06), 0.18)

  # Growth comes only from the SEC revenue CAGR. Falling back to revenue_growth
  # reintroduces the quarterly-EPS proxy this model was built to avoid: for the
  # 18 names with no SEC figure it read 533% (CRWD) and 351% (PTGX), which then
  # compounded against the growth cap and produced a 448% "upside" on Seneca
  # Foods. A flat conservative assumption is the honest stand-in.
  g <- n("rev_cagr")
  g_assumed <- is.na(g)
  if (g_assumed) g <- DCF_DEFAULT_G
  # No company compounds at its trailing rate for a decade; cap the starting
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
       growth_assumed = g_assumed,
       basis = if (g_assumed) "assumed growth" else "SEC XBRL")
}

# ── LBO return sensitivity ──────────────────────────────────────────────────
# A sponsor's view of the same company: buy the whole business with borrowed
# money, pay debt down out of cash flow, sell in five years. The output is a
# grid rather than a single IRR because the answer is dominated by two guesses —
# what you pay, and what you sell for.
LBO_YEARS    <- 5
LBO_LEVERAGE <- 5.0    # turns of EBITDA of entry debt
LBO_RATE     <- 0.09   # blended cost of that debt
LBO_TAX      <- 0.21
LBO_REINVEST <- 0.25   # capex and working capital as a share of EBITDA
LBO_GROWTH   <- c(0.00, 0.03, 0.06, 0.09)

compute_lbo <- function(s) {
  num <- function(x) {
    if (is.null(x)) return(NA_real_)
    v <- suppressWarnings(as.numeric(x[1]))
    if (length(v) == 0) NA_real_ else v
  }
  col <- function(d, nm) if (!is.null(d) && nm %in% names(d)) d[[nm]] else NULL
  ebitda <- num(col(s, "ebitda"))
  mcap   <- num(col(s, "market_cap"))
  debt   <- num(col(s, "total_debt")); if (is.na(debt) || debt < 0) debt <- 0
  cash   <- num(col(s, "cash"));       if (is.na(cash) || cash < 0) cash <- 0
  if (is.na(ebitda) || ebitda <= 0 || is.na(mcap) || mcap <= 0) return(NULL)

  ev         <- mcap + debt - cash
  entry_mult <- ev / ebitda
  if (!is.finite(entry_mult) || entry_mult <= 0 || entry_mult > 60) return(NULL)

  entry_debt <- LBO_LEVERAGE * ebitda
  equity_in  <- ev - entry_debt
  # A business already carrying more debt than the structure assumes cannot be
  # levered further; there is no sponsor equity cheque to compute a return on.
  if (equity_in <= 0) return(NULL)

  exit_mults <- round(entry_mult + c(-3, -1.5, 0, 1.5, 3), 1)
  exit_mults <- exit_mults[exit_mults > 1]
  if (length(exit_mults) == 0) return(NULL)

  irr_for <- function(g, xm) {
    d <- entry_debt
    for (yr in seq_len(LBO_YEARS)) {
      e        <- ebitda * (1 + g)^yr
      interest <- d * LBO_RATE
      sweep    <- (e - interest) * (1 - LBO_TAX) - e * LBO_REINVEST
      d        <- max(0, d - max(0, sweep))
    }
    exit_equity <- ebitda * (1 + g)^LBO_YEARS * xm - d
    if (exit_equity <= 0) return(NA_real_)
    (exit_equity / equity_in)^(1 / LBO_YEARS) - 1
  }

  grid <- lapply(exit_mults, function(xm)
    vapply(LBO_GROWTH, function(g) irr_for(g, xm), numeric(1)))
  names(grid) <- paste0(exit_mults, "x")

  list(entry_mult = round(entry_mult, 1), entry_debt = entry_debt,
       equity_in = equity_in, ev = ev, ebitda = ebitda,
       exit_mults = exit_mults, growth = LBO_GROWTH, grid = grid)
}

# ── Generated research brief ────────────────────────────────────────────────
# Assembled from the same data the rest of the page shows — scores, filings,
# valuation, sentiment — so it stays consistent with the panels above it and
# needs no model call at request time. It argues both directions on purpose:
# a brief that only makes the bull case is marketing, not research.
pitch_bullets <- function(s, filings = NULL, news_n = NA, wsb = NULL, stwits = NULL) {
  # Read by column name and check it exists first. Indexing a tibble with $ for
  # an absent column warns and yields NULL, which broke the whole panel for any
  # row that happened to be missing an optional field.
  col <- function(d, nm) if (!is.null(d) && nm %in% names(d)) d[[nm]] else NULL
  num <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_real_)
    v <- suppressWarnings(as.numeric(x[1])); if (length(v) == 0) NA_real_ else v
  }
  chr <- function(x, d = "") {
    if (is.null(x) || length(x) == 0 || is.na(x[1])) d else as.character(x[1])
  }
  n <- function(nm) num(col(s, nm))
  c_ <- function(nm, d = "") chr(col(s, nm), d)
  pc  <- function(x, dg = 1) if (is.na(x)) "n/a" else paste0(round(x * 100, dg), "%")
  ord <- function(n) {
    n <- round(n)
    sfx <- if (n %% 100 %in% 11:13) "th"
           else switch(as.character(n %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
    paste0(n, sfx)
  }

  sym     <- c_("symbol", "this stock")
  comp    <- c_("company", sym)
  sector  <- c_("sector", "its sector")
  score   <- n("master_score")
  pctl    <- n("master_percentile")
  driver  <- c_("primary_driver")
  dcf     <- tryCatch(compute_dcf(s), error = function(e) NULL)
  lbo     <- tryCatch(compute_lbo(s), error = function(e) NULL)

  supports <- character(0)
  against  <- character(0)

  # 1. Where the model ranks it
  if (!is.na(score) && !is.na(pctl)) {
    line <- glue("Composite score of {round(score,1)}, which is the ",
                 "{ord(pctl)} percentile of the 195-name universe")
    if (nzchar(driver)) line <- glue("{line}; the model attributes this mainly to {tolower(driver)}")
    if (pctl >= 60) supports <- c(supports, paste0(line, ".")) else against <- c(against, paste0(line, "."))
  }

  # 2. Valuation
  if (!is.null(dcf) && !is.na(dcf$intrinsic_value)) {
    v <- glue("Discounted cash flow puts fair value near ${dcf$intrinsic_value} against a ",
              "${round(n('close'),2)} price ({pc(dcf$upside)} difference), discounting at a ",
              "{pc(dcf$wacc)} WACC")
    if (isTRUE(dcf$growth_assumed))
      v <- glue("{v}. Filings gave no usable revenue trend, so growth is an assumption rather than an observation")
    if (!is.na(dcf$upside) && dcf$upside > 0.05) supports <- c(supports, paste0(v, "."))
    else if (!is.na(dcf$upside) && dcf$upside < -0.05) against <- c(against, paste0(v, "."))
  } else {
    against <- c(against, glue("No discounted cash flow value: {sym} does not report the ",
                               "positive free cash flow the model requires, so the valuation ",
                               "case rests on multiples alone."))
  }

  pe <- n("pe_ratio"); ee <- n("ev_ebitda")
  if (!is.na(pe) || !is.na(ee)) {
    m <- glue("Trades at {ifelse(is.na(pe),'n/a',paste0(round(pe,1),'x'))} earnings and ",
              "{ifelse(is.na(ee),'n/a',paste0(round(ee,1),'x'))} EV/EBITDA")
    # A fixed multiple threshold means nothing across sectors — 12x is cheap for
    # software and dear for a utility. Compare against the sector's own median.
    peer_med <- NA_real_
    if (exists("master_data") && !is.null(master_data) &&
        all(c("sector", "ev_ebitda") %in% names(master_data))) {
      pm <- master_data$ev_ebitda[master_data$sector == sector &
                                 !is.na(master_data$ev_ebitda) &
                                 master_data$ev_ebitda > 0]
      if (length(pm) >= 3) peer_med <- stats::median(pm)
    }
    if (!is.na(ee) && ee > 0 && !is.na(peer_med)) {
      rel <- ee / peer_med - 1
      m <- glue("{m}, against a {round(peer_med,1)}x median for {tolower(sector)} ",
                "({ifelse(rel >= 0, 'a premium of ', 'a discount of ')}{pc(abs(rel), 0)})")
      if (rel < 0) supports <- c(supports, paste0(m, ".")) else against <- c(against, paste0(m, "."))
    } else if (!is.na(ee) && ee > 0 && ee < 12) {
      supports <- c(supports, paste0(m, "."))
    } else {
      against <- c(against, paste0(m, "."))
    }
  }

  # 3. Momentum
  r3 <- n("ret_3m"); r1y <- n("ret_1y")
  if (!is.na(r3)) {
    m <- glue("Price is {pc(r3)} over three months and {pc(r1y)} over a year")
    zone <- c_("rsi_zone")
    if (nzchar(zone)) m <- glue("{m}, with RSI in {tolower(zone)} territory")
    if (r3 > 0) supports <- c(supports, paste0(m, ".")) else against <- c(against, paste0(m, "."))
  }

  # 4. What the crowd and the tape say
  sf <- n("short_percent_float")
  if (!is.na(sf) && sf > 0) {
    m <- glue("Short interest is {pc(sf)} of float")
    tier <- c_("squeeze_tier")
    if (nzchar(tier) && tier != "No Signal") m <- glue("{m}; squeeze screen flags it as {tier}")
    if (sf > 0.10) against <- c(against, glue("{m}. Heavy short interest cuts both ways: it is ",
                                              "fuel for a squeeze and a signal that informed ",
                                              "sellers are positioned against the name."))
    else supports <- c(supports, paste0(m, "."))
  }
  if (!is.null(wsb) && nrow(wsb) > 0) {
    mentions <- num(col(wsb, "mentions"))
    if (!is.na(mentions) && mentions > 0)
      supports <- c(supports, glue("Retail attention is measurable: {round(mentions)} r/wallstreetbets ",
                                   "mentions in the last day. This carries no weight in the score."))
  }

  # 5. What management is actually doing, from filings
  if (!is.null(filings) && nrow(filings) > 0) {
    ins <- num(col(filings, "insider_filings_90d"))
    ev1 <- num(col(filings, "material_events_1y"))
    mna <- isTRUE(as.logical(num(col(filings, "merger_activity_1y")) == 1 |
                            identical(chr(col(filings, "merger_activity_1y")), "TRUE")))
    if (!is.na(ins) && ins > 0) {
      m <- glue("{round(ins)} insider transaction reports (Form 4) in the last 90 days")
      if (ins >= 10) supports <- c(supports, glue("{m} — unusually active, though the filings ",
                                                  "count transactions without telling you whether ",
                                                  "they were purchases or sales."))
      else supports <- c(supports, paste0(m, "."))
    }
    if (mna) supports <- c(supports, glue("An S-4 was filed in the past year, which accompanies a ",
                                          "merger or acquisition — the clearest public signal that ",
                                          "{comp} has been buying or combining."))
    if (!is.na(ev1) && ev1 >= 12)
      against <- c(against, glue("{round(ev1)} material-event disclosures (8-K) in a year is a high ",
                                 "rate of unscheduled news, which tends to mean an unsettled story."))
  }
  if (!is.na(news_n) && news_n > 0)
    supports <- c(supports, glue("{news_n} recent stories are linked below for primary reading."))

  # 6. The sponsor's view
  if (!is.null(lbo)) {
    mid <- lbo$grid[[ceiling(length(lbo$grid) / 2)]][2]
    if (!is.na(mid))
      supports <- c(supports, glue("On an LBO framing — {LBO_LEVERAGE}x leverage, five-year hold, ",
                                   "exit at today's {lbo$entry_mult}x — the sponsor return lands near ",
                                   "{pc(mid, 0)} IRR with 3% EBITDA growth."))
  }

  # A brief that lists only strengths is marketing. Where the data genuinely
  # raises nothing against a name, say that plainly and carry the standing
  # caveat instead of presenting an unopposed case.
  if (length(against) == 0)
    against <- glue("Nothing in the current data argues the other side for {sym}, which is ",
                    "itself worth treating carefully. The composite score has not ",
                    "demonstrated statistically significant predictive power in walk-forward ",
                    "testing — see Methodology — so a clean read here is weaker evidence ",
                    "than it appears.")
  if (length(supports) == 0)
    supports <- glue("Nothing in the current data argues in favour of {sym} on these measures.")

  list(symbol = sym, company = comp, sector = sector,
       supports = supports, against = against, dcf = dcf, lbo = lbo)
}

# ── Live market news ────────────────────────────────────────────────────────
# The news CSV is regenerated nightly, so re-reading it on a timer would show
# the same stories all day. This pulls the RSS feeds directly so the panel is
# genuinely live. Results are cached for LIVE_NEWS_TTL seconds and shared across
# sessions, so twenty viewers polling every 30s still produce one fetch.
LIVE_NEWS_TTL <- 60
# Yahoo's site-wide news index answers 429 to server-side polling, so it is not
# in this list — its per-ticker feed is still used by the nightly pipeline. A
# feed that fails is dropped for that cycle rather than blanking the panel.
LIVE_NEWS_FEEDS <- c(
  "CNBC"        = "https://search.cnbc.com/rs/search/combinedcms/view.xml?partnerId=wrss01&id=100003114",
  "MarketWatch" = "https://feeds.content.dowjones.io/public/rss/mw_topstories",
  "Investing"   = "https://www.investing.com/rss/news_25.rss"
)
# WSJ's markets feed is deliberately absent: it still serves, but its newest
# item is dated January 2025, so every story it returns sorts to the bottom and
# is dropped anyway. A stale feed is worse than no feed — it looks like coverage.

# Feeds do not agree on a date format. RFC-822 is the RSS standard, but
# Investing.com emits plain "2026-08-22 17:04:56". Parsing only the standard
# left those items with no timestamp, so they sorted last and were cut by the
# item cap — the feed looked broken when it was simply mis-parsed.
LIVE_NEWS_DATE_FORMATS <- c("%a, %d %b %Y %H:%M:%S", "%Y-%m-%d %H:%M:%S",
                            "%Y-%m-%dT%H:%M:%S", "%d %b %Y %H:%M:%S")

parse_feed_time <- function(x) {
  out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "GMT")
  for (f in LIVE_NEWS_DATE_FORMATS) {
    miss <- is.na(out)
    if (!any(miss)) break
    out[miss] <- suppressWarnings(as.POSIXct(x[miss], format = f, tz = "GMT"))
  }
  out
}

.live_news_cache <- new.env(parent = emptyenv())
.live_news_cache$at <- NULL
.live_news_cache$df <- NULL

live_news_parse <- function(url, source) {
  tryCatch({
    if (!requireNamespace("xml2", quietly = TRUE)) return(NULL)
    doc <- xml2::read_xml(url)
    items <- xml2::xml_find_all(doc, "//item")
    if (length(items) == 0) return(NULL)
    pick <- function(node, tag) {
      v <- xml2::xml_text(xml2::xml_find_first(node, tag))
      if (length(v) == 0 || is.na(v)) NA_character_ else trimws(v)
    }
    tibble::tibble(
      title     = vapply(items, pick, character(1), "title"),
      url       = vapply(items, pick, character(1), "link"),
      published = vapply(items, pick, character(1), "pubDate"),
      source    = source
    )
  }, error = function(e) NULL)
}

get_live_news <- function(limit = 40, force = FALSE) {
  now <- as.numeric(Sys.time())
  if (!force && !is.null(.live_news_cache$at) &&
      (now - .live_news_cache$at) < LIVE_NEWS_TTL && !is.null(.live_news_cache$df)) {
    return(.live_news_cache$df)
  }
  out <- lapply(seq_along(LIVE_NEWS_FEEDS), function(i)
    live_news_parse(unname(LIVE_NEWS_FEEDS[i]), names(LIVE_NEWS_FEEDS)[i]))
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0) {
    # Never blank the panel on a transient failure — keep whatever is cached.
    return(.live_news_cache$df)
  }
  df <- dplyr::bind_rows(out)
  df <- df %>%
    dplyr::filter(!is.na(title), nzchar(title)) %>%
    dplyr::mutate(ts = parse_feed_time(published)) %>%
    dplyr::distinct(title, .keep_all = TRUE) %>%
    dplyr::arrange(dplyr::desc(ts)) %>%
    utils::head(limit)
  .live_news_cache$at <- now
  .live_news_cache$df <- df
  df
}

# ── Live television ─────────────────────────────────────────────────────────
# Channel-based embed URLs, not video IDs: a live stream's video ID changes
# every time the broadcaster restarts it, so a hardcoded ID goes dead within
# days. /embed/live_stream?channel=<id> always resolves to whatever that
# channel is airing now. All four IDs verified against the live channel pages.
TV_CHANNELS <- c(
  "Bloomberg TV"  = "UCIALMKvObZNtJ6AmdCLP7Lg",
  "Yahoo Finance" = "UCEAZeUIeJs0IjQiqTCdVSIg",
  "CNBC"          = "UCvJJ_dzjViJCoLf5uKUTwoA",
  "Reuters"       = "UChqUTb7kYRX8-EiaN3XFrSQ"
)

tv_embed_url <- function(channel) {
  id <- TV_CHANNELS[[channel]]
  # mute=1 because browsers block autoplay with sound; playsinline keeps it in
  # the panel on mobile rather than going fullscreen.
  paste0("https://www.youtube.com/embed/live_stream?channel=", id,
         "&autoplay=1&mute=1&playsinline=1")
}
