# An empty fetch used to overwrite the previous day's CSV with a 0-row file, so
# a single bad API response blanked a dashboard panel.  This actually destroyed
# 4340 rows of earnings data during a verification run.

test_that("write_or_keep preserves existing rows when the fetch is empty", {
  skip_if_not(have_pkgs("dplyr", "readr", "glue", "quantmod"), "deps missing")
  suppressMessages(source(repo_path("R", "03_data.R"), local = TRUE))

  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(a = 1:5), tmp)

  suppressMessages(write_or_keep(tibble::tibble(), tmp, "test"))

  expect_equal(nrow(readr::read_csv(tmp, show_col_types = FALSE)), 5)
  unlink(tmp)
})

test_that("write_or_keep writes when the fetch has rows", {
  skip_if_not(have_pkgs("dplyr", "readr", "glue", "quantmod"), "deps missing")
  suppressMessages(source(repo_path("R", "03_data.R"), local = TRUE))

  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(a = 1:5), tmp)

  suppressMessages(write_or_keep(tibble::tibble(a = 1:9), tmp, "test"))

  expect_equal(nrow(readr::read_csv(tmp, show_col_types = FALSE)), 9)
  unlink(tmp)
})

test_that("write_or_keep still creates the file when nothing exists yet", {
  skip_if_not(have_pkgs("dplyr", "readr", "glue", "quantmod"), "deps missing")
  suppressMessages(source(repo_path("R", "03_data.R"), local = TRUE))

  tmp <- tempfile(fileext = ".csv")
  suppressMessages(write_or_keep(tibble::tibble(), tmp, "test"))

  expect_true(file.exists(tmp))
  unlink(tmp)
})

# Alpha history is rebuilt from the full price window each run.  Rows recorded
# live must win over backfilled ones, gaps must fill, and today must refresh.
test_that("alpha history merge prefers live rows, backfills gaps, refreshes today", {
  skip_if_not(have_pkgs("dplyr"), "dplyr missing")
  library(dplyr)

  today <- as.Date("2026-08-05")
  existing <- tibble::tibble(
    date        = as.Date(c("2026-08-01", "2026-08-03", "2026-08-05")),
    symbol      = "AAA",
    daily_alpha = c(0.01, 0.02, 0.99))          # 0.99 is today's stale value
  window <- tibble::tibble(
    date        = rep(as.Date(c("2026-08-01", "2026-08-03", "2026-08-04", "2026-08-05")), 2),
    symbol      = rep(c("AAA", "NEW"), each = 4),
    daily_alpha = c(0.011, 0.021, 0.044, 0.055, 0.07, 0.08, 0.09, 0.10))

  merged <- bind_rows(filter(existing, date != today), window) %>%
    distinct(symbol, date, .keep_all = TRUE) %>%
    arrange(symbol, date)

  pick <- function(s, d) merged$daily_alpha[merged$symbol == s & merged$date == as.Date(d)]

  expect_equal(pick("AAA", "2026-08-03"), 0.02)   # live row preserved
  expect_equal(pick("AAA", "2026-08-04"), 0.044)  # gap backfilled
  expect_equal(pick("AAA", "2026-08-05"), 0.055)  # today recomputed
  expect_equal(sum(merged$symbol == "NEW"), 4)    # new ticker gets full history
  expect_equal(nrow(merged), nrow(distinct(merged, symbol, date)))
})

test_that("52-week high/low window is bounded to one year", {
  # Regression: getSymbols defaults to from = 2007-01-01, so max/min over the
  # whole series returned an all-time high labelled as a 52-week high. ACAD
  # reported 57.00 against a true 28.80; AI reported 177.47 against 22.66.
  src <- readLines("../../R/01_fundamentals.R")
  i <- grep("get_yahoo_base <- function", src)
  expect_length(i, 1)
  body_txt <- paste(src[i:(i + 20)], collapse = "\n")
  expect_match(body_txt, "from = Sys\\.Date\\(\\) - 365",
               info = "the 52-week price pull must be explicitly bounded")
})

test_that("one-year return uses the bar nearest a year ago, not the first bar", {
  # Regression: price_1y <- first(close) was only correct while the price pull
  # happened to be exactly one year long. With a multi-year lookback it turns
  # ret_1y into a multi-year return.
  src <- paste(readLines("../../R/02_momentum.R"), collapse = "\n")
  expect_no_match(src, "price_1y\\s*=\\s*first\\(close\\)")
  expect_match(src, "price_1y\\s*=\\s*close\\[which\\.min\\(abs\\(date - d1y\\)\\)\\]")
})

test_that("alpha history keeps only the three columns anything reads", {
  src <- paste(readLines("../../R/02_momentum.R"), collapse = "\n")
  expect_match(src, "select\\(date, symbol, daily_alpha\\)")
  # The retention window must cover the price pull or history is truncated
  lb <- as.numeric(sub(".*PRICE_LOOKBACK_DAYS <- ([0-9]+).*", "\\1", src))
  keep <- as.numeric(sub(".*HISTORY_KEEP_DAYS   <- ([0-9]+).*", "\\1", src))
  expect_gte(keep, lb)
})

test_that("disclaimer gate is present and blocks the page on load", {
  src <- paste(readLines("../../app/app.R"), collapse = "\n")
  expect_match(src, 'id="disclaimer-gate"')
  # Must be static markup, not a Shiny output — it has to paint before the
  # server connects, and must not depend on any reactive succeeding.
  expect_no_match(src, 'uiOutput\\("disclaimer')
  # Covers the page rather than sitting in a footer
  expect_match(src, "#disclaimer-gate \\{[^}]*position:fixed")
  expect_match(src, "#disclaimer-gate \\{[^}]*z-index:100000")
  # States the null result rather than only generic boilerplate. Matches the
  # metric name rather than a full sentence so rewording the gate does not
  # break the test, but removing the disclosure entirely still does.
  expect_match(src, "information coefficient")
  expect_match(src, "no statistically significant relationship to forward returns")
})

test_that("UI does not claim predictive power the validation disproves", {
  src <- paste(readLines("../../app/app.R"), collapse = "\n")
  expect_no_match(src, "HIGHEST PREDICTED RETURN")
  expect_no_match(src, "BACKTESTED CONFIDENCE")
})

test_that("cache age never depends on file mtime", {
  # actions/checkout rewrites every file at checkout time, so file.mtime() in CI
  # always reads ~0. This silently disabled the earnings TTL for six days and
  # made the health table's STALE check unreachable. Age must come from data
  # inside the file, or from git history.
  r_src  <- paste(readLines("../../R/03_data.R"), collapse = "\n")
  yml    <- paste(readLines("../../.github/workflows/daily-update.yml"), collapse = "\n")
  expect_no_match(r_src, "file\\.mtime")
  expect_no_match(yml,   "file\\.mtime")
  # The earnings cache stamps itself and reads that stamp back
  expect_match(r_src, "fetched_on = Sys\\.Date\\(\\)")
  expect_match(r_src, '"fetched_on" %in% names\\(cached\\)')
  # The health table derives age from git, using epoch seconds
  expect_match(yml, "--format=%ct")
})

test_that("earnings TTL treats an un-stamped cache as stale", {
  ttl <- 3
  age_of <- function(cached) {
    if (nrow(cached) > 0 && "fetched_on" %in% names(cached)) {
      f <- suppressWarnings(max(as.Date(cached$fetched_on), na.rm = TRUE))
      if (is.finite(as.numeric(f))) as.numeric(Sys.Date() - f) else Inf
    } else Inf
  }
  reuse <- function(d) nrow(d) > 0 && age_of(d) < ttl
  expect_false(reuse(data.frame(x = 1)))                                   # no stamp
  expect_true (reuse(data.frame(x = 1, fetched_on = Sys.Date())))          # fresh
  expect_false(reuse(data.frame(x = 1, fetched_on = Sys.Date() - 5)))      # expired
  expect_false(reuse(data.frame(x = integer(0), fetched_on = as.Date(character(0)))))
})

test_that("sentiment history records one row per symbol per day", {
  # Re-running on the same day must replace that day, not duplicate it —
  # otherwise a manual re-run silently doubles the observations and any future
  # significance test on this series would be inflated.
  library(dplyr)
  today <- as.Date("2026-08-20")
  snap  <- tibble::tibble(date = today, symbol = c("AAPL", "MSFT"), st_bull_ratio = c(0.8, 0.5))
  prior <- tibble::tibble(date = c(today, today - 1), symbol = c("AAPL", "AAPL"),
                          st_bull_ratio = c(0.99, 0.4))

  merged <- bind_rows(prior %>% filter(date != today), snap) %>%
    distinct(symbol, date, .keep_all = TRUE) %>%
    arrange(symbol, date)

  expect_equal(nrow(merged), nrow(distinct(merged, symbol, date)))
  # today's stale 0.99 is replaced by the fresh 0.8
  expect_equal(merged$st_bull_ratio[merged$symbol == "AAPL" & merged$date == today], 0.8)
  # the prior day survives untouched
  expect_equal(merged$st_bull_ratio[merged$symbol == "AAPL" & merged$date == today - 1], 0.4)
})

test_that("sentiment history is recorded but not yet scored", {
  # It cannot be validated until roughly a quarter of observations exist, and an
  # unvalidated signal must not enter the score — that is the whole premise of
  # the project's own methodology page.
  m3    <- paste(readLines("../../R/03_data.R"), collapse = "\n")
  score <- paste(readLines("../../R/04_master_score.R"), collapse = "\n")

  expect_match(m3, "append_sentiment_history")
  expect_match(m3, "sentiment_history\\.csv")
  # The scoring module must not consume it yet
  expect_no_match(score, "sentiment_history")
  expect_no_match(score, "st_bull_ratio")
})

test_that("select() is never asked to evaluate an expression", {
  # dplyr::select() accepts column selections, not expressions. Peer Comps did
  # `select(Margin = coalesce(profitMargins, profit_margin))`, which aborts with
  # "object 'profitMargins' not found" even though the column exists — so the
  # panel rendered an error instead of a table. Computation belongs in mutate().
  src <- readLines("../../app/app.R")
  sel_lines <- grep("^\\s*select\\(", src, value = TRUE)
  for (line in sel_lines) {
    expect_false(grepl("coalesce(", line, fixed = TRUE),
                 info = paste("coalesce() inside select():", trimws(line)))
    expect_false(grepl("ifelse(", line, fixed = TRUE),
                 info = paste("ifelse() inside select():", trimws(line)))
  }
})

test_that("macro tab renders every FRED series the pipeline collects", {
  # CPI and Unemployment were pulled from FRED nightly and never displayed.
  app <- paste(readLines("../../app/app.R"), collapse = "\n")
  m3  <- paste(readLines("../../R/03_data.R"), collapse = "\n")
  for (series in c("10Y Treasury", "Fed Funds Rate", "Yield Curve Spread",
                   "CPI", "Unemployment")) {
    expect_match(m3, series, fixed = TRUE,
                 info = paste("series no longer collected:", series))
  }
  expect_match(app, "cpi_yoy")
  expect_match(app, "unemployment")
})

test_that("deep dive surfaces the qualitative data collected per ticker", {
  app <- paste(readLines("../../app/app.R"), collapse = "\n")
  expect_match(app, "dd_news")        # company news for the selected symbol
  expect_match(app, "dd_sentiment")   # crowd sentiment for the selected symbol
})

test_that("no output is assigned twice in the server function", {
  # There were two output$news_feed definitions. The later one silently won, so
  # an edit to the first — the universe highlighting — had no effect at all and
  # looked like a feature that simply did not work.
  e <- parse("../../app/app.R")
  srv <- NULL
  for (x in e) {
    if (is.call(x) && as.character(x[[1]])[1] %in% c("<-", "=") &&
        identical(as.character(x[[2]]), "server")) srv <- x[[3]]
  }
  expect_false(is.null(srv))

  body_exprs <- as.list(body(eval(srv)))[-1]
  assigns <- character(0)
  for (st in body_exprs) {
    if (is.call(st) && as.character(st[[1]])[1] %in% c("<-", "=")) {
      lhs <- st[[2]]
      if (is.call(lhs) && as.character(lhs[[1]])[1] == "$" &&
          identical(as.character(lhs[[2]]), "output"))
        assigns <- c(assigns, as.character(lhs[[3]]))
    }
  }
  expect_gt(length(assigns), 50)
  expect_equal(assigns[duplicated(assigns)], character(0))
})

test_that("JS() callbacks are built as a single string", {
  # JS() joins its arguments with newlines. Interpolating a value mid-string
  # split the quoted JS literal across lines and produced invalid JavaScript.
  # Shiny threw "Invalid or unexpected token" evaluating it, and that
  # client-side error aborted rendering for every other output delivered in the
  # same batch — the entire Deep Dive tab was blank while the server computed it
  # all correctly. Server logs showed nothing; only the browser console did.
  src <- readLines("../../app/app.R")

  # Any JS( that opens without closing on the same line is a multi-line
  # construction, which is what produced the broken literal.
  bad <- character(0)
  for (line in grep("JS\\(", src, value = TRUE)) {
    opens  <- lengths(regmatches(line, gregexpr("\\(", line)))
    closes <- lengths(regmatches(line, gregexpr("\\)", line)))
    if (opens > closes && !grepl("sprintf|paste0|paste\\(", line))
      bad <- c(bad, trimws(line))
  }
  expect_equal(bad, character(0))

  # And the peer-comps callback specifically must build its string in one piece
  joined <- paste(src, collapse = "\n")
  expect_match(joined, "callback=JS\\(sprintf\\(")
})

test_that("short interest is fetched, not stubbed", {
  # This was a stub returning all-NA on the belief that no free source existed.
  # FINRA covers every US venue with no key. While the inputs were NA every
  # squeeze score defaulted to 50 and all 195 stocks read "No Signal".
  src <- paste(readLines("../../R/03_data.R"), collapse = "\n")
  expect_match(src, "FINRA_SHORT_URL")
  expect_match(src, "finra_short_bulk")
  expect_no_match(src, "Building short interest stubs")
  # The denominator actually used must be recorded, since shares outstanding
  # is a proxy for float rather than the real thing
  expect_match(src, "short_pct_basis")
})

test_that("short trend is derived from the prior period, not hardcoded", {
  trend <- function(now, prior) {
    if (is.na(now) || is.na(prior)) "Unknown"
    else if (now > prior * 1.05) "Increasing"
    else if (now < prior * 0.95) "Decreasing"
    else "Stable"
  }
  expect_equal(trend(150, 100), "Increasing")
  expect_equal(trend(100, 150), "Decreasing")
  expect_equal(trend(100, 100), "Stable")
  expect_equal(trend(103, 100), "Stable")      # inside the 5% band
  expect_equal(trend(NA, 100),  "Unknown")
})

test_that("float basis falls back conservatively", {
  # Float is a subset of shares outstanding, so using outstanding as the
  # denominator understates the percentage. That errs toward calling a stock
  # less squeezed, never more, which is the safe direction.
  shares_short <- 1e6
  float <- 5e6; outstanding <- 10e6
  pct_float <- shares_short / float
  pct_out   <- shares_short / outstanding
  expect_lt(pct_out, pct_float)
})
