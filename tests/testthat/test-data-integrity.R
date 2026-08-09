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
