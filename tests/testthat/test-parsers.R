# StockTwits and Alpha Vantage news payloads carry nested data.frame columns
# whose shape varies between calls. Bulk-coercing the whole payload aborted with
# "Tibble columns must have compatible sizes", which silently produced 0 rows on
# every run for weeks. Both are now built from atomic vectors with guards.

make_ragged <- function(n = 3) {
  # Mirrors the real payload: atomic columns alongside nested frames and a
  # ragged list column.
  df <- data.frame(symbol = paste0("S", seq_len(n)),
                   title  = paste("Company", seq_len(n)),
                   watchlist_count = seq_len(n) * 100,
                   stringsAsFactors = FALSE)
  df$aliases      <- lapply(seq_len(n), function(i) rep("x", i))       # ragged
  df$fundamentals <- data.frame(a = seq_len(n), b = seq_len(n))        # nested df
  df
}

test_that("atomic-vector extraction survives nested and ragged columns", {
  s <- make_ragged()

  pick <- function(col, cast = as.character) {
    if (!col %in% names(s)) return(rep(NA, nrow(s)))
    v <- s[[col]]
    if (is.list(v) || length(v) != nrow(s)) return(rep(NA, nrow(s)))
    suppressWarnings(cast(v))
  }

  expect_equal(pick("symbol"), c("S1", "S2", "S3"))
  expect_equal(pick("watchlist_count", as.numeric), c(100, 200, 300))
  # ragged list column must degrade to NA, not error or recycle
  expect_true(all(is.na(pick("aliases"))))
  # a column that isn't in the payload at all
  expect_true(all(is.na(pick("does_not_exist"))))
})

test_that("missing fields yield NA rather than aborting", {
  s <- make_ragged()
  s$watchlist_count <- NULL

  pick <- function(col, cast = as.character) {
    if (!col %in% names(s)) return(rep(NA, nrow(s)))
    v <- s[[col]]
    if (is.list(v) || length(v) != nrow(s)) return(rep(NA, nrow(s)))
    suppressWarnings(cast(v))
  }
  expect_true(all(is.na(pick("watchlist_count", as.numeric))))
})

test_that("nested_first pulls the first element or falls back", {
  f <- data.frame(title = c("a", "b"), stringsAsFactors = FALSE)
  f$ticker_sentiment <- list(
    data.frame(ticker = c("AAPL", "MSFT"), stringsAsFactors = FALSE),
    data.frame()                                   # empty for the second article
  )

  nested_first <- function(col, field, default = NA_character_) {
    if (!col %in% names(f)) return(rep(default, nrow(f)))
    vapply(f[[col]], function(x) {
      if (is.data.frame(x) && nrow(x) > 0 && field %in% names(x))
        as.character(x[[field]][1]) else default
    }, character(1))
  }

  expect_equal(nested_first("ticker_sentiment", "ticker"), c("AAPL", NA))
  expect_equal(nested_first("topics", "topic", ""), c("", ""))
})

test_that("a JSON body is detected before being fed to a CSV parser", {
  # Alpha Vantage answers the EARNINGS_CALENDAR CSV endpoint with a JSON notice
  # when it refuses a request; read_csv turned that into an opaque
  # as.Date(date) failure instead of a usable message.
  is_json <- function(x) grepl("^\\s*[{\\[]", x)

  expect_true(is_json('{"Information": "rate limit"}'))
  expect_true(is_json('  [{"a":1}]'))
  expect_false(is_json("symbol,name,reportDate\nAAPL,Apple,2026-08-01"))
})

test_that("sector subsetting drops NA rows instead of creating phantoms", {
  skip_if_not(have_pkgs("dplyr"), "dplyr missing")
  df <- data.frame(symbol = c("A", "B", "C"),
                   sector = c("TECHNOLOGY", NA, "ENERGY"),
                   stringsAsFactors = FALSE)

  # base-R subsetting produces an all-NA phantom row for the NA sector
  expect_equal(nrow(df[df$sector == "TECHNOLOGY", ]), 2)
  expect_true(any(is.na(df[df$sector == "TECHNOLOGY", "symbol"])))

  # the fix
  out <- dplyr::filter(df, !is.na(sector), sector == "TECHNOLOGY")
  expect_equal(nrow(out), 1)
  expect_equal(out$symbol, "A")
})

test_that("earnings date parsing never aborts on a malformed response", {
  # Bare as.Date() aborts two ways that suppressWarnings cannot catch:
  #   logical/list column -> "do not know how to convert 'x' to class Date"
  #   unparseable text    -> "character string is not in a standard format"
  # Production hit the first and lost the whole fetch. An explicit format
  # yields NA instead, so the response gets reported rather than crashing.
  parse_date <- function(x) suppressWarnings(as.Date(as.character(x), format = "%Y-%m-%d"))

  for (bad in list(c(TRUE, NA), I(list("x", "y")), c(NA, NA),
                   c("n/a", "--"), c("<html>", "</html>"), character(0))) {
    expect_no_error(parse_date(bad))
    expect_true(all(is.na(parse_date(bad))))
  }
  # A well-formed response still parses
  good <- parse_date(c("2026-09-01", "2026-09-02"))
  expect_false(any(is.na(good)))
  expect_equal(format(good[1]), "2026-09-01")
})

test_that("checkout fetches full history so file ages are real", {
  # A shallow clone leaves one commit, so `git log -1 -- <file>` returns the
  # same timestamp for every file and the health table reported 0.1h for all of
  # them, including one six days stale.
  yml <- paste(readLines("../../.github/workflows/daily-update.yml"), collapse = "\n")
  expect_match(yml, "fetch-depth: 0")
})

test_that("an AV notice is detected regardless of its framing", {
  # The refusal does not always arrive as JSON. Checking only for a leading "{"
  # missed a payload that read_csv then shredded character-by-character across
  # the expected header — "I | n | NA | o | r | m | a", i.e. "Informa...", with
  # the lone f typed as logical FALSE, which is what aborted as.Date().
  is_notice <- function(raw) {
    m <- regmatches(raw, regexpr("(?i)(information|note|error message|premium|thank you for using)",
                                 raw, perl = TRUE))
    length(m) > 0 && nchar(raw) < 2000
  }
  expect_true(is_notice('{"Information": "This is a premium endpoint."}'))
  expect_true(is_notice("Information: premium endpoint"))
  expect_true(is_notice('{"Note": "call frequency limit"}'))
  expect_true(is_notice('{"Error Message": "invalid API call"}'))

  # A genuine payload must pass through even if it contains a trigger word,
  # which the size guard is there to ensure.
  real <- paste0("symbol,name,reportDate,fiscalDateEnding,estimate,currency,timeOfTheDay\n",
                 paste(rep("AAPL,Apple Inc,2026-09-01,2026-06-30,1.2,USD,post-market", 60),
                       collapse = "\n"))
  expect_false(is_notice(real))
})

# Alpha Vantage answers EARNINGS_CALENDAR with a notice rather than CSV when it
# refuses. The detector was gated on the response being under 2000 characters,
# so a longer notice fell through to the CSV parser, produced one unparseable
# row, and the only log line was "no parseable dates" — which left the calendar
# 17 days stale with no way to tell why.
av_is_notice <- function(raw) {
  notice <- regmatches(raw, regexpr("(?i)(information|note|error message|premium|thank you for using)",
                                    raw, perl = TRUE))
  looks_like_csv <- grepl("^\\s*symbol\\s*,", raw)
  length(notice) > 0 && !looks_like_csv
}

test_that("a short AV notice is detected", {
  expect_true(av_is_notice('{"Information": "Thank you for using Alpha Vantage"}'))
  expect_true(av_is_notice('{"Note": "call frequency exceeded"}'))
})

test_that("a notice longer than the old 2000-char guard is still detected", {
  long <- paste0('{"Information": "', paste(rep("padding text ", 300), collapse = ""), 'premium"}')
  expect_gt(nchar(long), 2000)
  expect_true(av_is_notice(long))
})

test_that("a genuine earnings CSV is not mistaken for a notice", {
  real <- paste0("symbol,name,reportDate,fiscalDateEnding,estimate,currency,timeOfTheDay\n",
                 "AAPL,APPLE INCORPORATED,2026-08-21,2026-06-30,1.2,USD,pre-market")
  expect_false(av_is_notice(real))
  # even when a row legitimately contains one of the keywords
  withnote <- paste0("symbol,name,reportDate,fiscalDateEnding,estimate,currency,timeOfTheDay\n",
                     "NOTE,NOTEWORTHY INFORMATION CORP,2026-08-21,2026-06-30,1.2,USD,")
  expect_false(av_is_notice(withnote))
})
