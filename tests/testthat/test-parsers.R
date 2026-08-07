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
