# write_history_or_keep exists because a SHORT upstream pull is unrecoverable.
#
# tq_get in R/02_momentum.R is unchecked. When Yahoo throttles it returns a
# handful of recent sessions for every symbol rather than nothing, so the frame
# is non-empty and every emptiness guard passes it through. safe_calc then
# turns ma200/rsi14/macd/bb into NA for want of periods, the write lands, and
# the next run pulls the same short window and writes it again — three
# calendar years of history gone, with no path back.
#
# The false-positive case is as important as the true one. This guard refuses
# writes, so a threshold that trips on a normal night would freeze the history
# permanently and no price would ever update again. Hence the normal-day and
# legitimate-shrinkage tests below.

source(repo_path("R", "00_utils.R"))

# A plausible history: n symbols x bars sessions ending at `last`.
hist_frame <- function(n_sym = 195, bars = 751, last = Sys.Date()) {
  syms <- sprintf("S%03d", seq_len(n_sym))
  do.call(rbind, lapply(syms, function(s) data.frame(
    symbol = s,
    date   = seq(last - bars + 1, last, by = "day"),
    close  = seq_len(bars) / 10,
    stringsAsFactors = FALSE)))
}

# Run the guard in a scratch dir; return whether it wrote, plus any warning.
try_write <- function(prev, incoming, ...) {
  d <- file.path(tempdir(), paste0("hg-", basename(tempfile(""))))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  old <- setwd(d); on.exit({ setwd(old); unlink(d, recursive = TRUE) }, add = TRUE)
  p <- "price_history.csv"
  if (!is.null(prev)) readr::write_csv(prev, p)
  warn <- NULL
  wrote <- withCallingHandlers(
    write_history_or_keep(incoming, p, ...),
    warning = function(w) { warn <<- conditionMessage(w); invokeRestart("muffleWarning") })
  on_disk <- if (file.exists(p))
    nrow(readr::read_csv(p, show_col_types = FALSE, progress = FALSE)) else 0
  list(wrote = isTRUE(wrote), warning = warn, rows_on_disk = on_disk)
}

test_that("a first run with no file on disk writes", {
  inc <- hist_frame(5, 50)
  r <- try_write(NULL, inc)
  expect_true(r$wrote)
  expect_null(r$warning)
  expect_equal(r$rows_on_disk, nrow(inc))
})

test_that("an ordinary night writes", {
  # Yesterday's file, then today's with one more session per symbol.
  prev <- hist_frame(195, 751, Sys.Date() - 1)
  inc  <- hist_frame(195, 752, Sys.Date())
  r <- try_write(prev, inc)
  expect_true(r$wrote)
  expect_null(r$warning)
})

test_that("legitimate trimming still writes", {
  # HISTORY_KEEP_DAYS trims the far end, so the file can shrink a little every
  # night. A guard that blocked that would freeze the history forever.
  prev <- hist_frame(195, 751, Sys.Date() - 1)
  inc  <- hist_frame(195, 715, Sys.Date())     # ~5% shorter
  r <- try_write(prev, inc)
  expect_true(r$wrote)
  expect_null(r$warning)
})

test_that("a throttled short pull is refused and the history survives", {
  # The real failure: Yahoo returns ~20 recent sessions for every symbol.
  prev <- hist_frame(195, 751, Sys.Date() - 1)
  inc  <- hist_frame(195, 20,  Sys.Date())
  r <- try_write(prev, inc)
  expect_false(r$wrote)
  expect_match(r$warning, "refusing to overwrite")
  expect_match(r$warning, "median 20 bars/symbol replacing 751")
  # The point of the whole exercise: the good data is still there.
  expect_equal(r$rows_on_disk, nrow(prev))
})

test_that("a truncation masked by a grown universe is still refused", {
  # Row TOTAL is the misleading number here: 900 symbols x 200 bars is MORE
  # rows than 195 x 751, so a row-count test alone passes it. The median bars
  # per symbol is what exposes it.
  prev <- hist_frame(195, 751, Sys.Date() - 1)
  inc  <- hist_frame(900, 200, Sys.Date())
  expect_gt(nrow(inc), nrow(prev))
  r <- try_write(prev, inc)
  expect_false(r$wrote)
  expect_match(r$warning, "bars/symbol")
  expect_equal(r$rows_on_disk, nrow(prev))
})

test_that("a newest date moving backwards is refused", {
  # No healthy pull ever does this; it means the fetch returned an older
  # window than the file already holds.
  prev <- hist_frame(195, 751, Sys.Date())
  inc  <- hist_frame(195, 751, Sys.Date() - 30)
  r <- try_write(prev, inc)
  expect_false(r$wrote)
  expect_match(r$warning, "older than")
  expect_equal(r$rows_on_disk, nrow(prev))
})

test_that("an empty pull is refused rather than blanking the file", {
  prev <- hist_frame(195, 751, Sys.Date() - 1)
  inc  <- prev[0, , drop = FALSE]
  r <- try_write(prev, inc)
  expect_false(r$wrote)
  expect_equal(r$rows_on_disk, nrow(prev))
})

test_that("the module calls the guard rather than write_csv", {
  src <- readLines(repo_path("R", "02_momentum.R"), warn = FALSE)
  code <- grep("^\\s*#", src, invert = TRUE, value = TRUE)
  at <- grep("price_history\\.csv", code)
  expect_gt(length(at), 0L)
  # A bare write_csv to price_history.csv is the bug this guard replaces, so
  # its return would silently reintroduce it.
  expect_false(any(grepl("write_csv\\s*\\(", code[at])),
               info = paste(code[at], collapse = " | "))
  expect_true(any(grepl("write_history_or_keep", code, fixed = TRUE)))
})

test_that("a collapsed universe with intact per-symbol history is refused", {
  # Isolates the row-count check. Here each surviving symbol keeps its full
  # 751 bars, so the median-bars test is satisfied and only the row total
  # exposes the loss: 20 symbols instead of 195 is a failed fetch, not a
  # universe that shrank by 90% overnight.
  #
  # Mutation testing added this case: with only the throttle and grown-universe
  # scenarios present, deleting the row-count check entirely still passed,
  # because the median-bars check caught both of them.
  prev <- hist_frame(195, 751, Sys.Date() - 1)
  inc  <- hist_frame(20,  751, Sys.Date())
  expect_equal(stats::median(as.numeric(table(inc$symbol))),
               stats::median(as.numeric(table(prev$symbol))))
  r <- try_write(prev, inc)
  expect_false(r$wrote)
  expect_match(r$warning, "rows replacing")
  expect_equal(r$rows_on_disk, nrow(prev))
})
