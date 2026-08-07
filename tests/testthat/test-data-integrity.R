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
