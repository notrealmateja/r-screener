# Yahoo intermittently returns price bars with an interior NA.  Every TTR
# indicator aborts on those with "Series contains non-leading NAs", which halted
# the entire pipeline — it was the cause of every workflow failure in the
# project's history.  Two defences: drop incomplete bars, and guard each call.

test_that("raw TTR still aborts on interior NAs (the bug being defended against)", {
  skip_if_not(have_pkgs("TTR"), "TTR missing")
  x <- cumsum(rnorm(120, 0.1, 1)) + 100
  x[60] <- NA
  expect_error(TTR::SMA(x, 20), "non-leading NA")
})

test_that("safe_calc returns an all-NA column instead of aborting", {
  skip_if_not(have_pkgs("TTR", "dplyr", "readr"), "deps missing")
  source(repo_path("R", "02_momentum.R"), local = TRUE)

  short <- cumsum(rnorm(10, 0.1, 1)) + 100
  out <- safe_calc(TTR::SMA(short, 200), length(short))
  expect_length(out, length(short))
  expect_true(all(is.na(out)))
})

test_that("safe_calc passes a valid series through unchanged", {
  skip_if_not(have_pkgs("TTR", "dplyr", "readr"), "deps missing")
  source(repo_path("R", "02_momentum.R"), local = TRUE)

  x <- cumsum(rnorm(120, 0.1, 1)) + 100
  expect_equal(safe_calc(TTR::SMA(x, 20), length(x)), as.numeric(TTR::SMA(x, 20)))
})

test_that("safe_calc rejects a wrong-length result rather than recycling it", {
  skip_if_not(have_pkgs("dplyr", "readr", "TTR"), "deps missing")
  source(repo_path("R", "02_momentum.R"), local = TRUE)

  out <- safe_calc(c(1, 2, 3), 10)
  expect_length(out, 10)
  expect_true(all(is.na(out)))
})

test_that("dropping incomplete bars makes the indicator computable", {
  skip_if_not(have_pkgs("TTR"), "TTR missing")
  x <- cumsum(rnorm(120, 0.1, 1)) + 100
  x[60] <- NA
  clean <- x[!is.na(x)]
  v <- TTR::SMA(clean, 20)
  expect_length(v, length(clean))
  expect_true(sum(!is.na(v)) > 0)
})
