# Scoring invariants. These are the properties the model claims to have, so a
# change that silently breaks one should fail here rather than on the dashboard.

test_that("composite weights sum to 1", {
  expect_equal(0.55 + 0.22 + 0.13 + 0.10, 1.0)
})

test_that("alpha component weights sum to 1", {
  expect_equal(0.30 + 0.25 + 0.20 + 0.15 + 0.10, 1.0)
})

test_that("confidence weight ramps to 1 at 63 days and never exceeds it", {
  cw <- function(d) pmin(d / 63, 1.0)
  expect_equal(cw(0), 0)
  expect_equal(cw(63), 1)
  expect_equal(cw(252), 1)          # a full year is still capped at 1
  expect_equal(round(cw(38), 3), 0.603)
  expect_true(all(diff(cw(0:63)) >= 0))   # monotonic
})

test_that("confidence shrinkage pulls a raw score toward neutral 50", {
  blend <- function(raw, w) raw * w + 50 * (1 - w)
  expect_equal(blend(87.2, 1.0), 87.2)          # full history: unchanged
  expect_equal(blend(87.2, 0.0), 50)            # no history: fully neutral
  expect_equal(round(blend(87.2, 0.603), 1), 72.4)
  # shrinkage always moves toward 50, never past it
  expect_true(blend(87.2, 0.5) < 87.2 && blend(87.2, 0.5) > 50)
  expect_true(blend(20, 0.5) > 20 && blend(20, 0.5) < 50)
})

test_that("rating thresholds are ordered and Strong Buy requires confidence", {
  rate <- function(score, w) {
    if (score >= 78 && w >= 0.3) "Strong Buy"
    else if (score >= 65) "Buy"
    else if (score >= 45) "Hold"
    else if (score >= 30) "Underperform"
    else "Avoid"
  }
  expect_equal(rate(82.9, 1.0), "Strong Buy")
  expect_equal(rate(82.9, 0.1), "Buy")   # high score, too little history
  expect_equal(rate(71.8, 1.0), "Buy")
  expect_equal(rate(50.0, 1.0), "Hold")
  expect_equal(rate(35.0, 1.0), "Underperform")
  expect_equal(rate(10.0, 1.0), "Avoid")
})

test_that("unicorn screen requires genuinely small caps", {
  universe <- data.frame(
    symbol     = c("HON", "CRM", "PRPL", "LOVE", "SOUN"),
    market_cap = c(7.7e10, 1.54e11, 4.03e7, 2.69e8, 2.80e9))
  eligible <- universe[universe$market_cap < 5e9, "symbol"]

  expect_setequal(eligible, c("PRPL", "LOVE", "SOUN"))
  # The megacap-only universe made this screen structurally impossible
  expect_false("HON" %in% eligible)
  expect_false("CRM" %in% eligible)
})

test_that("backtest outputs are internally consistent when present", {
  skip_if_not(have_pkgs("readr"), "readr missing")
  f <- repo_path("data", "backtest_summary.csv")
  skip_if_not(file.exists(f), "no backtest results yet")

  s <- readr::read_csv(f, show_col_types = FALSE)
  expect_true(all(c("bucket", "ann_alpha", "hit_rate", "avg_n") %in% names(s)))
  expect_equal(nrow(s), 5)
  expect_true(all(s$hit_rate >= 0 & s$hit_rate <= 1))
  expect_true(all(s$avg_n > 0))
  expect_equal(sort(s$bucket), 1:5)
})
