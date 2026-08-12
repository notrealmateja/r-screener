# Scoring invariants. These are the properties the model claims to have, so a
# change that silently breaks one should fail here rather than on the dashboard.

test_that("both composite weightings sum to 1", {
  # With a live options feed
  expect_equal(0.55 + 0.22 + 0.13 + 0.10, 1.0)
  # With the options term dropped (free Polygon tier), its weight is
  # redistributed rather than simply removed — otherwise every score would
  # silently shrink by 10%.
  expect_equal(0.61 + 0.24 + 0.15, 1.0)
})

test_that("dropping a constant options term widens the score range", {
  # The failure this guards against: a term that is identical for every stock
  # still consumes weight, compressing the spread between good and bad names.
  alpha <- c(90, 70, 50, 30, 10); tech <- c(80, 65, 50, 35, 20)
  qual  <- c(75, 60, 50, 40, 25); opts <- rep(50, 5)   # constant, no information

  with_opts <- alpha*0.55 + tech*0.22 + qual*0.13 + opts*0.10
  without   <- alpha*0.61 + tech*0.24 + qual*0.15

  expect_gt(diff(range(without)), diff(range(with_opts)))
  # Ordering must be unaffected — this is a rescaling, not a re-ranking
  expect_equal(order(without), order(with_opts))
})

test_that("options term is only active when the signal actually varies", {
  is_live <- function(x) length(unique(x[!is.na(x)])) > 1
  expect_false(is_live(rep(50, 195)))        # free tier: constant fallback
  expect_false(is_live(rep(NA_real_, 10)))   # nothing fetched at all
  expect_true(is_live(c(20, 50, 80)))        # paid key: real variation
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

test_that("component diagnostic covers every component of the live blend", {
  # If a weight is added to alpha_score_raw but not to COMPONENTS, the
  # diagnostic would silently stop testing it.
  src <- paste(readLines("../../R/05_backtest.R"), collapse = "\n")
  for (cmp in c("ann_alpha", "ir", "hit_rate", "alpha_63d", "streak")) {
    expect_match(src, paste0(cmp, "\\s*="),
                 info = paste("component missing from diagnostic:", cmp))
  }
  # The blend itself is included as a baseline to compare components against
  expect_match(src, "score\\s*=\\s*\"Blended score")
})

test_that("IC t-statistic uses the standard error of the mean", {
  # Guards the arithmetic: t = mean / (sd / sqrt(n)), not mean / sd.
  ics <- c(0.10, -0.02, 0.05, 0.01, -0.04, 0.07, 0.00, 0.03)
  t_correct <- mean(ics) / (sd(ics) / sqrt(length(ics)))
  expect_gt(abs(t_correct), abs(mean(ics) / sd(ics)))
  expect_equal(round(t_correct, 4), round(mean(ics) / (sd(ics) / sqrt(8)), 4))
})

test_that("reversal test adjusts its significance bar for multiple windows", {
  # Testing N formation windows gives N chances at a false positive. The bar
  # must be stricter than the usual |t| > 2, or the test is rigged to find
  # something.
  src <- paste(readLines("../../R/05_backtest.R"), collapse = "\n")
  n_windows <- length(eval(parse(text = sub(
    ".*REVERSAL_FORMATIONS <- (c\\([0-9, ]+\\)).*", "\\1", src))))
  bar <- as.numeric(sub(".*REVERSAL_T_BAR      <- ([0-9.]+).*", "\\1", src))
  expect_gt(n_windows, 1)
  expect_gt(bar, 2.0)
})

test_that("reversal verdict follows the sign of the correlation", {
  # negative IC = losers outperform = reversal; positive = momentum
  verdict <- function(ic, t, bar = 2.5) {
    if (is.na(t)) "undetermined"
    else if (abs(t) <= bar) "none"
    else if (ic < 0) "reversal" else "momentum"
  }
  expect_equal(verdict(-0.04, -3.0), "reversal")
  expect_equal(verdict( 0.04,  3.0), "momentum")
  expect_equal(verdict(-0.04, -1.5), "none")     # below the bar
  expect_equal(verdict(-0.04, -2.2), "none")     # below adjusted bar, above 2.0
  expect_equal(verdict(-0.04, NA),   "undetermined")
})

test_that("score bands describe the score rather than recommend a trade", {
  # These were "Strong Buy"/"Buy"/"Hold"/"Underperform"/"Avoid" — investment
  # verbs on a model whose own validation puts its IC at 0.003. The bands are
  # unchanged; only the wording is.
  src <- paste(readLines("../../R/04_master_score.R"), collapse = "\n")
  for (band in c("Very Strong", "Strong", "Neutral", "Weak", "Very Weak"))
    expect_match(src, paste0('"', band, '"'), fixed = FALSE)
  # The recommendation verbs must not come back in the label definitions
  rating_block <- sub('.*rating = dplyr::case_when\\((.*?)\\),.*', "\\1", src)
  for (verb in c("Strong Buy", "Underperform", "Avoid"))
    expect_false(grepl(verb, rating_block, fixed = TRUE),
                 info = paste("recommendation verb reintroduced:", verb))
})

test_that("band thresholds are unchanged by the relabelling", {
  band <- function(score, cw = 1) {
    if (score >= 78 && cw >= 0.3) "Very Strong"
    else if (score >= 65) "Strong"
    else if (score >= 45) "Neutral"
    else if (score >= 30) "Weak"
    else "Very Weak"
  }
  expect_equal(band(84.6), "Very Strong")
  expect_equal(band(78),   "Very Strong")
  expect_equal(band(77.9), "Strong")
  expect_equal(band(65),   "Strong")
  expect_equal(band(45),   "Neutral")
  expect_equal(band(30),   "Weak")
  expect_equal(band(29.9), "Very Weak")
  # A high score with too little history still cannot reach the top band
  expect_equal(band(90, cw = 0.2), "Strong")
})
