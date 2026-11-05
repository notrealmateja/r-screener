# Two published signals that were computed every night and dead on arrival.
#
# 1. build_squeeze_score stubbed its three fundamental-improvement flags to
#    FALSE. fi_score carries the largest of the four weights (0.40), so the
#    composite could not exceed 0.25*100 + 0.20*100 + 0.15*80 = 57. Every
#    threshold reading it was unreachable — the 60 and 80 tier cuts, the
#    squeeze_candidate gate, the app KPI tile and High-Conviction panel — and
#    the live universe topped out at 53.4 with all 195 names in the bottom two
#    tiers. The 1.20 squeeze_mult was dead for the same reason: it tests
#    fundamentals_improving, which was FALSE by construction.
#
# 2. confidence_weight was keyed to days_tracked, which counts rows stored in
#    alpha_history. merge_history deliberately preserves rows for symbols the
#    price pull never reached, so a symbol whose feed had gone thin kept full
#    weight off stale rows. QRVO shipped with 5 fresh bars behind 711 stored
#    ones: weight 1.00, alpha_score 67.7, 70th percentile, badged Proven and
#    Alpha Leader. Worse, with no usable beta every date fell through to the
#    raw-excess fallback, so it was ranked on the exact measure this project
#    abandoned on 2026-09-23 for rewarding market exposure as skill.
#
# These tests execute the shipped code rather than restating it: the squeeze
# tests call build_squeeze_score, and the confidence tests pull the actual
# expressions out of run_module2's AST and evaluate them.

decomment <- function(x) sub("#.*$", "", x)

# Locate the call inside `expr` that passes `argname` as a named argument, so
# the test evaluates shipped code instead of a copy of it.
find_arg <- function(expr, argname) {
  found <- NULL
  walk <- function(e) {
    if (!is.null(found) || !is.call(e)) return(invisible())
    nm <- names(e)
    if (!is.null(nm) && argname %in% nm) { found <<- e; return(invisible()) }
    # An empty argument (as in x[, 1]) is the empty symbol, and merely touching
    # it raises a missing-argument error; skip those rather than fail the walk.
    for (i in seq_along(e))
      tryCatch({ el <- e[[i]]; if (!is.null(el)) walk(el) }, error = function(x) NULL)
  }
  walk(expr)
  found
}

# ── 1. Squeeze score ────────────────────────────────────────────────────────

if (have_pkgs("dplyr", "tibble", "readr", "glue", "httr", "jsonlite")) {
  suppressMessages({ library(dplyr); library(tibble) })
  SOURCED_BY_MASTER <- TRUE
  suppressMessages(try(source(repo_path("R", "03_data.R"), local = TRUE),
                       silent = TRUE))

  # A 20-name cross-section. Short interest rises across the book so the
  # percent_rank terms span their full range; improvement_count is driven to
  # each of 0/1/2/3 explicitly rather than left to where three seq() lines
  # happen to cross zero.
  n_fix <- 20
  cnt_fix <- rep(0:3, length.out = n_fix)
  short_fix <- tibble(
    symbol              = sprintf("S%02d", seq_len(n_fix)),
    short_percent_float = seq(0.01, 0.20, length.out = n_fix),
    short_ratio         = seq(0.50, 12.0, length.out = n_fix),
    short_trend         = rep(c("Increasing", "Stable", "Stable", "Decreasing"),
                              length.out = n_fix))
  fund_fix <- tibble(
    symbol         = sprintf("S%02d", seq_len(n_fix)),
    revenue_growth = ifelse(cnt_fix >= 1,  0.12, -0.08),
    earningsGrowth = ifelse(cnt_fix >= 2,  0.20, -0.15),
    profit_margin  = ifelse(cnt_fix >= 3,  0.09, -0.04))

  sq <- if (exists("build_squeeze_score"))
    suppressMessages(build_squeeze_score(short_fix, fund_fix)) else NULL

  test_that("the fundamental-improvement term is computed, not stubbed", {
    skip_if_not(exists("build_squeeze_score"), "R/03_data.R not sourced")
    skip_if(is.null(sq), "build_squeeze_score returned NULL")
    # The regression was a constant. Any constant fails here, including the
    # all-zero one that capped the composite at 57.
    expect_gt(dplyr::n_distinct(sq$improvement_count), 1)
    expect_gt(dplyr::n_distinct(sq$fi_score), 1)
    expect_true(any(sq$fi_score > 0))
    expect_setequal(sort(unique(sq$improvement_count)), 0:3)
    expect_true(any(sq$fundamentals_improving))
  })

  test_that("the composite reaches the tiers its own thresholds name", {
    skip_if_not(exists("build_squeeze_score"), "R/03_data.R not sourced")
    skip_if(is.null(sq), "build_squeeze_score returned NULL")
    # 57 was the old hard ceiling; 80 is the High Conviction cut.
    expect_gt(max(sq$squeeze_score), 57)
    expect_gte(max(sq$squeeze_score), 80)
    expect_true(all(c("High Conviction", "Watch List", "Low Signal",
                      "No Signal") %in% sq$squeeze_tier))
    # squeeze_candidate at R/04_master_score.R reads this cut.
    expect_true(any(sq$squeeze_score >= 60))
  })

  test_that("the squeeze bonus is live", {
    skip_if_not(exists("build_squeeze_score"), "R/03_data.R not sourced")
    skip_if(is.null(sq), "build_squeeze_score returned NULL")
    # It tests fundamentals_improving, so stubbing the flags killed it too.
    # Was squeeze_mult, a 1.20 multiplier, until it was found clipping nine
    # names to a tie at 100 -- see test-squeeze-ceiling.R.
    expect_true(any(sq$squeeze_bonus > 0))
    boosted <- sq[sq$squeeze_bonus > 0, ]
    expect_true(all(boosted$short_percent_float > 0.10))
    expect_true(all(boosted$fundamentals_improving))
    # And it has to actually move the score, or it is decoration.
    expect_true(all(boosted$squeeze_score > boosted$squeeze_score_raw))
  })

  test_that("absent fundamentals stop the build instead of capping the score", {
    skip_if_not(exists("build_squeeze_score"), "R/03_data.R not sourced")
    # Silently scoring every symbol 0 on the heaviest term is the failure this
    # whole file exists to prevent, so the contract violation must be loud.
    for (col in c("revenue_growth", "earningsGrowth", "profit_margin")) {
      msg <- tryCatch({
        suppressMessages(build_squeeze_score(
          short_fix, fund_fix[setdiff(names(fund_fix), col)]))
        NA_character_
      }, error = function(e) conditionMessage(e))
      expect_false(is.na(msg),
                   info = paste("dropping", col, "did not raise an error"))
      if (is.na(msg)) next
      expect_match(msg, col, fixed = TRUE)
      # It has to be the deliberate guard, not an incidental object-not-found
      # from dplyr: that error also happens to contain the column name, so
      # matching the name alone passes whether the guard is there or not.
      expect_match(msg, "fundamentals_scored.csv", fixed = TRUE)
      expect_match(msg, "tier threshold", fixed = TRUE)
    }
  })

  test_that("the squeeze weights still sum to one", {
    txt <- decomment(readLines(repo_path("R", "03_data.R"), warn = FALSE))
    raw <- grep("squeeze_score_raw *=", txt, value = TRUE)
    expect_length(raw, 1)
    w <- as.numeric(regmatches(raw, gregexpr("0\\.[0-9]+", raw))[[1]])
    expect_equal(sum(w), 1)
  })
}

# ── 2. Confidence weight ────────────────────────────────────────────────────

if (have_pkgs("dplyr", "tibble", "readr", "glue")) {
  suppressMessages({ library(dplyr); library(tibble) })
  SOURCED_BY_MASTER <- TRUE
  suppressMessages(try(source(repo_path("R", "02_momentum.R"), local = TRUE),
                       silent = TRUE))

  mut <- if (exists("run_module2")) find_arg(body(run_module2), "daily_resid") else NULL
  smz <- if (exists("run_module2")) find_arg(body(run_module2), "confidence_weight") else NULL

  # Evaluate a shipped expression against a supplied frame.
  shipped <- function(expr, env) eval(expr, env, baseenv())

  test_that("a symbol with no usable beta scores no residual at all", {
    skip_if(is.null(mut), "daily_resid not found in run_module2")
    # compute_beta needs 20 paired observations. Below that the old code sent
    # every date to the raw-excess fallback and ranked the symbol on it.
    env <- list2env(list(beta_window = rep(NA_real_, 5),
                         daily_ret   = c(0.01, 0.02, NA, 0.01, 0.00),
                         spy_ret     = c(0.01, 0.01, NA, 0.01, 0.00),
                         daily_alpha = c(0.00, 0.01, 0.02, 0.00, 0.00)))
    expect_true(all(is.na(shipped(mut$daily_resid, env))))
    expect_true(all(!shipped(mut$resid_scored, env)))
  })

  test_that("a few pre-pull dates still fall back to raw excess", {
    skip_if(is.null(mut), "daily_resid not found in run_module2")
    # alpha_history deliberately keeps dates that predate the current pull.
    # Those must keep feeding the alpha metrics — but must not buy confidence.
    env <- list2env(list(beta_window = rep(1.3, 4),
                         daily_ret   = c(0.02, NA, 0.01, 0.03),
                         spy_ret     = c(0.01, NA, 0.01, 0.02),
                         daily_alpha = c(0.01, 0.07, 0.00, 0.01)))
    resid  <- shipped(mut$daily_resid,  env)
    scored <- shipped(mut$resid_scored, env)
    expect_false(any(is.na(resid)))
    expect_equal(resid[2], 0.07)                      # raw excess, preserved
    expect_equal(resid[1], 0.02 - 1.3 * 0.01)         # residual elsewhere
    expect_equal(scored, c(TRUE, FALSE, TRUE, TRUE))  # the fallback is not fresh
  })

  test_that("confidence is keyed to scored days, not stored rows", {
    skip_if(is.null(smz), "confidence_weight not found in run_module2")
    expect_true("days_scored" %in% names(smz))
    conf <- function(days_scored)
      shipped(smz$confidence_weight, list2env(list(days_scored = days_scored)))
    expect_equal(conf(0),   0)
    expect_equal(conf(5),   5 / 63)
    expect_equal(conf(63),  1)
    expect_equal(conf(750), 1)
    # The regression: QRVO had days_tracked 711 and 5 scored days. Reading the
    # stored count gave it 1.0; the fix must not.
    expect_lt(conf(5), 0.1)
    # And the expression must not reach for the stored count at all.
    expect_false(grepl("days_tracked",
                       paste(deparse(smz$confidence_weight), collapse = " ")))
    expect_false(grepl("days_tracked",
                       paste(deparse(smz$days_scored), collapse = " ")))
    # And it must be counted from the per-date freshness flag. Defining it as
    # n() contains no banned word yet makes it identical to days_tracked.
    expect_match(paste(deparse(smz$days_scored), collapse = " "),
                 "resid_scored", fixed = TRUE)
  })

  test_that("scored days require both a usable beta and fresh returns", {
    skip_if(is.null(mut), "resid_scored not found in run_module2")
    expect_true("resid_scored" %in% names(mut))
    txt <- paste(deparse(mut$resid_scored), collapse = " ")
    for (needed in c("beta_window", "daily_ret", "spy_ret"))
      expect_match(txt, needed, fixed = TRUE)
    # daily_alpha is the stale fallback; counting it would restore the bug.
    expect_false(grepl("daily_alpha", txt, fixed = TRUE))
  })

  test_that("the stored-row count no longer drives any published label", {
    txt <- decomment(readLines(repo_path("R", "04_master_score.R"), warn = FALSE))
    band <- grep('"Proven"|"Established"|"Developing"', txt, value = TRUE)
    expect_gt(length(band), 0)
    expect_true(all(grepl("days_scored", band)))
    expect_false(any(grepl("days_tracked", band)))
    driver <- grep('"Alpha Leader"|"High IR"', txt, value = TRUE)
    expect_gt(length(driver), 0)
    expect_true(all(grepl("days_scored", driver)))
    expect_false(any(grepl("days_tracked", driver)))
    # Both published tables must carry the honest count beside the stored one.
    expect_gte(length(grep("^ *days_scored,", txt)), 2)
  })

  test_that("the stored-row count is reported but never gated on", {
    # days_tracked is still written out, and should be: a long history beside a
    # low scored count is exactly the diagnosis a reader wants. What it must
    # never do again is decide anything. Any threshold comparison against it is
    # the bug, wherever it appears.
    for (f in c(file.path("R", "02_momentum.R"),
                file.path("R", "04_master_score.R"))) {
      txt <- decomment(readLines(repo_path(f), warn = FALSE))
      # Comparison operators only: a plain = is assignment, and the word
      # boundary keeps max_days_tracked (a reported statistic) out of it.
      gated <- grep("\\bdays_tracked *(>=|<=|==|<|>)", txt, value = TRUE)
      expect_equal(gated, character(0),
                   info = paste(f, "gates on stored rows:",
                                paste(trimws(gated), collapse = " | ")))
    }
  })

  test_that("the nightly no longer claims thin names are shrunk to neutral", {
    # This prose moved into tools/verify_pipeline.R on 2026-11-03 when the
    # verify program came out of Rscript -e; read both so the assertion holds
    # wherever it lives.
    wf <- c(readLines(repo_path(".github", "workflows", "daily-update.yml"),
                      warn = FALSE),
            readLines(repo_path("tools", "verify_pipeline.R"), warn = FALSE))
    # The old rationale was false for exactly the symbol class it described: a
    # thin feed left days_tracked high, so the weight did not shrink at all.
    expect_false(any(grepl("weight that shrinks them toward neutral", wf,
                           fixed = TRUE)))
    cov <- grep("Warn, do not fail", wf)
    expect_length(cov, 1)
    expect_match(paste(wf[cov:(cov + 7)], collapse = " "), "compute_beta",
                 fixed = TRUE)
  })
}
