# =============================================================================
# MODULE 5 — WALK-FORWARD BACKTEST
#
# Question this answers: does the alpha score actually predict FORWARD excess
# return, or is it just describing the past?
#
# Method (out-of-sample by construction):
#   1. Step through rebalance dates every REBAL_DAYS trading days.
#   2. At each date t, score every symbol using ONLY daily_alpha up to t,
#      reproducing the live alpha_score_raw blend from 04_master_score.R.
#   3. Sort into quintiles, Q1 = highest score.
#   4. Measure realised alpha over the NEXT HOLD_DAYS days — data the score
#      could not have seen.
#   5. Aggregate across all rebalances.
#
# Headline metrics:
#   spread     — annualised Q1 alpha minus Q5 alpha.  If the score has no
#                predictive power this is ~0.
#   rank_ic    — Spearman correlation between score and forward alpha, averaged
#                over rebalances.  The cleanest single measure of skill.
#
# Nothing here is fit to the data: the weights come from the live model.
# =============================================================================
library(dplyr); library(readr); library(tidyr); library(glue)

# Must match ALPHA_WINDOW in 02_momentum.R, or this validates a configuration
# the live model does not use. It previously measured 63 days while the score
# averaged the whole history, so the two were never testing the same thing.
FORMATION_DAYS <- 126   # ~6 months, matching the live alpha window
# Three months forward. The classic momentum configuration is a 3-12 month
# formation held 3-12 months; 126/63 sits inside that. Measured at 21 days the
# same score gives IC +0.008, so the horizon is doing the work, not the factors.
HOLD_DAYS      <- 63
REBAL_DAYS     <- 21    # rebalance monthly
N_BUCKETS      <- 5     # quintiles: ~39 names each at a 195-stock universe
MIN_OBS        <- 40    # skip a symbol with too little formation history

# Reproduce the live alpha_score_raw blend (04_master_score.R) on a formation
# window.  Percentile ranks are computed cross-sectionally, as in production.
score_formation <- function(fm) {
  per_sym <- fm %>%
    group_by(symbol) %>%
    arrange(date) %>%
    summarize(
      n_obs      = sum(!is.na(daily_alpha)),
      ann_alpha  = mean(daily_alpha, na.rm = TRUE) * 252,
      ir         = {
        s <- sd(daily_alpha, na.rm = TRUE)
        if (is.na(s) || s == 0) NA_real_
        else (mean(daily_alpha, na.rm = TRUE) / s) * sqrt(252)
      },
      hit_rate   = mean(daily_alpha > 0, na.rm = TRUE),
      alpha_63d  = {
        d <- tail(daily_alpha[!is.na(daily_alpha)], 63)
        if (length(d) == 0) NA_real_ else prod(1 + d) - 1
      },
      streak     = {
        s <- rev(daily_alpha[!is.na(daily_alpha)])
        k <- 0L
        for (v in s) { if (!is.na(v) && v > 0) k <- k + 1L else break }
        k
      },
      .groups = "drop"
    ) %>%
    filter(n_obs >= MIN_OBS)

  if (nrow(per_sym) < N_BUCKETS * 2) return(NULL)

  pr <- function(x) {
    if (all(is.na(x))) return(rep(0.5, length(x)))
    dplyr::percent_rank(x) * 100
  }

  per_sym %>%
    mutate(
      p_ann_alpha = pr(ann_alpha),
      p_ir        = pr(ir),
      p_hit_rate  = pr(hit_rate),
      p_alpha_63d = pr(alpha_63d),
      p_streak    = pr(streak),
      score = p_ann_alpha * 0.30 + p_ir * 0.25 + p_hit_rate * 0.20 +
              p_alpha_63d * 0.15 + p_streak * 0.10
    ) %>%
    filter(!is.na(score))
}

run_backtest <- function(history_path = "data/alpha_history.csv") {
  message("\n=== MODULE 5: WALK-FORWARD BACKTEST ===\n")

  if (!file.exists(history_path)) {
    message("No alpha history at ", history_path, " — skipping backtest.")
    return(NULL)
  }
  hist <- read_csv(history_path, show_col_types = FALSE) %>%
    mutate(date = as.Date(date)) %>%
    filter(!is.na(daily_alpha)) %>%
    select(date, symbol, daily_alpha)

  dates <- sort(unique(hist$date))
  message(glue("History: {nrow(hist)} rows, {n_distinct(hist$symbol)} symbols, ",
               "{length(dates)} trading days ({min(dates)} to {max(dates)})"))

  # Rebalance points need a full formation window behind and a hold window ahead
  starts <- seq(FORMATION_DAYS + 1, length(dates) - HOLD_DAYS, by = REBAL_DAYS)
  if (length(starts) == 0) {
    message("Not enough history for a walk-forward test — need at least ",
            FORMATION_DAYS + HOLD_DAYS + 1, " trading days.")
    return(NULL)
  }
  message(glue("Formation {FORMATION_DAYS}d, hold {HOLD_DAYS}d, ",
               "{length(starts)} rebalances, {N_BUCKETS} buckets\n"))

  per_rebal <- list()
  ic_rows   <- list()

  for (i in seq_along(starts)) {
    ix     <- starts[i]
    t_date <- dates[ix]
    fm_dates <- dates[(ix - FORMATION_DAYS):(ix - 1)]
    fw_dates <- dates[ix:min(ix + HOLD_DAYS - 1, length(dates))]

    scored <- score_formation(hist %>% filter(date %in% fm_dates))
    if (is.null(scored)) next

    forward <- hist %>%
      filter(date %in% fw_dates) %>%
      group_by(symbol) %>%
      summarize(
        fwd_mean_alpha = mean(daily_alpha, na.rm = TRUE),
        fwd_cum_alpha  = prod(1 + daily_alpha) - 1,
        fwd_days       = n(),
        .groups = "drop"
      )

    joined <- inner_join(scored %>% select(symbol, score), forward, by = "symbol") %>%
      filter(!is.na(fwd_mean_alpha))
    if (nrow(joined) < N_BUCKETS * 2) next

    # Rank information coefficient for this rebalance
    ic <- suppressWarnings(cor(joined$score, joined$fwd_mean_alpha,
                               method = "spearman", use = "complete.obs"))
    ic_rows[[length(ic_rows) + 1]] <- tibble(rebalance_date = t_date, rank_ic = ic,
                                             n_stocks = nrow(joined))

    joined <- joined %>%
      mutate(bucket = ntile(desc(score), N_BUCKETS))   # bucket 1 = highest score

    per_rebal[[length(per_rebal) + 1]] <- joined %>%
      group_by(bucket) %>%
      summarize(
        rebalance_date = t_date,
        mean_daily_alpha = mean(fwd_mean_alpha, na.rm = TRUE),
        cum_alpha        = mean(fwd_cum_alpha,  na.rm = TRUE),
        hit_rate         = mean(fwd_mean_alpha > 0, na.rm = TRUE),
        n_stocks         = n(),
        .groups = "drop"
      )
  }

  if (length(per_rebal) == 0) {
    message("No usable rebalances — skipping backtest.")
    return(NULL)
  }

  detail <- bind_rows(per_rebal)
  ics    <- bind_rows(ic_rows)

  summary_tbl <- detail %>%
    group_by(bucket) %>%
    summarize(
      periods          = n(),
      ann_alpha        = mean(mean_daily_alpha, na.rm = TRUE) * 252,
      alpha_ir         = {
        s <- sd(mean_daily_alpha, na.rm = TRUE)
        if (is.na(s) || s == 0) NA_real_
        else (mean(mean_daily_alpha, na.rm = TRUE) / s) * sqrt(252 / HOLD_DAYS)
      },
      hit_rate         = mean(hit_rate, na.rm = TRUE),
      avg_cum_alpha    = mean(cum_alpha, na.rm = TRUE),
      worst_period     = min(cum_alpha, na.rm = TRUE),
      avg_n            = round(mean(n_stocks)),
      .groups = "drop"
    ) %>%
    mutate(bucket_label = paste0("Q", bucket,
                                 ifelse(bucket == 1, " (highest score)",
                                 ifelse(bucket == N_BUCKETS, " (lowest score)", "")))) %>%
    arrange(bucket)

  top    <- summary_tbl$ann_alpha[summary_tbl$bucket == 1]
  bottom <- summary_tbl$ann_alpha[summary_tbl$bucket == N_BUCKETS]
  spread <- top - bottom
  mean_ic <- mean(ics$rank_ic, na.rm = TRUE)
  # t-stat of the IC series: is the mean IC distinguishable from zero?
  ic_t <- if (nrow(ics) > 1 && sd(ics$rank_ic, na.rm = TRUE) > 0)
    mean_ic / (sd(ics$rank_ic, na.rm = TRUE) / sqrt(nrow(ics))) else NA_real_

  headline <- tibble(
    formation_days = FORMATION_DAYS,
    hold_days      = HOLD_DAYS,
    rebalances     = nrow(ics),
    n_buckets      = N_BUCKETS,
    q1_ann_alpha   = top,
    q5_ann_alpha   = bottom,
    spread_ann     = spread,
    mean_rank_ic   = mean_ic,
    ic_t_stat      = ic_t,
    monotonic      = all(diff(summary_tbl$ann_alpha) <= 0)
  )

  if (!dir.exists("data")) dir.create("data", recursive = TRUE)
  write_csv(summary_tbl, "data/backtest_summary.csv")
  write_csv(detail,      "data/backtest_detail.csv")
  write_csv(ics,         "data/backtest_ic.csv")
  write_csv(headline,    "data/backtest_headline.csv")

  message("\n--- RESULTS (out-of-sample) ---")
  for (i in seq_len(nrow(summary_tbl))) {
    r <- summary_tbl[i, ]
    message(glue("  {r$bucket_label}: ann alpha {sprintf('%+.2f%%', r$ann_alpha*100)}  ",
                 "IR {sprintf('%.2f', r$alpha_ir)}  hit {sprintf('%.0f%%', r$hit_rate*100)}  ",
                 "n={r$avg_n}"))
  }
  message(glue("\n  Q1 - Q5 spread: {sprintf('%+.2f%%', spread*100)} annualised"))
  message(glue("  Mean rank IC:   {sprintf('%+.4f', mean_ic)} (t = {sprintf('%.2f', ic_t)})"))
  message(glue("  Monotonic across buckets: {headline$monotonic}"))
  if (is.na(ic_t) || abs(ic_t) < 2) {
    message("  -> IC is not statistically distinguishable from zero at this sample size.")
  } else if (mean_ic > 0) {
    message("  -> Score shows positive predictive power.")
  } else {
    message("  -> Score is inversely related to forward alpha.")
  }

  list(summary = summary_tbl, detail = detail, ic = ics, headline = headline)
}

# =============================================================================
# COMPONENT DIAGNOSTIC
#
# run_backtest() measures the 5-way blend and finds an IC near zero.  That
# leaves one question open: is the whole premise dead, or does one component
# carry information the other four dilute away?
#
# This runs the identical walk-forward loop but correlates each RAW component
# with forward alpha separately, instead of the blend.  Reuses
# score_formation() so the components can never drift from the live model.
#
# Reading it: mean_ic is the average Spearman correlation between the component
# and forward excess return.  t_stat tests whether that mean differs from zero
# across rebalances — |t| > 2 is the conventional bar.  pct_positive is how
# often the correlation had the expected sign, where 50% is a coin flip.
# =============================================================================
COMPONENTS <- c(
  ann_alpha = "Annualised alpha",
  ir        = "Information ratio",
  hit_rate  = "Hit rate vs SPY",
  alpha_63d = "Alpha, trailing 63d",
  streak    = "Positive-alpha streak",
  score     = "Blended score (baseline)"
)

run_component_diagnostic <- function(history_path = "data/alpha_history.csv") {
  message("\n=== COMPONENT DIAGNOSTIC ===\n")

  if (!file.exists(history_path)) {
    message("No alpha history — skipping component diagnostic.")
    return(NULL)
  }
  hist <- read_csv(history_path, show_col_types = FALSE) %>%
    mutate(date = as.Date(date)) %>%
    filter(!is.na(daily_alpha)) %>%
    select(date, symbol, daily_alpha)

  dates  <- sort(unique(hist$date))
  starts <- seq(FORMATION_DAYS + 1, length(dates) - HOLD_DAYS, by = REBAL_DAYS)
  if (length(starts) == 0) {
    message("Not enough history for the component diagnostic.")
    return(NULL)
  }

  rows <- list()
  for (ix in starts) {
    fm_dates <- dates[(ix - FORMATION_DAYS):(ix - 1)]
    fw_dates <- dates[ix:min(ix + HOLD_DAYS - 1, length(dates))]

    scored <- score_formation(hist %>% filter(date %in% fm_dates))
    if (is.null(scored)) next

    forward <- hist %>%
      filter(date %in% fw_dates) %>%
      group_by(symbol) %>%
      summarize(fwd = mean(daily_alpha, na.rm = TRUE), .groups = "drop")

    j <- inner_join(scored, forward, by = "symbol") %>% filter(!is.na(fwd))
    if (nrow(j) < 20) next

    for (cmp in names(COMPONENTS)) {
      if (!cmp %in% names(j)) next
      v <- j[[cmp]]
      if (sum(!is.na(v)) < 20 || length(unique(v[!is.na(v)])) < 3) next
      ic <- suppressWarnings(cor(v, j$fwd, method = "spearman", use = "complete.obs"))
      if (!is.na(ic)) rows[[length(rows) + 1]] <-
        tibble(rebalance_date = dates[ix], component = cmp, ic = ic)
    }
  }

  if (length(rows) == 0) {
    message("No usable rebalances for the component diagnostic.")
    return(NULL)
  }

  per_rebal <- bind_rows(rows)
  out <- per_rebal %>%
    group_by(component) %>%
    summarize(
      rebalances   = n(),
      mean_ic      = mean(ic, na.rm = TRUE),
      ic_sd        = sd(ic, na.rm = TRUE),
      t_stat       = {
        s <- sd(ic, na.rm = TRUE)
        if (is.na(s) || s == 0) NA_real_ else mean(ic, na.rm = TRUE) / (s / sqrt(n()))
      },
      pct_positive = mean(ic > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      label       = unname(COMPONENTS[component]),
      significant = !is.na(t_stat) & abs(t_stat) > 2
    ) %>%
    arrange(desc(abs(mean_ic)))

  if (!dir.exists("data")) dir.create("data", recursive = TRUE)
  write_csv(out, "data/backtest_components.csv")

  message(glue("Component ICs over {max(out$rebalances)} rebalances ",
               "(formation {FORMATION_DAYS}d, hold {HOLD_DAYS}d):\n"))
  for (i in seq_len(nrow(out))) {
    r <- out[i, ]
    message(glue("  {sprintf('%-26s', r$label)} IC {sprintf('%+.4f', r$mean_ic)}  ",
                 "t {sprintf('%6.2f', r$t_stat)}  ",
                 "pos {sprintf('%3.0f%%', r$pct_positive*100)}  ",
                 "{ifelse(r$significant, '<-- SIGNIFICANT', '')}"))
  }
  if (any(out$significant)) {
    hits <- out %>% filter(significant)
    message(glue("\n  {nrow(hits)} component(s) clear |t| > 2 — the blend may be ",
                 "diluting real signal."))
  } else {
    message("\n  No component clears |t| > 2. The signal is absent at the ",
            "component level too, not merely diluted by the blend.")
  }

  out
}

# =============================================================================
# REVERSAL TEST
#
# The component diagnostic turned up something the model did not predict: of
# the six measures, `streak` had the largest magnitude, it was NEGATIVE
# (IC -0.024, correct sign only 44% of the time), and it points against the
# momentum premise the whole score is built on.  Short-term reversal is a
# documented effect, so the honest next step is to test it rather than ignore
# a result that disagrees with the design.
#
# Method: the same walk-forward harness, but correlating TRAILING alpha with
# FORWARD alpha directly.  The sign of that correlation is the whole answer —
#   negative => losers outperform      (reversal)
#   positive => winners outperform     (momentum)
# Several formation windows are tested because reversal and momentum operate
# at different horizons; short windows are where reversal usually shows up.
#
# Rebalance dates are held constant across windows so the formation length is
# the only thing that varies.
#
# Multiple testing is accounted for: testing 4 windows means 4 chances at a
# false positive, so significance is judged against a Bonferroni-adjusted
# |t| > 2.5 rather than the usual 2.0.
# =============================================================================
REVERSAL_FORMATIONS <- c(5, 10, 21, 63)   # trading days
REVERSAL_T_BAR      <- 2.5                # Bonferroni-adjusted for 4 windows

run_reversal_test <- function(history_path = "data/alpha_history.csv") {
  message("\n=== REVERSAL TEST ===\n")

  if (!file.exists(history_path)) {
    message("No alpha history — skipping reversal test.")
    return(NULL)
  }
  hist <- read_csv(history_path, show_col_types = FALSE) %>%
    mutate(date = as.Date(date)) %>%
    filter(!is.na(daily_alpha)) %>%
    select(date, symbol, daily_alpha)

  dates  <- sort(unique(hist$date))
  longest <- max(REVERSAL_FORMATIONS)
  starts <- seq(longest + 1, length(dates) - HOLD_DAYS, by = REBAL_DAYS)
  if (length(starts) == 0) {
    message("Not enough history for the reversal test.")
    return(NULL)
  }

  rows <- list()
  for (fdays in REVERSAL_FORMATIONS) {
    for (ix in starts) {
      fm_dates <- dates[(ix - fdays):(ix - 1)]
      fw_dates <- dates[ix:min(ix + HOLD_DAYS - 1, length(dates))]

      trailing <- hist %>%
        filter(date %in% fm_dates) %>%
        group_by(symbol) %>%
        summarize(n_obs = n(),
                  trail = prod(1 + daily_alpha) - 1, .groups = "drop") %>%
        filter(n_obs >= max(3, floor(fdays * 0.6)))

      forward <- hist %>%
        filter(date %in% fw_dates) %>%
        group_by(symbol) %>%
        summarize(fwd = mean(daily_alpha, na.rm = TRUE), .groups = "drop")

      j <- inner_join(trailing, forward, by = "symbol") %>%
        filter(!is.na(trail), !is.na(fwd))
      if (nrow(j) < N_BUCKETS * 4) next

      ic <- suppressWarnings(cor(j$trail, j$fwd, method = "spearman",
                                 use = "complete.obs"))
      # Long the worst trailing performers, short the best.  Positive spread
      # means the losers went on to beat the winners — reversal paying off.
      j <- j %>% mutate(bucket = ntile(trail, N_BUCKETS))   # 1 = worst trailing
      losers  <- mean(j$fwd[j$bucket == 1],         na.rm = TRUE)
      winners <- mean(j$fwd[j$bucket == N_BUCKETS], na.rm = TRUE)

      if (!is.na(ic)) rows[[length(rows) + 1]] <- tibble(
        formation_days = fdays, rebalance_date = dates[ix],
        ic = ic, ls_spread = losers - winners)
    }
  }

  if (length(rows) == 0) {
    message("No usable rebalances for the reversal test.")
    return(NULL)
  }

  per_rebal <- bind_rows(rows)
  tstat <- function(x) {
    s <- sd(x, na.rm = TRUE)
    if (is.na(s) || s == 0) NA_real_ else mean(x, na.rm = TRUE) / (s / sqrt(sum(!is.na(x))))
  }

  out <- per_rebal %>%
    group_by(formation_days) %>%
    summarize(
      rebalances     = n(),
      mean_ic        = mean(ic, na.rm = TRUE),
      ic_t           = tstat(ic),
      pct_negative   = mean(ic < 0, na.rm = TRUE),     # how often reversal held
      ls_spread_ann  = mean(ls_spread, na.rm = TRUE) * 252,
      ls_t           = tstat(ls_spread),
      .groups = "drop"
    ) %>%
    mutate(
      effect      = dplyr::case_when(
        is.na(ic_t)                      ~ "undetermined",
        abs(ic_t) <= REVERSAL_T_BAR      ~ "none",
        mean_ic < 0                      ~ "reversal",
        TRUE                             ~ "momentum"),
      significant = !is.na(ic_t) & abs(ic_t) > REVERSAL_T_BAR
    ) %>%
    arrange(formation_days)

  if (!dir.exists("data")) dir.create("data", recursive = TRUE)
  write_csv(out, "data/reversal_results.csv")

  message(glue("Trailing vs forward alpha, {max(out$rebalances)} rebalances, ",
               "hold {HOLD_DAYS}d. Negative IC = reversal.\n"))
  for (i in seq_len(nrow(out))) {
    r <- out[i, ]
    message(glue("  formation {sprintf('%2d', r$formation_days)}d: ",
                 "IC {sprintf('%+.4f', r$mean_ic)} (t {sprintf('%5.2f', r$ic_t)})  ",
                 "L/S {sprintf('%+6.2f%%', r$ls_spread_ann*100)} ",
                 "(t {sprintf('%5.2f', r$ls_t)})  ",
                 "{r$effect}{ifelse(r$significant, '  <-- SIGNIFICANT', '')}"))
  }
  message(glue("\n  Significance bar |t| > {REVERSAL_T_BAR} ",
               "(Bonferroni-adjusted for {length(REVERSAL_FORMATIONS)} windows)."))
  if (any(out$significant)) {
    hit <- out %>% filter(significant) %>% slice(1)
    message(glue("  -> {hit$effect} detected at the {hit$formation_days}-day ",
                 "formation window."))
  } else {
    message("  -> No window shows a significant effect in either direction. ",
            "The negative streak IC does not survive as a tradeable signal.")
  }

  out
}


# =============================================================================
# 4. EQUITY CURVE VERSUS THE S&P 500
# =============================================================================
# The first three sections answer "does the score rank correctly?" in units of
# alpha. This one answers the question a person actually asks — "would holding
# this have beaten the index?" — in money, and it is built so it can show the
# model LOSING. Q5 (the names the model ranks worst) is carried through as a
# falsification control: if Q5 also beats the benchmark, the ranking is sorting
# on market exposure rather than skill, and the tab says so.
#
# Non-overlapping tranches: rebalance every HOLD_DAYS rather than every
# REBAL_DAYS. The IC sections above deliberately overlap windows to gain
# observations, but overlapping tranches cannot be drawn as one tradeable
# equity curve without double-counting capital.

EQ_COST_BPS <- 10   # round-trip cost per unit turnover, in basis points

# Corporate actions are identified and neutralised at ingest by 02_momentum.R,
# but this module reads price_history.csv directly and may be run against a
# file written before that guard existed, so it applies the same test itself.
# Shared so the two can never drift apart.
if (!exists("corporate_action")) {
  # Modules run from the repo root in production but from tests/testthat under
  # testthat, so resolve rather than assuming the working directory.
  .utils <- Filter(file.exists,
                   c("R/00_utils.R", "../R/00_utils.R", "../../R/00_utils.R"))
  if (length(.utils)) source(.utils[1]) else
    stop("00_utils.R not found from ", getwd())
}

equity_stats <- function(r, bench, dates = NULL, periods_per_year = 252) {
  ok <- !is.na(r) & !is.na(bench)
  r <- r[ok]; bench <- bench[ok]
  if (length(r) < 2) return(NULL)
  cum <- cumprod(1 + r)
  # Years from the calendar span where we have dates. Deriving them as
  # length(r)/252 silently inflates CAGR whenever a day is dropped from the
  # series, because the denominator shrinks while the money made does not.
  yrs <- if (!is.null(dates) && length(dates) == length(ok)) {
           d <- dates[ok]
           as.numeric(max(d) - min(d)) / 365.25
         } else length(r) / periods_per_year
  if (!is.finite(yrs) || yrs <= 0) yrs <- length(r) / periods_per_year
  cagr  <- cum[length(cum)]^(1 / yrs) - 1
  vol   <- sd(r) * sqrt(periods_per_year)
  bcum  <- cumprod(1 + bench)
  bcagr <- bcum[length(bcum)]^(1 / yrs) - 1
  beta  <- if (var(bench) > 0) cov(r, bench) / var(bench) else NA_real_
  ex    <- r - bench
  te    <- sd(ex) * sqrt(periods_per_year)
  list(
    total_return = cum[length(cum)] - 1,
    cagr         = cagr,
    vol          = vol,
    # Return over volatility with NO risk-free deduction. Named so, because
    # calling it "Sharpe" while assuming cash pays nothing overstates it.
    return_vol   = if (vol > 0) cagr / vol else NA_real_,
    max_drawdown = min(cum / cummax(cum) - 1),
    beta         = beta,
    tracking_err = te,
    info_ratio   = if (te > 0) (cagr - bcagr) / te else NA_real_,
    excess_cagr  = cagr - bcagr,
    # Daily excess t. Reported for reference only — it divides by sqrt(623)
    # when the strategy makes ~10 independent decisions, so it overstates
    # significance. excess_t_tranche below is the one to quote.
    excess_t     = if (sd(ex) > 0) mean(ex) / (sd(ex) / sqrt(length(ex))) else NA_real_,
    days         = length(r)
  )
}

# Buy and hold one equal-weighted basket for the life of a tranche, letting the
# weights drift. Averaging daily returns cross-sectionally instead would
# silently rebalance to equal weight EVERY day — a different, higher-turnover
# strategy than the one described, and one whose turnover goes uncharged.
tranche_returns <- function(fw, held) {
  fw <- fw %>% filter(symbol %in% held) %>% select(date, symbol, daily_ret)
  if (nrow(fw) == 0) return(NULL)
  wide <- fw %>%
    tidyr::pivot_wider(names_from = symbol, values_from = daily_ret,
                       values_fill = 0) %>%
    arrange(date)
  mat <- as.matrix(wide[, -1, drop = FALSE])
  if (ncol(mat) == 0) return(NULL)
  mat[is.na(mat)] <- 0
  growth <- apply(1 + mat, 2, cumprod)
  if (is.null(dim(growth))) growth <- matrix(growth, nrow = nrow(mat))
  port <- rowMeans(growth)
  tibble(date = wide$date,
         ret  = c(port[1] - 1, port[-1] / port[-length(port)] - 1),
         n    = ncol(mat))
}

run_equity_curve <- function(price_path = "data/price_history.csv") {
  if (!file.exists(price_path)) {
    message("No price history at ", price_path, " — skipping equity curve.")
    return(invisible(NULL))
  }
  px <- read_csv(price_path, show_col_types = FALSE)
  need <- c("symbol", "date", "daily_ret", "spy_ret", "daily_alpha", "volume")
  if (!all(need %in% names(px))) {
    message("price_history lacks ", paste(setdiff(need, names(px)), collapse = ", "),
            " — skipping equity curve.")
    return(invisible(NULL))
  }

  px <- px %>% select(all_of(need)) %>% arrange(symbol, date) %>%
    group_by(symbol) %>% mutate(vol_ratio = volume / lag(volume)) %>% ungroup()

  px$is_ca <- corporate_action(px$daily_ret, px$vol_ratio)
  n_ca <- sum(px$is_ca, na.rm = TRUE)
  if (n_ca > 0) {
    hits <- px[px$is_ca, ] %>% arrange(desc(abs(daily_ret)))
    message(glue("Neutralised {n_ca} corporate-action bars (price and volume ",
                 "moved opposite ways):"))
    for (i in seq_len(min(5, nrow(hits))))
      message(glue("    {hits$symbol[i]} {hits$date[i]}: ",
                   "{round(hits$daily_ret[i] * 100, 1)}% on ",
                   "{round(hits$vol_ratio[i], 2)}x volume"))
    # A split preserves position value, so the correct portfolio return is 0 —
    # not the peer average that deleting the row would implicitly assign.
    px$daily_ret[px$is_ca]   <- 0
    px$daily_alpha[px$is_ca] <- 0 - px$spy_ret[px$is_ca]
  }

  px <- px %>% filter(!is.na(daily_ret), !is.na(spy_ret)) %>% arrange(date, symbol)
  if (nrow(px) == 0) { message("No usable price rows."); return(invisible(NULL)) }

  bench <- px %>% distinct(date, spy_ret) %>% arrange(date)

  # Equal-weighting the WHOLE universe with no signal at all. This is the
  # control for survivorship: the 195 tickers are a list written in 2026 and
  # backtested from 2024, with zero delistings in three years, so simply being
  # eligible is worth something. Whatever this line earns above the index is
  # the candidate set, not the score. Without it the tab credits the model for
  # the bias in its own universe.
  universe <- px %>% group_by(date) %>%
    summarize(univ_ret = mean(daily_ret, na.rm = TRUE), .groups = "drop") %>%
    arrange(date)
  dates <- bench$date
  if (length(dates) < FORMATION_DAYS + HOLD_DAYS + 1) {
    message(glue("Need {FORMATION_DAYS + HOLD_DAYS + 1} trading days, have {length(dates)}."))
    return(invisible(NULL))
  }

  # Run to the END of the data. Stopping at the last FULL tranche discarded the
  # most recent 56 trading days, and which days get discarded depends only on
  # where the grid happens to start — a phase choice that moved the result.
  starts <- seq(FORMATION_DAYS + 1, length(dates) - 1, by = HOLD_DAYS)
  message(glue("Equity curve: formation {FORMATION_DAYS}d, hold {HOLD_DAYS}d, ",
               "{length(starts)} non-overlapping tranches, {EQ_COST_BPS}bp turnover cost."))

  legs <- list(); prev_q1 <- character(0); prev_q5 <- character(0)

  for (ix in starts) {
    fm <- px %>% filter(date %in% dates[(ix - FORMATION_DAYS):(ix - 1)]) %>%
      select(date, symbol, daily_alpha)
    scored <- score_formation(fm)
    if (is.null(scored)) next

    scored <- scored %>% mutate(bucket = ntile(desc(score), N_BUCKETS))
    q1 <- scored$symbol[scored$bucket == 1]
    q5 <- scored$symbol[scored$bucket == N_BUCKETS]
    if (length(q1) == 0 || length(q5) == 0) next

    fw_dates <- dates[ix:min(ix + HOLD_DAYS - 1, length(dates))]
    fw <- px %>% filter(date %in% fw_dates)

    leg1 <- tranche_returns(fw, q1)
    leg5 <- tranche_returns(fw, q5)
    if (is.null(leg1) || is.null(leg5)) next
    leg <- leg1 %>% rename(model_ret = ret, n_held = n) %>%
      inner_join(leg5 %>% select(date, q5_ret = ret), by = "date")
    if (nrow(leg) == 0) next

    # Charge turnover once, on the first day. Holding a name across a rebalance
    # costs nothing; only the switched fraction does.
    turn1 <- if (length(prev_q1) == 0) 1 else 1 - length(intersect(prev_q1, q1)) / length(q1)
    turn5 <- if (length(prev_q5) == 0) 1 else 1 - length(intersect(prev_q5, q5)) / length(q5)
    leg$model_ret[1] <- leg$model_ret[1] - turn1 * EQ_COST_BPS / 10000
    leg$q5_ret[1]    <- leg$q5_ret[1]    - turn5 * EQ_COST_BPS / 10000

    prev_q1 <- q1; prev_q5 <- q5
    leg$tranche <- length(legs) + 1L
    legs[[length(legs) + 1]] <- leg
  }

  if (length(legs) == 0) { message("No tranches produced."); return(invisible(NULL)) }

  curve <- bind_rows(legs) %>% arrange(date) %>%
    inner_join(bench, by = "date") %>%
    inner_join(universe, by = "date") %>%
    mutate(
      model_cum = cumprod(1 + model_ret),
      q5_cum    = cumprod(1 + q5_ret),
      spy_cum   = cumprod(1 + spy_ret),
      univ_cum  = cumprod(1 + univ_ret),
      # Where the model actually loses. Both lines can rise together and hide a
      # long stretch of underperformance.
      rel_cum   = model_cum / spy_cum,
      model_dd  = model_cum / cummax(model_cum) - 1,
      spy_dd    = spy_cum   / cummax(spy_cum)   - 1
    )

  m  <- equity_stats(curve$model_ret, curve$spy_ret, curve$date)
  q5 <- equity_stats(curve$q5_ret,    curve$spy_ret, curve$date)
  b  <- equity_stats(curve$spy_ret,   curve$spy_ret, curve$date)
  u  <- equity_stats(curve$univ_ret,  curve$spy_ret, curve$date)

  # Significance on INDEPENDENT decisions. The daily excess t divides by
  # sqrt(623) when the strategy only rebalances ~10 times, and every day inside
  # a 63-day hold is the same decision still playing out — not fresh evidence.
  tranche_t <- function(col) {
    per <- curve %>% group_by(tranche) %>%
      summarize(ex = prod(1 + .data[[col]]) / prod(1 + spy_ret) - 1, .groups = "drop")
    if (nrow(per) < 2 || sd(per$ex) == 0) return(NA_real_)
    mean(per$ex) / (sd(per$ex) / sqrt(nrow(per)))
  }
  m$excess_t_tranche  <- tranche_t("model_ret")
  q5$excess_t_tranche <- tranche_t("q5_ret")
  b$excess_t_tranche  <- NA_real_
  u$excess_t_tranche  <- tranche_t("univ_ret")
  m$n_decisions  <- length(legs); q5$n_decisions <- length(legs)
  b$n_decisions  <- length(legs); u$n_decisions  <- length(legs)

  ca_in_window <- sum(px$is_ca & px$date >= min(curve$date) &
                        px$date <= max(curve$date), na.rm = TRUE)

  stats <- bind_rows(
    tibble(series = "model", label = "Model (top quintile)",    !!!m),
    tibble(series = "q5",    label = "Model (bottom quintile)", !!!q5),
    tibble(series = "univ",  label = "Universe, equal weight (no signal)", !!!u),
    tibble(series = "spy",   label = "S&P 500 (SPY)",           !!!b)
  ) %>%
    mutate(tranches = length(legs), cost_bps = EQ_COST_BPS,
           corporate_actions_neutralised = ca_in_window,
           start_date = min(curve$date), end_date = max(curve$date))

  write_csv(curve %>% select(date, tranche, model_ret, q5_ret, spy_ret, univ_ret,
                             model_cum, q5_cum, spy_cum, univ_cum, rel_cum,
                             model_dd, spy_dd, n_held),
            "data/backtest_equity.csv")
  write_csv(stats, "data/backtest_equity_stats.csv")

  message(glue("\nGrowth of $1, {min(curve$date)} to {max(curve$date)} ({m$days} days):"))
  message(glue("  Model Q1  {round(m$cagr * 100, 1)}%/yr  maxDD {round(m$max_drawdown * 100, 1)}%  ",
               "beta {round(m$beta, 2)}  excess t {round(m$excess_t, 2)}"))
  message(glue("  S&P 500   {round(b$cagr * 100, 1)}%/yr  maxDD {round(b$max_drawdown * 100, 1)}%"))
  message(glue("  Universe  {round(u$cagr * 100, 1)}%/yr  (equal weight, NO signal)"))
  message(glue("  Model Q5  {round(q5$cagr * 100, 1)}%/yr  (falsification control)"))
  message(glue("  Excess t over {length(legs)} independent rebalances: ",
               "{round(m$excess_t_tranche, 2)}"))
  if (!is.na(u$excess_cagr) && u$excess_cagr > 0)
    message(glue("  NOTE: holding the whole universe with no signal beat the index by ",
                 "{round(u$excess_cagr * 100, 1)}pp/yr. That much of the model's edge is ",
                 "the candidate list, not the score."))
  if (!is.na(q5$excess_cagr) && q5$excess_cagr > 0)
    message("  NOTE: the bottom quintile also beat the index — the ranking is ",
            "sorting substantially on market exposure, not skill.")

  list(curve = curve, stats = stats)
}

if (!exists("SOURCED_BY_MASTER")) {
  backtest_results   <- run_backtest()
  component_results  <- run_component_diagnostic()
  reversal_results   <- run_reversal_test()
  equity_results     <- run_equity_curve()
}
