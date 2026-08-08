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

FORMATION_DAYS <- 63    # one quarter — the model's own significance threshold
HOLD_DAYS      <- 21    # ~one month forward
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

if (!exists("SOURCED_BY_MASTER")) {
  backtest_results   <- run_backtest()
  component_results  <- run_component_diagnostic()
}
