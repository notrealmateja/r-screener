# =============================================================================
# SHARED HELPERS
# =============================================================================
# Sourced defensively by the modules that need it:
#   if (!exists("corporate_action")) source("R/00_utils.R")
# It defines functions only, so a double source is harmless.

# Detect a corporate action masquerading as a return.
#
# Yahoo's `close` is raw. Its `adjusted` column does NOT help here — verified
# against PRPL, where adjusted is byte-identical to close and both carry the
# same +2010% bar on 2026-07-20. So the artifact has to be identified from its
# own shape.
#
# A magnitude threshold alone cannot do it. At |return| >= 50% the candidates
# are overwhelmingly genuine small-cap catalysts — FSLY +72%, RGTI +58%,
# TDUP -63% — and deleting those is fat-tail truncation, not cleaning.
#
# The signature is the tell. A k:1 reverse split multiplies price by k and
# divides share volume by k, so price and volume move in OPPOSITE directions.
# No news event does that: every real mover in this dataset arrived on volume
# up 2.8x to 139x. A forward split is the mirror image, where the two logs
# cancel. Checked against all 21 candidate bars in three years of history,
# this flags exactly PRPL, BRCC and IESC and leaves all 18 real moves alone.
CA_MOVE_FLOOR    <- 0.50   # only examine bars that moved at least this much
CA_FWD_TOLERANCE <- 0.30   # |log(price ratio) + log(volume ratio)| for a forward split

corporate_action <- function(daily_ret, vol_ratio) {
  big <- !is.na(daily_ret) & abs(daily_ret) >= CA_MOVE_FLOOR &
         !is.na(vol_ratio) & vol_ratio > 0
  sig     <- log(1 + daily_ret) + log(vol_ratio)
  reverse <- big & daily_ret > 0 & vol_ratio < 1          # price up, volume down
  forward <- big & daily_ret < 0 & abs(sig) < CA_FWD_TOLERANCE
  out <- reverse | forward
  ifelse(is.na(out), FALSE, out)
}

# Merge freshly computed history with what is already on disk.
#
# Fresh wins wherever the current pull reaches; disk only backfills dates the
# pull no longer covers. The previous logic kept every stored row except the
# current day and relied on distinct(), which keeps the FIRST match — so a bad
# bar could never be corrected once written. Yahoo restates recent history
# routinely; it has since withdrawn the BRCC bar that this pipeline recorded as
# +872%, yet the phantom value persisted and kept feeding the score.
merge_history <- function(existing, fresh, keep_days = NULL, today = Sys.Date()) {
  if (is.null(fresh) || nrow(fresh) == 0) fresh <- fresh[0, , drop = FALSE]
  # An undated row cannot be merged against anything, and leaving one in makes
  # min() below return Inf, which keeps every stored row and quietly restores
  # the stale-value bug this function exists to prevent.
  if (nrow(fresh) > 0) fresh <- fresh[!is.na(fresh$date), , drop = FALSE]
  if (is.null(existing) || nrow(existing) == 0) {
    out <- fresh
  } else if (nrow(fresh) == 0) {
    out <- existing
  } else {
    fresh_from <- min(fresh$date)
    out <- dplyr::bind_rows(existing[existing$date < fresh_from, , drop = FALSE], fresh)
  }
  out <- dplyr::distinct(out, symbol, date, .keep_all = TRUE)
  if (!is.null(keep_days)) out <- out[out$date >= today - keep_days, , drop = FALSE]
  dplyr::arrange(out, symbol, date)
}
