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
  # An undated row cannot be merged against anything. On the fresh side it made
  # min() return Inf, so `existing$date < Inf` kept every stored row and quietly
  # restored the stale-value bug this function exists to prevent. On the stored
  # side it survived the comparison as NA, and `df[NA, ]` injects an entire row
  # of NAs — into a file the pipeline commits. Both sides are dropped up front.
  dated <- function(d) !is.null(d) && nrow(d) > 0 && "date" %in% names(d)
  if (dated(fresh))    fresh    <- fresh[!is.na(fresh$date), , drop = FALSE]
  if (dated(existing)) existing <- existing[!is.na(existing$date), , drop = FALSE]

  if (is.null(existing) || nrow(existing) == 0) {
    out <- fresh
  } else if (is.null(fresh) || nrow(fresh) == 0) {
    out <- existing
  } else {
    # How far back the pull reached is a PER-SYMBOL fact. Taking one min()
    # across the whole pull applied the longest series' start date to every
    # symbol, so a ticker that came back short — or did not come back at all —
    # had every stored row after that date deleted. On a 3-symbol fixture a
    # symbol returning 5 of 366 days lost the other 361, and a symbol absent
    # from the pull was erased outright. This is not hypothetical: CRNX and WBS
    # both pull short today.
    #
    # Comparing each symbol against its own reach keeps the anti-stale property
    # exactly where it belongs — a restated bar is still overwritten, because
    # the pull that restates it necessarily reaches it.
    # Bounded at BOTH ends, because "everything from here on" over-reaches.
    # A stored row after the pull's last date is a row the pull says nothing
    # about, exactly like one before its first. That matters for score_history,
    # where fresh is a single day: dropping every row at-or-after it deletes
    # later days whenever the as-of date moves backward, which it can now that
    # the date is taken from the data rather than the wall clock.
    # Inside the range the pull remains authoritative, so a bar Yahoo has since
    # withdrawn still disappears — the case this function was written for.
    sym  <- as.character(fresh$symbol)
    lo   <- tapply(as.numeric(fresh$date), sym, min)
    hi   <- tapply(as.numeric(fresh$date), sym, max)
    esym <- as.character(existing$symbol)
    ed   <- as.numeric(existing$date)
    keep <- ed < lo[esym] | ed > hi[esym]
    # NA means the pull said nothing about that symbol, so it cannot correct it.
    keep[is.na(keep)] <- TRUE
    out <- dplyr::bind_rows(existing[keep, , drop = FALSE], fresh)
  }
  out <- dplyr::distinct(out, symbol, date, .keep_all = TRUE)
  # which() rather than a bare logical: an NA date here would subset in another
  # row of NAs, which is the failure mode above wearing a different hat.
  if (!is.null(keep_days)) out <- out[which(out$date >= today - keep_days), , drop = FALSE]
  dplyr::arrange(out, symbol, date)
}

# Replace a long history file only if the incoming pull plausibly IS one.
#
# write_or_keep() in R/03_data.R guards the soft feeds against an EMPTY fetch.
# That is the wrong shape of guard for price history, whose dangerous failure
# is a SHORT fetch. When Yahoo throttles, tq_get returns a handful of recent
# sessions for every symbol rather than nothing — so the result is non-empty,
# every emptiness check waves it through, and the write lands. safe_calc() then
# turns ma200, rsi14, macd and the Bollinger bands into NA because TTR has too
# few periods, and three calendar years of history is gone in one write_csv.
#
# Nothing downstream can recover it. The next run pulls the same short window
# and writes it again, so the only place the loss can be prevented is here,
# against what is already on disk, before the write.
#
# Three ways the incoming frame can fail to be a history:
#   - far fewer rows than the file it would replace
#   - a median bar count per symbol that has collapsed (catches a truncation
#     that hits every symbol equally, which a row total can mask if the
#     universe grew at the same time)
#   - a newest date that moves BACKWARDS, which no healthy pull ever does
#
# Returns TRUE if it wrote, FALSE if it kept what was there.
write_history_or_keep <- function(df, path, min_frac = 0.80,
                                  label = basename(path)) {
  prev <- if (file.exists(path))
    tryCatch(readr::read_csv(path, show_col_types = FALSE,
                             progress = FALSE),
             error = function(e) NULL) else NULL

  # Nothing to protect: first run, or an unreadable file that is no loss.
  if (is.null(prev) || nrow(prev) == 0) {
    readr::write_csv(df, path)
    return(invisible(TRUE))
  }

  bars <- function(d) {
    if (!"symbol" %in% names(d) || nrow(d) == 0) return(0)
    stats::median(as.numeric(table(d$symbol)))
  }
  newest <- function(d) {
    if (!"date" %in% names(d) || nrow(d) == 0) return(NA)
    suppressWarnings(max(as.Date(substr(as.character(d$date), 1, 10)),
                         na.rm = TRUE))
  }

  why <- character(0)
  if (nrow(df) < min_frac * nrow(prev))
    why <- c(why, sprintf("%d rows replacing %d", nrow(df), nrow(prev)))
  if (bars(df) < min_frac * bars(prev))
    why <- c(why, sprintf("median %.0f bars/symbol replacing %.0f",
                          bars(df), bars(prev)))
  nd <- newest(df); pd <- newest(prev)
  if (!is.na(nd) && !is.na(pd) && nd < pd)
    why <- c(why, sprintf("newest date %s is older than %s on disk",
                          format(nd), format(pd)))

  if (length(why) > 0) {
    # warning(), so it is echoed at the end of a step whose log runs to
    # thousands of lines. Keeping the good file is the safe outcome, so this
    # must not halt the run — the rest of the pipeline reads the kept file and
    # produces correct output from it. It must simply stop being invisible.
    warning(sprintf(
      "%s: refusing to overwrite — %s. Keeping the %d rows already on disk. This is a short or truncated upstream pull, not a shrinking universe.",
      label, paste(why, collapse = "; "), nrow(prev)), call. = FALSE)
    return(invisible(FALSE))
  }

  readr::write_csv(df, path)
  invisible(TRUE)
}
