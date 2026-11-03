# Verify the nightly pipeline output before anything is published.
#
# This is a FILE, not an Rscript -e argument, and that is the whole point.
#
# R caps an `Rscript -e` expression, but NOT at a count of the bytes you wrote.
# `$R_HOME/bin/R` is a shell script, and before R sees the argument it rewrites
# every space as `~+~`, every newline as `~n~` and every tab as `~t~` (lines
# 196-197). Each of those costs THREE bytes against the limit, so indented
# multi-line R measures about 1.5x its own size. Over the limit R prints
#     WARNING: '-e <encoded expression>'
# to STDOUT (not stderr), discards the expression, and falls through to reading
# a program from stdin. On a runner stdin is /dev/null, so it reads EOF, finds
# nothing to do, and EXITS 0. The step runs nothing and reports success.
#
# This program was inlined that way. It went silent at commit 6e17f66 on
# 2026-10-26 -- 9109 raw bytes, which any raw-byte threshold would have passed,
# but 13871 encoded. The first affected nightly is run #246 on 2026-10-27,
# whose verify step emitted the WARNING and nothing else; run #245 the night
# before had failed loudly with a bash syntax error. 6e17f66 is titled "An
# apostrophe in a comment was taking the nightly down": fixing the apostrophe
# converted a loud failure into a silent one, and nobody looked again.
#
# Seven nightly commits published with no verification at all: 10-27, 10-28,
# 10-29, 10-31, 11-01, 11-02 and 11-03. Not just the dark-feed gate -- the
# core-file checks, the run-stamp provenance check that 856380c was written to
# add, and the step-summary table all live here, so the Data health table was
# missing from every one of those runs. `steps.verify.outcome != 'failure'` on
# the commit gate was vacuous: a step that runs nothing cannot fail.
#
# Measured on R 4.5.2 by bisection: an argument of 9997 encoded bytes runs and
# 9998 is refused, and an all-space payload reaches that same ceiling at
# exactly one third of the raw bytes a whitespace-free one does -- which is how
# the encoding was confirmed rather than inferred. The limit is cumulative
# across all -e arguments in one invocation, not per argument. In this workflow
# the largest block known to have run is 8043 encoded (the verify step at
# 9d9bc94); the one R threw away was 13871. The ceiling belongs to the R build,
# so tools/lint_workflow_r.R refuses anything over 8000 rather than sitting
# near it.
#
# As a script file the same mistake is impossible. There is no length limit, a
# syntax error exits non-zero and names a line, and apostrophes are allowed --
# the no-apostrophe rule in tools/lint_workflow_r.R exists only because bash
# closes a single-quoted -e argument at the first apostrophe.
#
# Run from the repository root:  Rscript tools/verify_pipeline.R

suppressMessages(library(readr))
files <- c("data/master_scored.csv","data/top15_sweetspot.csv",
           "data/top15_unicorns.csv","data/alpha_history.csv",
           "data/price_history.csv",
           "data/market_news.csv","data/earnings_calendar.csv",
           "data/wsb_trending.csv","data/stocktwits_trending.csv",
           "data/macro_data.csv","data/sector_performance.csv",
           "data/stock_news.csv","data/sec_filings.csv",
           "data/sec_financials.csv","data/tv_channels.csv")
# Age is measured from the last commit that CHANGED the file, so a
# legitimately slow-moving file would otherwise trip STALE every day.
# Company financials only move when a 10-Q or 10-K lands.
max_age <- c("data/sec_financials.csv" = 24 * 120)
out <- c("## Data health", "",
         "| File | Rows | Age (h) | Status |", "|---|---:|---:|---|")
problems <- character(0)
# Split hard breakage from soft noise. Only the files every screen
# reads are fatal; news/sentiment/TV feeds fail for reasons outside
# this pipeline and must not halt the run. Staleness is deliberately
# NOT fatal: a weekend run produces identical data, nothing gets
# committed, and the last-commit age crosses the threshold by Monday
# through no fault of the pipeline.
CORE <- c("data/master_scored.csv", "data/top15_sweetspot.csv",
          "data/top15_unicorns.csv", "data/price_history.csv",
          "data/macro_data.csv")
hard <- character(0)
for (f in files) {
  if (!file.exists(f)) {
    out <- c(out, sprintf("| `%s` | - | - | MISSING |", basename(f)))
    problems <- c(problems, paste(basename(f), "missing"))
    if (f %in% CORE) hard <- c(hard, paste(basename(f), "missing"))
    next
  }
  n <- tryCatch(nrow(suppressWarnings(read_csv(f, show_col_types = FALSE))),
                error = function(e) NA_integer_)
  # Deliberately not modification time: actions/checkout rewrites every file at
  # checkout time, so mtime is always ~0 here and STALE never fired.
  # The last commit that touched the file is the real data age.
  # %ct is epoch seconds — %cI emits a trailing "Z" that strptime %z
  # will not parse, which silently produced NA for every file.
  ts  <- suppressWarnings(system2("git", c("log","-1","--format=%ct","--",f),
                                  stdout = TRUE, stderr = FALSE))
  age <- if (length(ts) == 1 && nzchar(ts) && !is.na(suppressWarnings(as.numeric(ts))))
           round((as.numeric(Sys.time()) - as.numeric(ts)) / 3600, 1)
         else NA_real_
  status <- "OK"
  if (is.na(n) || n == 0) {
    status <- "EMPTY"
    problems <- c(problems, paste(basename(f), "empty"))
    if (f %in% CORE) hard <- c(hard, paste(basename(f), "empty"))
  } else if (!is.na(age) && age > (if (f %in% names(max_age)) max_age[[f]] else 36)) {
    status <- "STALE"
    problems <- c(problems, paste(basename(f), "stale"))
  }
  out <- c(out, sprintf("| `%s` | %s | %s | %s |", basename(f), n, age, status))
}
# A row count cannot catch the failure that matters for the valuation
# inputs: if SEC renames an XBRL concept the fetch still returns one
# row per ticker with every value NA, write_or_keep sees a non-empty
# frame and overwrites the good data. Check coverage, not row count.
if (file.exists("data/sec_financials.csv")) {
  sfin <- tryCatch(read_csv("data/sec_financials.csv", show_col_types = FALSE),
                   error = function(e) NULL)
  if (is.null(sfin) || !"fcf_sec" %in% names(sfin)) {
    problems <- c(problems, "sec_financials unreadable or missing fcf_sec")
    out <- c(out, "", "**DCF inputs:** unreadable.")
  } else {
    cov <- sum(!is.na(sfin$fcf_sec))
    cagr <- if ("rev_cagr_sec" %in% names(sfin)) sum(!is.na(sfin$rev_cagr_sec)) else 0
    out <- c(out, "",
             sprintf("**DCF inputs:** free cash flow %d/%d, revenue CAGR %d/%d.",
                     cov, nrow(sfin), cagr, nrow(sfin)))
    if (cov < 100)
      problems <- c(problems,
                    sprintf("DCF free-cash-flow coverage collapsed to %d/%d", cov, nrow(sfin)))
  }
}

# Per-symbol price coverage. Row counts cannot see this: the file
# can be the right size while individual tickers carry nothing.
# WBS has had ZERO price rows and CRNX eight for over a week, and
# every check passed every night. Both were still scored, ranked
# and displayed, and a 17-observation CRNX was being announced in
# the run log as the best alpha of the day.
# No apostrophes anywhere in this block: it is embedded in the
# shell inside a single-quoted Rscript argument, so one apostrophe
# closes that quote and bash tries to parse the rest as shell.
# That took the nightly down for a day. tools/lint_workflow_r.R
# now refuses the whole file if one reappears.
if (file.exists("data/price_history.csv") &&
    file.exists("data/master_scored.csv")) {
  ph <- tryCatch(read_csv("data/price_history.csv",
                          col_select = c("symbol", "daily_ret"),
                          show_col_types = FALSE), error = function(e) NULL)
  ms <- tryCatch(read_csv("data/master_scored.csv",
                          col_select = "symbol",
                          show_col_types = FALSE), error = function(e) NULL)
  if (!is.null(ph) && !is.null(ms) && nrow(ph) > 0 && nrow(ms) > 0) {
    cov  <- table(ph$symbol[!is.na(ph$daily_ret)])
    ref  <- stats::median(as.numeric(cov))
    none <- setdiff(unique(ms$symbol), names(cov))
    thin <- names(cov)[as.numeric(cov) < 0.5 * ref]
    out <- c(out, "",
             sprintf("**Price coverage:** %d/%d symbols have history (median %d days).",
                     length(unique(ms$symbol)) - length(none),
                     length(unique(ms$symbol)), as.integer(ref)))
    if (length(none))
      out <- c(out, sprintf("No price history at all: %s.",
                            paste(sort(none), collapse = ", ")))
    if (length(thin))
      out <- c(out, sprintf("Under half the median: %s.",
                            paste(sort(thin), collapse = ", ")))
    # Warn, do not fail: a thin feed now costs the symbol its
    # alpha score rather than mis-scoring it. Under 20 paired
    # observations compute_beta returns NA, the residual is left
    # NA, and pct_scale places the symbol at neutral 50 with a
    # confidence weight near zero, so these names drop out of the
    # ranking instead of entering it on raw excess vs SPY.
    # Failing the run would stop a healthy nightly over two
    # stale tickers.
    if (length(none) + length(thin) > 0)
      problems <- c(problems,
                    sprintf("%d symbol(s) with missing or thin price history",
                            length(none) + length(thin)))
    # Fatal ONLY when the pull is so broken the site would be
    # useless anyway.
    #
    # This used to be fatal above 10%, which was a mistake and a
    # costly one. The commit step is gated on this check passing,
    # so a tenth of the universe pulling short stopped the ENTIRE
    # nightly refresh — no price update, no scores, no deploy —
    # and reopened the failure issue every night until someone
    # looked. A coverage warning is a reason to investigate, not a
    # reason to publish nothing: the good data already on disk plus
    # a partial pull beats a site frozen for days.
    #
    # 10% is well inside the range an ordinary flaky Yahoo night
    # produces. Half the universe is not; that is the pull itself
    # failing, and committing it would overwrite good data with
    # rubbish, which is what the gate exists to prevent.
    if (length(none) + length(thin) > 0.5 * length(unique(ms$symbol)))
      hard <- c(hard, sprintf("price coverage collapsed: %d of %d symbols unusable",
                              length(none) + length(thin),
                              length(unique(ms$symbol))))
  }
}

if (file.exists("data/tv_channels.csv")) {
  tv <- tryCatch(read_csv("data/tv_channels.csv", show_col_types = FALSE),
                 error = function(e) NULL)
  if (!is.null(tv) && "is_live" %in% names(tv)) {
    nlive <- sum(as.logical(tv$is_live), na.rm = TRUE)
    out <- c(out, "", sprintf("**Live TV:** %d/%d channels streaming.", nlive, nrow(tv)))
    # Several major broadcasters run 24/7, so zero live means the
    # resolver broke rather than everyone going off air at once.
    if (nlive == 0) problems <- c(problems, "no live TV channels resolved")
  }
}

# UPSTREAM FEED HEALTH. Everything above inspects pipeline OUTPUTS.
# Those get rewritten every night from whatever the caches hold, so
# a dead upstream credential leaves every output present, every row
# count healthy, every age fresh -- and the numbers quietly frozen.
# The Alpha Vantage key stopped answering on 2026-10-06 and the run
# log went on reporting 194/195 full fundamentals for 26 days.
#
# Age comes from the file CONTENT, never from git. A cache that has
# stopped updating is never committed, so its commit age is the age
# of the last SUCCESS and says nothing about today.
feeds <- list(
  list(file = "data/av_cache.csv", col = "last_fetched",
       label = "Alpha Vantage fundamentals", warn = 14, dark = 21),
  list(file = "data/market_news.csv", col = "publishedDate",
       label = "Alpha Vantage news", warn = 7, dark = 14),
  # write_history_or_keep in R/00_utils.R now refuses to replace
  # this file with a short or truncated pull, which protects the
  # history but leaves the prices frozen until the upstream
  # recovers. Keeping the good data is the right call; letting it
  # go unreported was how Alpha Vantage stayed dark for 26 days.
  # Thresholds are in calendar days and sized for the longest
  # legitimate market closure, so a holiday weekend cannot trip it.
  list(file = "data/price_history.csv", col = "date",
       label = "Price history", warn = 7, dark = 12)
)
dark <- character(0)
out <- c(out, "", "### Upstream feeds", "",
         "| Source | Last answered | Age (d) | Status |",
         "|---|---|---:|---|")
for (fd in feeds) {
  if (!file.exists(fd$file)) {
    out <- c(out, sprintf("| %s | - | - | MISSING |", fd$label))
    next
  }
  dd <- tryCatch(suppressWarnings(
           read_csv(fd$file, show_col_types = FALSE)),
         error = function(e) NULL)
  if (is.null(dd) || !(fd$col %in% names(dd)) || nrow(dd) == 0) {
    out <- c(out, sprintf("| %s | - | - | UNREADABLE |", fd$label))
    next
  }
  # Timestamps appear as both dates and ISO datetimes, so take the
  # leading 10 characters rather than guessing a format.
  seen <- suppressWarnings(max(as.Date(
            substr(as.character(dd[[fd$col]]), 1, 10)), na.rm = TRUE))
  if (!is.finite(as.numeric(seen))) {
    out <- c(out, sprintf("| %s | - | - | NO DATES |", fd$label))
    next
  }
  age <- as.numeric(Sys.Date() - seen)
  st <- if (age >= fd$dark) "DARK"
        else if (age >= fd$warn) "STALE" else "ok"
  out <- c(out, sprintf("| %s | %s | %d | %s |",
                        fd$label, format(seen), age, st))
  if (identical(st, "DARK")) {
    dark <- c(dark, sprintf("%s silent %d days", fd$label, age))
  } else if (identical(st, "STALE")) {
    problems <- c(problems,
                  sprintf("%s last answered %d days ago", fd$label, age))
  }
}
# Handed to a later step rather than stopped on here. A dead
# third-party credential must not block publication: the pipeline
# still regenerated everything it could, and withholding all of it
# would take the whole site down over one panel.
gh_out <- Sys.getenv("GITHUB_OUTPUT")
if (nzchar(gh_out) && length(dark) > 0)
  cat(sprintf("dark_feeds=%s\n", paste(dark, collapse = "; ")),
      file = gh_out, append = TRUE)

# PROVENANCE. Every check above inspects files sitting in the
# working tree, which a fresh checkout supplies whether or not this
# run produced them. So all of it passes when the pipeline was
# skipped. On 2026-10-29 it did exactly that: the install step
# failed, the pipeline never ran, every check here went green
# against data committed two nights earlier, and the commit step
# published "Auto-update: Daily alpha refresh 2026-10-29" whose
# entire content was a reshuffled manifest. Tie the verdict to the
# run that is executing.
stamp <- "data/.run_stamp"
run_id <- Sys.getenv("GITHUB_RUN_ID")
if (!file.exists(stamp)) {
  hard <- c(hard, "pipeline never ran (no run stamp)")
} else {
  stamped <- trimws(readLines(stamp, warn = FALSE)[1])
  if (nzchar(run_id) && !identical(stamped, run_id)) {
    hard <- c(hard, sprintf("run stamp is from run %s, not %s",
                            stamped, run_id))
  } else {
    # The stamp proves the pipeline STARTED. Whether it achieved
    # anything is a separate question, so count the core files it
    # actually rewrote. Partial credit is deliberate here: one
    # rewritten core file is enough to publish, zero is not.
    ref <- file.info(stamp)$mtime
    cand <- Sys.glob("data/*.csv")
    mt <- file.info(cand)$mtime
    rewrote <- cand[!is.na(mt) & mt >= ref]
    core_fresh <- intersect(CORE, rewrote)
    out <- c(out, "",
             sprintf("**Provenance:** %d/%d core files rewritten by this run.",
                     length(core_fresh), length(CORE)))
    if (length(core_fresh) == 0) {
      hard <- c(hard, "pipeline ran but rewrote no core data file")
    } else {
      cold <- setdiff(CORE, rewrote)
      if (length(cold) > 0)
        problems <- c(problems, paste("not rewritten this run:",
                       paste(basename(cold), collapse = ", ")))
    }
  }
}

out <- c(out, "")
if (length(problems) > 0) {
  out <- c(out, sprintf("**%d issue(s):** %s", length(problems),
                        paste(problems, collapse = "; ")))
} else {
  out <- c(out, "All outputs present and fresh.")
}
cat(paste(out, collapse = "\n"), "\n")
sf <- Sys.getenv("GITHUB_STEP_SUMMARY")
if (nzchar(sf)) writeLines(out, sf)

# Record that the checks above actually ran and reached a verdict.
#
# Without this the pipeline cannot tell "checked, all clean" from "never
# checked": both leave dark_feeds unset, so the dark-feed gate is skipped
# either way. That is exactly how the oversized -e block hid for seven nights.
# A later step requires this to be 1, so an absent verdict fails the run
# instead of passing silently.
gh_out <- Sys.getenv("GITHUB_OUTPUT")
if (nzchar(gh_out))
  cat("verify_ran=1\n", file = gh_out, append = TRUE)
# Report red when a file the app cannot run without is missing or
# empty. Previously every problem was written to the summary and the
# job still went green, so a source returning zero rows looked
# identical to a healthy night.
if (length(hard) > 0)
  stop("Core data unusable: ", paste(hard, collapse = "; "))
