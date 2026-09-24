# The live pipeline's two data-integrity guards.
#
# Both exist because the top two names by alpha on the live site were data
# artifacts: PRPL ranked #1 on a +2010% day and BRCC #2 on a +872% day, neither
# of which was a return. Yahoo has since withdrawn the BRCC bar entirely, but
# the pipeline could not pick that up, because it preferred whatever it had
# already written.

if (have_pkgs("dplyr", "tibble")) {
  suppressMessages({ library(dplyr); library(tibble) })
  source(repo_path("R", "00_utils.R"), local = TRUE)

  # ── the detector ─────────────────────────────────────────────────────────
  test_that("reverse splits are caught: price up while volume collapses", {
    expect_true(corporate_action(20.103, 0.099))   # PRPL 2026-07-20
    expect_true(corporate_action(8.716,  0.509))   # BRCC 2026-08-25
  })

  test_that("forward splits are caught by the mirror signature", {
    expect_true(corporate_action(-0.543, 2.351))   # IESC 2026-08-25
    expect_true(corporate_action(-0.5,   2.0))     # an exact 2:1
  })

  # This is the half that matters most. An earlier magnitude-only filter
  # deleted these and understated the model by about 4 points a year.
  test_that("genuine catalysts survive — every one arrived on rising volume", {
    real <- tibble::tribble(
      ~sym,   ~ret,    ~volx,
      "SANA",  1.606,   5.635,
      "OLMA",  1.364, 139.390,
      "FSLY",  0.723,   6.897,
      "SOUN",  0.667,   7.776,
      "VERA",  0.675,   5.985,
      "TDUP", -0.627,  11.126,
      "ZNTL",  0.601,  22.617,
      "RGTI",  0.580,   2.836,
      "MEDP",  0.547,   3.993,
      "STRL",  0.522,   3.289,
      "QBTS",  0.512,  11.178,
      "PRIM", -0.501,   5.475
    )
    flagged <- corporate_action(real$ret, real$volx)
    expect_false(any(flagged),
                 info = paste("wrongly flagged:", paste(real$sym[flagged], collapse = ", ")))
  })

  test_that("ordinary bars and unusable inputs never trip it", {
    expect_false(corporate_action(0.02, 1.1))
    expect_false(corporate_action(0.40, 0.5))        # under the move floor
    expect_false(corporate_action(0.80, NA_real_))   # no prior volume
    expect_false(corporate_action(NA_real_, 0.1))
    expect_false(corporate_action(0.80, 0))          # zero prior volume
  })

  test_that("the detector is vectorised and row-local", {
    r <- c(20.103, 0.02, -0.543, 0.723)
    v <- c(0.099,  1.10,  2.351, 6.897)
    expect_equal(corporate_action(r, v), c(TRUE, FALSE, TRUE, FALSE))
    # A row's verdict must not depend on its neighbours, or a 2026 event could
    # retroactively alter a 2024 bar.
    for (i in seq_along(r))
      expect_equal(corporate_action(r[i], v[i]), corporate_action(r, v)[i])
  })

  # ── history merge ────────────────────────────────────────────────────────
  test_that("a restated value replaces the stored one", {
    existing <- tibble(symbol = "BRCC", date = as.Date("2026-08-25"), daily_alpha = 8.716)
    fresh    <- tibble(symbol = "BRCC", date = as.Date("2026-08-25"), daily_alpha = 0.0)
    got <- merge_history(existing, fresh)
    expect_equal(nrow(got), 1)
    expect_equal(got$daily_alpha, 0.0)   # fresh wins; previously the stale 8.716 survived
  })

  test_that("dates older than the pull are still backfilled from disk", {
    existing <- tibble(symbol = "AAA",
                       date = as.Date(c("2023-01-01", "2026-01-01")),
                       daily_alpha = c(0.11, 0.99))
    fresh    <- tibble(symbol = "AAA", date = as.Date("2026-01-01"), daily_alpha = 0.22)
    got <- merge_history(existing, fresh)
    expect_equal(nrow(got), 2)
    expect_equal(got$daily_alpha[got$date == as.Date("2023-01-01")], 0.11)  # kept
    expect_equal(got$daily_alpha[got$date == as.Date("2026-01-01")], 0.22)  # refreshed
  })

  test_that("merge handles empty and missing inputs without erroring", {
    fresh <- tibble(symbol = "AAA", date = as.Date("2026-01-01"), daily_alpha = 0.5)
    expect_equal(nrow(merge_history(NULL, fresh)), 1)
    expect_equal(nrow(merge_history(fresh[0, ], fresh)), 1)
    expect_equal(nrow(merge_history(fresh, fresh[0, ])), 1)
  })

  test_that("keep_days trims history without dropping the fresh window", {
    fresh <- tibble(symbol = "AAA",
                    date = as.Date(c("2020-01-01", "2026-01-01")),
                    daily_alpha = c(0.1, 0.2))
    got <- merge_history(NULL, fresh, keep_days = 30, today = as.Date("2026-01-15"))
    expect_equal(nrow(got), 1)
    expect_equal(got$date, as.Date("2026-01-01"))
  })

  test_that("output is sorted and free of duplicate symbol/date pairs", {
    d <- tibble(symbol = c("B", "A", "A"),
                date = as.Date(c("2026-01-02", "2026-01-02", "2026-01-01")),
                daily_alpha = c(1, 2, 3))
    got <- merge_history(NULL, d)
    expect_equal(anyDuplicated(got[c("symbol", "date")]), 0)
    expect_equal(got$symbol, c("A", "A", "B"))
  })

  # Found by a debugging pass, in this function's own code. An undated row in
  # the fresh pull made min() return Inf, so `existing$date < Inf` kept every
  # stored row — silently restoring the stale-value bug merge_history exists to
  # prevent, and duplicating the symbol/date besides.
  test_that("undated fresh rows cannot resurrect a stale value", {
    existing <- tibble(symbol = "BRCC", date = as.Date("2026-08-25"), daily_alpha = 8.716)
    fresh    <- tibble(symbol = "BRCC", date = as.Date(NA),           daily_alpha = 0)
    got <- merge_history(existing, fresh)
    expect_equal(nrow(got), 1)                 # not 2
    expect_equal(got$daily_alpha, 8.716)       # nothing usable arrived, so disk stands
  })

  test_that("a dated fresh row still wins when other fresh rows are undated", {
    existing <- tibble(symbol = "BRCC", date = as.Date("2026-08-25"), daily_alpha = 8.716)
    fresh    <- tibble(symbol = "BRCC",
                       date = as.Date(c(NA, "2026-08-25")),
                       daily_alpha = c(9.9, 0))
    got <- merge_history(existing, fresh)
    expect_equal(nrow(got), 1)
    expect_equal(got$daily_alpha, 0)           # the corrected value replaces the stale one
  })

  test_that("merging emits no warning on undated input", {
    existing <- tibble(symbol = "A", date = as.Date("2026-01-01"), daily_alpha = 1)
    fresh    <- tibble(symbol = "A", date = as.Date(NA),           daily_alpha = 2)
    expect_silent(merge_history(existing, fresh))
  })

  # Found by a debugging pass. How far back the pull reached was taken as one
  # min() over the whole pull, so the longest series' start date was applied to
  # every symbol. A ticker that came back short lost the stored rows the pull
  # never covered, and a ticker missing from the pull was deleted outright —
  # from a file the pipeline commits. CRNX and WBS pull short in live data.
  test_that("a short pull does not delete the stored rows it never reached", {
    days <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")
    existing <- bind_rows(
      tibble(symbol = "AAA", date = days, daily_alpha = 0.001),
      tibble(symbol = "BBB", date = days, daily_alpha = 0.001))
    # AAA pulls the full year; BBB comes back with only the last five days.
    fresh <- bind_rows(
      tibble(symbol = "AAA", date = days,          daily_alpha = 0.002),
      tibble(symbol = "BBB", date = tail(days, 5), daily_alpha = 0.002))
    got <- merge_history(existing, fresh)
    expect_equal(sum(got$symbol == "BBB"), length(days))     # was 5
    # The five days the pull DID reach are still refreshed.
    expect_equal(unique(got$daily_alpha[got$symbol == "BBB" &
                                        got$date %in% tail(days, 5)]), 0.002)
    # The rest is the stored history, untouched.
    expect_equal(unique(got$daily_alpha[got$symbol == "BBB" &
                                        !(got$date %in% tail(days, 5))]), 0.001)
  })

  test_that("a symbol missing from the pull keeps its history", {
    existing <- bind_rows(
      tibble(symbol = "AAA", date = as.Date("2024-01-01") + 0:9, daily_alpha = 0.001),
      tibble(symbol = "CCC", date = as.Date("2024-01-01") + 0:9, daily_alpha = 0.001))
    fresh <- tibble(symbol = "AAA", date = as.Date("2024-01-01") + 0:9, daily_alpha = 0.002)
    got <- merge_history(existing, fresh)
    expect_true("CCC" %in% got$symbol)          # was erased entirely
    expect_equal(sum(got$symbol == "CCC"), 10)
  })

  test_that("one symbol's restatement does not reach across to another", {
    # The anti-stale property has to survive the per-symbol fix: BRCC's bad bar
    # is still overwritten, because the pull that restates it reaches it.
    existing <- bind_rows(
      tibble(symbol = "BRCC", date = as.Date("2026-08-25"), daily_alpha = 8.716),
      tibble(symbol = "AAA",  date = as.Date("2026-08-25"), daily_alpha = 0.01))
    fresh <- tibble(symbol = "BRCC", date = as.Date("2026-08-25"), daily_alpha = 0)
    got <- merge_history(existing, fresh)
    expect_equal(got$daily_alpha[got$symbol == "BRCC"], 0)      # corrected
    expect_equal(got$daily_alpha[got$symbol == "AAA"],  0.01)   # untouched
  })

  test_that("an undated stored row cannot inject a row of NAs", {
    # `existing$date < fresh_from` is NA for an undated stored row, and
    # `df[NA, ]` returns a whole row of NAs rather than nothing.
    existing <- bind_rows(
      tibble(symbol = "AAA", date = as.Date("2024-01-01") + 0:9, daily_alpha = 0.001),
      tibble(symbol = "BBB", date = as.Date(NA),                 daily_alpha = 0.002))
    fresh <- tibble(symbol = "AAA", date = as.Date("2024-01-08") + 0:2, daily_alpha = 0.009)
    got <- merge_history(existing, fresh, keep_days = 1120, today = as.Date("2024-01-11"))
    expect_equal(sum(is.na(got$symbol)), 0)
    expect_equal(sum(is.na(got$date)), 0)
  })

  # score_history's fresh frame is a single day. Dropping every stored row
  # at-or-after it deleted later days whenever the as-of date moved backward —
  # which it can now that the date comes from the price pull, not the clock.
  test_that("a single-day refresh does not delete the days after it", {
    existing <- tibble(symbol = "AAA",
                       date = as.Date(c("2026-09-19", "2026-09-20", "2026-09-22")),
                       master_score = c(1, 2, 3))
    fresh <- tibble(symbol = "AAA", date = as.Date("2026-09-19"), master_score = 99)
    got <- merge_history(existing, fresh)
    expect_equal(nrow(got), 3)                                    # was 1
    expect_equal(got$master_score[got$date == as.Date("2026-09-19")], 99)  # restated
    expect_equal(got$master_score[got$date == as.Date("2026-09-22")], 3)   # untouched
  })

  test_that("a bar withdrawn inside the pull's range still disappears", {
    # The bound must not weaken the case merge_history exists for: Yahoo
    # withdrew the BRCC bar, and a pull that spans it has to drop it.
    existing <- tibble(symbol = "BRCC",
                       date = as.Date(c("2026-08-24", "2026-08-25", "2026-08-26")),
                       daily_alpha = c(0.01, 8.716, 0.02))
    fresh <- tibble(symbol = "BRCC",
                    date = as.Date(c("2026-08-24", "2026-08-26")),
                    daily_alpha = c(0.01, 0.02))
    got <- merge_history(existing, fresh)
    expect_false(as.Date("2026-08-25") %in% got$date)   # phantom bar gone
    expect_equal(nrow(got), 2)
  })

  test_that("a NULL fresh pull returns the stored history instead of erroring", {
    existing <- tibble(symbol = "AAA", date = as.Date("2026-01-01"), daily_alpha = 1)
    expect_equal(nrow(merge_history(existing, NULL)), 1)
  })
}
