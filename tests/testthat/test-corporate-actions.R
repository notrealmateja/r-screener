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
}
