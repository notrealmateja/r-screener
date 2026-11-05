# The squeeze score's ceiling, and the tie it manufactured.
#
# squeeze_score_raw is a weighted average of four percent-ranked terms, and it
# cannot exceed 97: st_score tops out at 80, so 0.25*100 + 0.20*100 + 0.40*100
# + 0.15*80 = 97. A confirmed setup then earned a 1.20 MULTIPLIER on top of
# that, which runs to 116.4 -- past the end of the scale the number is read on
# -- and pmin(.., 100) clipped the overflow flat.
#
# On 2026-11-04 nine of 195 names cleared raw 83.34 and so all scored exactly
# 100.00: CAVA, HALO, LOVE, OLLI, PTGX, TROW, UPST, WING, WLDN. Their real
# spread was 11.92 points, UPST at 112.13 down to CAVA at 100.21.
#
# A tie is not just a cosmetic problem at the top of a ranking. dplyr::arrange
# is stable, so ties keep row order; master_scored.csv is written in
# alphabetical order; and the Top Squeeze panel took head(8) of the result.
# CAVA, the WEAKEST of the nine, sorted first and was presented as the
# strongest name in the universe, and the ninth was pushed off the page by an
# alphabetical accident.

if (have_pkgs("dplyr", "tibble", "readr", "glue", "httr", "jsonlite")) {
  suppressMessages({ library(dplyr); library(tibble) })
  SOURCED_BY_MASTER <- TRUE
  suppressMessages(try(source(repo_path("R", "03_data.R"), local = TRUE),
                       silent = TRUE))

  # The real inputs, not a fixture: data/squeeze_scored.csv carries every
  # column build_squeeze_score consumes, so the function can be re-run over
  # the exact book that produced the tie.
  live <- local({
    sp <- repo_path("data", "squeeze_scored.csv")
    fp <- repo_path("data", "fundamentals_scored.csv")
    if (!file.exists(sp) || !file.exists(fp)) return(NULL)
    s <- suppressWarnings(readr::read_csv(sp, show_col_types = FALSE))
    f <- suppressWarnings(readr::read_csv(fp, show_col_types = FALSE))
    need <- c("symbol", "short_percent_float", "short_ratio", "short_trend")
    if (!all(need %in% names(s))) return(NULL)
    if (!all(c("revenue_growth", "earningsGrowth", "profit_margin") %in% names(f)))
      return(NULL)
    if (!exists("build_squeeze_score")) return(NULL)
    tryCatch(suppressMessages(build_squeeze_score(s[need], f)),
             error = function(e) NULL)
  })

  test_that("the bonus cannot push a score past the top of its own scale", {
    skip_if(is.null(live), "live squeeze inputs unavailable")
    # No clip, because nothing can overflow: raw + (100 - raw) * bonus is a
    # weighted average of raw and 100 whenever bonus is in [0, 1].
    expect_true(all(live$squeeze_score <= 100))
    expect_true(all(live$squeeze_score >= 0))
    expect_true(all(live$squeeze_score >= live$squeeze_score_raw - 1e-9))
    # And the clip itself is gone from the source, not merely unreached.
    src <- paste(readLines(repo_path("R", "03_data.R"), warn = FALSE),
                 collapse = "\n")
    expect_false(grepl("pmin(squeeze_score_raw", src, fixed = TRUE))
    expect_match(src,
      "squeeze_score = squeeze_score_raw \\+ \\(100 - squeeze_score_raw\\) \\* squeeze_bonus")
  })

  test_that("no name is clipped to the ceiling any more", {
    skip_if(is.null(live), "live squeeze inputs unavailable")
    # The symptom: a pile of names sharing one value because the scale ran out.
    expect_lt(sum(live$squeeze_score >= 100 - 1e-9), 2)
    top <- max(live$squeeze_score, na.rm = TRUE)
    expect_equal(sum(live$squeeze_score >= top - 1e-9), 1L)
    # The nine that used to tie are now all distinct from one another.
    nine <- c("CAVA", "HALO", "LOVE", "OLLI", "PTGX", "TROW", "UPST", "WING", "WLDN")
    got <- live[live$symbol %in% nine, ]
    skip_if(nrow(got) < length(nine), "the 2026-11-04 book is not the current one")
    expect_equal(dplyr::n_distinct(got$squeeze_score), length(nine))
  })

  test_that("the score keeps the order the four components produced", {
    skip_if(is.null(live), "live squeeze inputs unavailable")
    # Strict monotonicity in raw is the property the clip destroyed, and it is
    # what makes the ranking mean anything. Checked within the bonus group and
    # within the unbonused group, since the two sit on different lines.
    for (b in unique(live$squeeze_bonus)) {
      g <- live[live$squeeze_bonus == b & !is.na(live$squeeze_score_raw), ]
      g <- g[order(g$squeeze_score_raw), ]
      if (nrow(g) < 3) next
      expect_false(is.unsorted(g$squeeze_score),
                   info = sprintf("score not monotone in raw at bonus %s", b))
      ds <- diff(g$squeeze_score)
      dr <- diff(g$squeeze_score_raw)
      expect_true(all(ds[dr > 1e-9] > 0),
                  info = sprintf("distinct raws collapsed to one score at bonus %s", b))
    }
    # UPST had the highest raw of the nine and CAVA the lowest, so the fix has
    # to put them at opposite ends rather than side by side at 100.
    g <- live[live$symbol %in% c("UPST", "CAVA"), ]
    skip_if(nrow(g) < 2, "the 2026-11-04 book is not the current one")
    expect_gt(g$squeeze_score[g$symbol == "UPST"],
              g$squeeze_score[g$symbol == "CAVA"])
  })

  test_that("the bonus is still worth having", {
    skip_if(is.null(live), "live squeeze inputs unavailable")
    # A fix that quietly neutered the bonus would pass every test above.
    expect_true(any(live$squeeze_bonus > 0))
    b <- live[live$squeeze_bonus > 0, ]
    expect_true(all(b$squeeze_score > b$squeeze_score_raw))
    # Worth a fifth of the distance remaining to 100. squeeze_score is stored
    # rounded to two decimals, so this is checked to that precision, not finer.
    expect_equal(mean((b$squeeze_score - b$squeeze_score_raw) /
                        (100 - b$squeeze_score_raw)), 0.20, tolerance = 1e-3)
    # What the bonus is worth at the High Conviction cut: it lowers the raw a
    # name needs from 80 to 75, so the cut and the bonus still interact.
    #
    # Evaluated at the boundary rather than searched for in the book. Today's
    # universe contains no bonused name with a raw between 75 and 80, so an
    # any() over the live rows would assert a fact about this Tuesday's data
    # and fail on a different one. The formula itself is pinned by the exact
    # source line asserted in the first test above.
    hc <- function(raw, bn) raw + (100 - raw) * bn >= 80
    expect_true(hc(75, 0.20))
    expect_false(hc(74.9, 0.20))
    expect_false(hc(79.9, 0))
    expect_true(hc(80, 0))
  })
}

test_that("the Top Squeeze panel sorts before it takes the head", {
  app <- repo_path("app", "app.R")
  skip_if_not(file.exists(app), "app/app.R not present")
  src <- readLines(app, warn = FALSE)
  a <- grep("output\\$squeeze_top <- renderDT", src)
  expect_length(a, 1)
  blk <- paste(src[a[1]:(a[1] + 14)], collapse = "\n")

  # head(8) over an unsorted frame is not a top eight. master_scored.csv is
  # alphabetical, so this panel was showing A-names and calling them the top.
  expect_match(blk, "head\\(8\\)")
  expect_match(blk, "arrange\\(desc\\(squeeze_score\\)\\)")
  # The sort has to come first, or it orders eight already-chosen rows.
  expect_lt(regexpr("arrange(desc(squeeze_score))", blk, fixed = TRUE)[[1]],
            regexpr("head(8)", blk, fixed = TRUE)[[1]])
  # And it must sort on the score, not on the renamed display column, which
  # does not exist until after select().
  expect_lt(regexpr("arrange(desc(squeeze_score))", blk, fixed = TRUE)[[1]],
            regexpr("select(Symbol=symbol", blk, fixed = TRUE)[[1]])
})

test_that("master_scored.csv is alphabetical, which is why the sort is needed", {
  f <- repo_path("app", "master_scored.csv")
  skip_if_not(file.exists(f), "master_scored.csv not present")
  d <- suppressWarnings(readr::read_csv(f, show_col_types = FALSE,
                                        col_types = readr::cols(.default = "c")))
  skip_if(!"symbol" %in% names(d) || nrow(d) < 10, "no symbol column")
  # Not a style preference: it is the premise of the bug above. If this file
  # ever starts arriving sorted by score, the panel still needs its arrange,
  # but this comment stops being the explanation for what went wrong.
  expect_false(is.unsorted(d$symbol))
})
