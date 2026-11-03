# Telling the reader the right thing to do about a dead upstream feed.
#
# Alpha Vantage stopped answering on 2026-10-06 and the pipeline said, for 28
# days: "Check AV_KEY: expired, revoked, or over its daily cap." That advice
# was wrong. The key was valid the whole time. Alpha Vantage had moved the
# OVERVIEW and NEWS_SENTIMENT endpoints behind a paid plan, and every request
# came back with "This is a premium endpoint." Rotating the secret could not
# have helped, and the remediation sent the reader to do exactly that.
#
# AV signals its refusals in three keys, and they do not mean the same thing:
#   Note          -> rate limited
#   Information   -> premium endpoint, or another refusal
#   Error Message -> malformed or invalid request
# These tests pin the distinction and the advice that follows from it.

if (have_pkgs("dplyr", "readr", "glue", "httr", "jsonlite")) {
  suppressMessages({ library(dplyr) })
  SOURCED_BY_MASTER <- TRUE
  suppressMessages(try(source(repo_path("R", "01_fundamentals.R"), local = TRUE),
                       silent = TRUE))

  # The real strings, as the 2026-11-03 nightly log recorded them.
  PREMIUM <- paste("Thank you for using Alpha Vantage! This is a premium endpoint.",
                   "You may subscribe to any of the premium plans at",
                   "https://www.alphavantage.co/premium/ to instantly unlock all",
                   "premium endpoints")
  QUOTA <- paste("Thank you for using Alpha Vantage! Our standard API rate limit",
                 "is 25 requests per day.")
  BADKEY <- "the parameter apikey is invalid or missing."

  test_that("a premium refusal is not mistaken for a key problem", {
    skip_if_not(exists("av_classify"), "R/01_fundamentals.R not sourced")
    expect_equal(av_classify("Information", PREMIUM), "premium")
    expect_equal(av_classify("Information", QUOTA), "quota")
    expect_equal(av_classify("Note", QUOTA), "quota")
    expect_equal(av_classify("Error Message", BADKEY), "other")
    # Note always means rate limiting, whatever the body says.
    expect_equal(av_classify("Note", PREMIUM), "premium")
  })

  test_that("the remedy names the actual cause", {
    skip_if_not(exists("av_remedy"), "R/01_fundamentals.R not sourced")
    skip_if_not(exists("AV_REFUSALS"), "refusal record not present")
    clear <- function() rm(list = ls(AV_REFUSALS), envir = AV_REFUSALS)

    clear()
    expect_match(av_remedy(), "AV_KEY")

    clear(); assign("quota", "AAPL", envir = AV_REFUSALS)
    expect_match(av_remedy(), "rate limit")

    clear(); assign("premium", "AAPL", envir = AV_REFUSALS)
    r <- av_remedy()
    # It must say plainly that replacing the secret is not the fix. Anything
    # less and the reader does the one thing that cannot work.
    expect_match(r, "not fix it", fixed = TRUE)
    expect_match(r, "premium", fixed = TRUE)
    # And it must point at the free source the pipeline already has.
    expect_match(r, "sec_financials", fixed = TRUE)
    clear()
  })

  test_that("the old unconditional key advice is gone", {
    src <- readLines(repo_path("R", "01_fundamentals.R"), warn = FALSE)
    code <- grep("^\\s*#", src, invert = TRUE, value = TRUE)
    # The exact wrong sentence, and the assumption behind it.
    expect_false(any(grepl("expired, revoked, or over its daily cap", code,
                           fixed = TRUE)))
    # The failure message must route through the classifier, not hard-code one
    # cause. A literal remedy string here is the regression.
    expect_true(any(grepl("av_remedy()", code, fixed = TRUE)))
  })

  test_that("the nightly does not send the reader to rotate a working key", {
    wf <- readLines(repo_path(".github", "workflows", "daily-update.yml"),
                    warn = FALSE)
    txt <- paste(wf, collapse = "\n")
    expect_false(grepl("the key is expired, revoked, or over its", txt,
                       fixed = TRUE))
    # The dark-feed step must mention the premium case and say the key is fine.
    at <- grep("^\\s*- name:\\s*Fail on a dark upstream feed\\s*$", wf)
    expect_length(at, 1L)
    j <- at + 1L
    body <- character(0)
    while (j <= length(wf) && !grepl("^\\s*- name:", wf[j])) {
      body <- c(body, wf[j]); j <- j + 1L
    }
    step <- paste(body, collapse = " ")
    expect_match(step, "premium", fixed = TRUE)
    expect_match(step, "key is fine", fixed = TRUE)
  })
}
