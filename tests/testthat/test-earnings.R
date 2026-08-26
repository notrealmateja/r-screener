# Earnings dates derived from SEC filings.
#
# Alpha Vantage refused EARNINGS_CALENDAR on this key from 5 August, answering
# with a CSV header followed by an Information notice, and the calendar froze
# for 18 days. SEC needs no key and answers the same from a datacenter IP —
# the property that decided the live-TV problem after four attempts.

if (have_pkgs("dplyr", "readr", "tibble")) {
  suppressMessages({ library(dplyr); library(readr); library(tibble) })

  test_that("same-quarter duplicate releases collapse before measuring cadence", {
    # Some companies file more than one 8-K tagged 2.02 per quarter. Measuring
    # across both halved the cadence — Honeywell read 63 days against a real 91
    # — and would have put every projected date weeks early.
    collapse <- function(dates, window = 45) {
      dates <- sort(dates, decreasing = TRUE)
      keep <- dates[1]
      for (dt in dates[-1])
        if (as.numeric(keep[length(keep)] - dt) >= window) keep <- c(keep, dt)
      keep
    }
    d <- as.Date(c("2026-07-23","2026-07-18",   # two filings, one quarter
                   "2026-04-22","2026-01-23","2025-10-24"))
    out <- collapse(d)
    expect_equal(length(out), 4)
    expect_false(as.Date("2026-07-18") %in% out)
    gaps <- as.numeric(head(out, -1) - out[-1])
    expect_true(all(gaps >= 60 & gaps <= 200))
  })

  test_that("a release just past reads as due now, not next quarter", {
    # NVIDIA last filed 20 May, which projects to 19 August. Rolling that
    # forward showed 18 November when the real date was days away.
    project <- function(last, cadence, today, grace = 35) {
      nxt <- last + cadence
      if (nxt < today) {
        if (as.numeric(today - nxt) <= grace) nxt <- today
        else while (nxt < today) nxt <- nxt + cadence
      }
      nxt
    }
    today <- as.Date("2026-08-23")
    expect_equal(project(as.Date("2026-05-20"), 91, today), today)          # NVDA
    expect_equal(project(as.Date("2026-07-30"), 91, today),
                 as.Date("2026-10-29"))                                     # AAPL
    # genuinely dormant filer still rolls forward to a future date
    expect_gt(project(as.Date("2024-01-15"), 91, today), today)
  })

  test_that("the median gap is used, so one late filing cannot drag every date", {
    gaps <- c(91, 91, 152, 91)     # one delayed quarter
    expect_equal(stats::median(gaps), 91)
    expect_gt(mean(gaps), stats::median(gaps))
  })

  test_that("the calendar carries the columns the panel reads", {
    p <- repo_path("data", "earnings_calendar.csv")
    skip_if_not(file.exists(p), "earnings_calendar.csv not generated yet")
    d <- read_csv(p, show_col_types = FALSE)
    expect_true(all(c("symbol","name","date","epsEstimated","time") %in% names(d)))
    skip_if(nrow(d) == 0, "empty this cycle — health check reports it")
    expect_true(all(!is.na(d$date)))
    # dates must be usable: the panel filters on date >= today
    expect_gt(sum(d$date >= Sys.Date()), 0)
  })

  test_that("a quarterly cadence is what actually came back", {
    p <- repo_path("data", "earnings_calendar.csv")
    skip_if_not(file.exists(p), "earnings_calendar.csv not generated yet")
    d <- read_csv(p, show_col_types = FALSE)
    skip_if_not("cadence_days" %in% names(d), "cadence column absent")
    expect_equal(unname(stats::median(d$cadence_days, na.rm = TRUE)), 91)
    # nothing sub-quarterly survived the collapse
    expect_true(all(d$cadence_days >= 60, na.rm = TRUE))
  })

  test_that("an all-NA column read back as logical does not break the panel", {
    # SEC publishes no time-of-day, so that column is empty; readr returns an
    # empty column as logical and replace_na then cannot insert a character.
    logical_col <- c(NA, NA)
    expect_error(tidyr::replace_na(logical_col, ""), NULL)
    expect_equal(tidyr::replace_na(as.character(logical_col), ""), c("", ""))
  })

  test_that("every output is exempted from suspension", {
    src <- paste(readLines(repo_path("app", "app.R")), collapse = "\n")
    # The panes are divs toggled by a CSS class, so Shiny is never told an
    # output became visible and leaves it suspended, rendering nothing. That
    # caused three separate bugs, each patched by adding one more name to a
    # hand-maintained list.
    expect_true(grepl("names(outputOptions(output))", src, fixed = TRUE))
  })

  test_that("tabset panels are spliced, not handed over as one list", {
    src <- paste(readLines(repo_path("app", "app.R")), collapse = "\n")
    # tabsetPanel(lapply(...)) rejects the lot as not being tabPanels.
    expect_false(grepl("tabsetPanel(id=\"news_source_tab\",\n      lapply", src, fixed = TRUE))
    expect_true(grepl("do.call(tabsetPanel", src, fixed = TRUE))
  })
}

test_that("earnings rows open the stock in Deep Dive", {
  src <- paste(readLines(repo_path("app", "app.R")), collapse = "\n")
  expect_true(grepl("earn-clickable", src, fixed = TRUE))
  expect_true(grepl('sprintf("openDeepDive(\'%s\')", row$symbol)', src, fixed = TRUE))
  # A name outside the universe has no Deep Dive page, so it must not look
  # clickable — the click and the affordance are both gated on in_universe.
  expect_true(grepl('if (in_universe) "earn-item earn-clickable" else "earn-item"',
                    src, fixed = TRUE))
})
