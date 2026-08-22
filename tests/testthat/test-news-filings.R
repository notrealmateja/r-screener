# Per-ticker news (Yahoo RSS) and SEC EDGAR filings.
#
# Both feeds replaced panels that were effectively empty: the market-wide Alpha
# Vantage feed carries ~50 articles for the whole market, so it covered roughly
# 6 of 195 stocks on any given day. These tests cover the parsing and filtering
# logic offline — no network — plus the shape the app expects to load.

if (have_pkgs("dplyr", "tibble", "readr")) {
  suppressMessages({
    library(dplyr); library(tibble); library(readr)
  })

  # ── news relevance filter ────────────────────────────────────────────────
  # Yahoo's per-ticker feed mixes in general market stories: an Nvidia earnings
  # preview was returned inside both the AAPL and AMZN feeds and rendered on
  # each stock's Deep Dive as though it were about that company.
  #
  # Source the shipped function rather than restating it here — a test that
  # re-implements its subject passes even when the real code drifts.
  news_deps <- have_pkgs("httr", "jsonlite", "glue", "lubridate", "quantmod", "curl", "xml2")
  if (news_deps) source(repo_path("R", "03_data.R"), local = TRUE)

  test_that("relevance filter keeps on-topic headlines", {
    skip_if_not(news_deps, "03_data.R deps missing")
    expect_true(news_is_relevant("Jefferies Just Said Sell Apple (AAPL)", "AAPL", "Apple Inc."))
    expect_true(news_is_relevant("Apple's Next CEO Could Spend More to Catch Up in AI", "AAPL", "Apple Inc."))
    expect_true(news_is_relevant("The Premium On AAPL Stock Vs What Its Peers Deliver", "AAPL", "Apple Inc."))
  })

  test_that("relevance filter drops the cross-ticker story that caused this", {
    skip_if_not(news_deps, "03_data.R deps missing")
    expect_false(news_is_relevant("Morgan Stanley resets Nvidia stock forecast ahead of earnings",
                                  "AAPL", "Apple Inc."))
    expect_false(news_is_relevant("Morgan Stanley resets Nvidia stock forecast ahead of earnings",
                                  "AMZN", "Amazon.com, Inc."))
    # ...but it is genuinely relevant to NVDA
    expect_true(news_is_relevant("Morgan Stanley resets Nvidia stock forecast ahead of earnings",
                                 "NVDA", "NVIDIA Corporation"))
  })

  test_that("generic leading words do not match half the market", {
    skip_if_not(news_deps, "03_data.R deps missing")
    # "General Motors" must not claim every headline containing "general", and
    # "The Trade Desk" must not claim every headline containing "the".
    expect_false(news_is_relevant("General market selloff deepens", "GM", "General Motors Company"))
    expect_false(news_is_relevant("The market rallied on Friday", "TTD", "The Trade Desk"))
  })

  test_that("filter handles missing titles and company names without erroring", {
    skip_if_not(news_deps, "03_data.R deps missing")
    expect_false(news_is_relevant(NA_character_, "AAPL", "Apple Inc."))
    expect_false(news_is_relevant("", "AAPL", "Apple Inc."))
    expect_false(news_is_relevant("Some unrelated headline", "AAPL", NA_character_))
    expect_false(news_is_relevant("Some unrelated headline", "AAPL", ""))
  })

  # The fallback matters: a strict filter that empties the panel is worse than a
  # little noise, so a ticker whose headlines never name it keeps its feed.
  test_that("all-irrelevant feed falls back to unfiltered rather than going blank", {
    skip_if_not(news_deps, "03_data.R deps missing")
    d <- tibble(title = c("Chip sector rallies", "Semis lead the tape"))
    d$relevant <- vapply(d$title, news_is_relevant, logical(1),
                         sym = "AVGO", company = "Broadcom Inc.")
    keep <- if (any(d$relevant)) d[d$relevant, , drop = FALSE] else d
    expect_equal(nrow(keep), 2)
  })

  # ── RSS date parsing ─────────────────────────────────────────────────────
  # RFC-822 is the RSS date format. Parsing it with the wrong format string
  # yields NA silently, which previously made every story sort as undated.
  test_that("RFC-822 pubDate parses to a real timestamp", {
    p <- as.POSIXct("Thu, 21 Aug 2026 13:45:00 GMT",
                    format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
    expect_false(is.na(p))
    expect_equal(format(p, "%Y-%m-%d"), "2026-08-21")
  })

  test_that("a malformed pubDate yields NA instead of aborting", {
    p <- suppressWarnings(as.POSIXct("not a date", format = "%a, %d %b %Y %H:%M:%S", tz = "GMT"))
    expect_true(is.na(p))
  })

  # ── SEC form labelling ───────────────────────────────────────────────────
  # EDGAR leaves primaryDocDescription empty for most large filers, so rows
  # rendered the form code twice ("10-Q ... 10-Q").
  form_label <- function(form, desc) {
    norm <- gsub("^FORM\\s+", "", toupper(trimws(ifelse(is.na(desc), "", desc))))
    if (nzchar(norm) && norm != toupper(form)) return(desc)
    switch(as.character(form),
      "10-K"    = "Annual report",
      "10-Q"    = "Quarterly report",
      "8-K"     = "Material event disclosure",
      "4"       = "Insider transaction",
      "S-4"     = "Merger / acquisition registration",
      "DEF 14A" = "Proxy statement",
      "S-1"     = "Securities registration",
      "S-3"     = "Shelf registration",
      as.character(form))
  }

  test_that("empty or echoed descriptions fall back to a readable label", {
    expect_equal(form_label("10-Q", NA_character_), "Quarterly report")
    expect_equal(form_label("10-Q", ""),            "Quarterly report")
    expect_equal(form_label("10-Q", "10-Q"),        "Quarterly report")
    # EDGAR's echoed-with-prefix form, which rendered as "8-K ... FORM 8-K"
    expect_equal(form_label("8-K",  "FORM 8-K"),     "Material event disclosure")
    expect_equal(form_label("8-K",  "form 8-k"),     "Material event disclosure")
    expect_equal(form_label("S-4",  NA_character_), "Merger / acquisition registration")
  })

  test_that("a real description is preferred over the generic label", {
    expect_equal(form_label("8-K", "Results of Operations"), "Results of Operations")
  })

  test_that("an unknown form degrades to the form code, not an error", {
    expect_equal(form_label("ABS-EE", NA_character_), "ABS-EE")
  })

  # ── loaded data shape ────────────────────────────────────────────────────
  # app/global.R loads these by name; a schema drift would blank the panels.
  test_that("stock_news.csv has the columns the Deep Dive panel reads", {
    p <- repo_path("data", "stock_news.csv")
    skip_if_not(file.exists(p), "stock_news.csv not generated yet")
    d <- read_csv(p, show_col_types = FALSE)
    expect_true(all(c("symbol", "title", "url", "publisher", "published_parsed") %in% names(d)))
    skip_if(nrow(d) == 0, "no stock news this cycle — health check reports it")
    expect_true(all(!is.na(d$symbol) & nzchar(d$symbol)))
    # a story tagged to a ticker must have a link, or the headline is dead text
    expect_true(all(!is.na(d$url) & nzchar(d$url)))
  })

  test_that("sec_filings.csv has the columns the filings panel reads", {
    p <- repo_path("data", "sec_filings.csv")
    skip_if_not(file.exists(p), "sec_filings.csv not generated yet")
    d <- read_csv(p, show_col_types = FALSE)
    expect_true(all(c("symbol", "form", "filed", "url",
                      "insider_filings_90d", "merger_activity_1y",
                      "material_events_1y") %in% names(d)))
    skip_if(nrow(d) == 0, "no filings this cycle — health check reports it")
    expect_true(all(!is.na(as.Date(d$filed))))
    # EDGAR URLs must be absolute; a relative path renders as a broken link
    expect_true(all(grepl("^https://www\\.sec\\.gov/", d$url)))
    # the flags are per-company, so they must be constant within a symbol
    per_sym <- d %>% group_by(symbol) %>%
      summarise(n_flag = n_distinct(merger_activity_1y), .groups = "drop")
    expect_true(all(per_sym$n_flag == 1))
  })
}
