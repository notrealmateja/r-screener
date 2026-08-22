# Discounted cash flow.
#
# compute_dcf() was called by the Deep Dive panel but never defined anywhere in
# the repo, so its tryCatch swallowed "could not find function" on every render
# and the panel showed a permanent N/A beside a hardcoded "WACC Used 10.0%".
# Its inputs were empty too: fcf, debt_equity and gross_margin were NA for all
# 195 names because Polygon's financials endpoint returned nothing.

if (have_pkgs("dplyr", "tibble", "readr")) {
  suppressMessages({ library(dplyr); library(tibble); library(readr) })

  # Load the shipped implementation rather than restating it. global.R's data
  # loaders run against the app directory, so give it a macro_data to read and
  # source only the DCF section.
  macro_data <- tibble(date = as.Date("2026-08-20"), series = "10Y Treasury",
                       ticker = "DGS10", value = 4.5, price = 4.5)
  dcf_src <- readLines(repo_path("app", "global.R"))
  dcf_start <- grep("^DCF_ERP", dcf_src)[1]
  if (!is.na(dcf_start)) {
    eval(parse(text = paste(dcf_src[dcf_start:length(dcf_src)], collapse = "\n")))
  }
  have_dcf <- exists("compute_dcf") && is.function(compute_dcf)

  # A healthy, ordinary company: $1B FCF, 1B shares, modest growth.
  base_stock <- function(...) {
    d <- tibble(symbol = "TEST", fcf = 1e9, shares_outstanding = 1e9,
                close = 20, beta = 1.0, market_cap = 20e9,
                total_debt = 2e9, cash = 1e9, rev_cagr = 0.06,
                revenue_growth = 0.06)
    mods <- list(...)
    for (n in names(mods)) d[[n]] <- mods[[n]]
    d
  }

  test_that("compute_dcf exists at all", {
    # The original defect: the function was simply absent.
    expect_true(have_dcf)
  })

  test_that("a healthy company produces a finite per-share value", {
    skip_if_not(have_dcf, "compute_dcf missing")
    d <- compute_dcf(base_stock())
    expect_true(is.finite(d$intrinsic_value))
    expect_gt(d$intrinsic_value, 0)
    expect_true(is.finite(d$upside))
    expect_false(d$dcf_rating == "N/A")
  })

  test_that("the reported WACC is the one actually used, not a constant", {
    skip_if_not(have_dcf, "compute_dcf missing")
    # The panel used to print a hardcoded "10.0%" regardless of the company.
    low  <- compute_dcf(base_stock(beta = 0.6))
    high <- compute_dcf(base_stock(beta = 2.2))
    expect_true(is.finite(low$wacc) && is.finite(high$wacc))
    expect_gt(high$wacc, low$wacc)
    # A riskier company must be worth less, all else equal.
    expect_lt(high$intrinsic_value, low$intrinsic_value)
  })

  test_that("the risk-free rate comes from the 10Y Treasury series", {
    skip_if_not(have_dcf, "compute_dcf missing")
    expect_equal(dcf_risk_free(), 0.045)
    d <- compute_dcf(base_stock())
    expect_equal(d$rf, 0.045)
  })

  test_that("missing inputs yield N/A rather than a fabricated number", {
    skip_if_not(have_dcf, "compute_dcf missing")
    for (bad in list(list(fcf = NA_real_), list(shares_outstanding = NA_real_),
                     list(close = NA_real_), list(shares_outstanding = 0),
                     list(close = 0))) {
      d <- do.call(base_stock, bad) %>% compute_dcf()
      expect_true(is.na(d$intrinsic_value))
      expect_equal(d$dcf_rating, "N/A")
    }
  })

  test_that("negative free cash flow is not valued", {
    skip_if_not(have_dcf, "compute_dcf missing")
    # A DCF on negative FCF has no meaningful terminal value; SOUN and the other
    # cash-burning names must show N/A instead of a negative price target.
    d <- compute_dcf(base_stock(fcf = -5e8))
    expect_true(is.na(d$intrinsic_value))
    expect_equal(d$dcf_rating, "N/A")
  })

  test_that("terminal growth always stays below the discount rate", {
    skip_if_not(have_dcf, "compute_dcf missing")
    # If terminal growth ever reached WACC the Gordon denominator goes to zero
    # and the valuation explodes to infinity.
    for (b in c(0.5, 1, 1.5, 2.5)) {
      d <- compute_dcf(base_stock(beta = b))
      expect_lt(d$terminal_growth, d$wacc)
      expect_true(is.finite(d$intrinsic_value))
    }
  })

  test_that("extreme trailing growth cannot run away with the valuation", {
    skip_if_not(have_dcf, "compute_dcf missing")
    # revenue_growth reached 4672% across the universe when it was aliased to
    # quarterly EPS growth. Growth must be capped before it compounds.
    wild <- compute_dcf(base_stock(rev_cagr = 46.7))
    sane <- compute_dcf(base_stock(rev_cagr = 0.20))
    expect_true(is.finite(wild$intrinsic_value))
    expect_equal(wild$growth, sane$growth)
    expect_lte(wild$growth, 0.20)
  })

  test_that("growth falls back through rev_cagr then revenue_growth", {
    skip_if_not(have_dcf, "compute_dcf missing")
    d <- compute_dcf(base_stock(rev_cagr = NA_real_, revenue_growth = 0.11))
    expect_equal(d$growth, 0.11)
    # with neither, it still returns a value rather than erroring
    d2 <- compute_dcf(base_stock(rev_cagr = NA_real_, revenue_growth = NA_real_))
    expect_true(is.finite(d2$intrinsic_value))
  })

  test_that("net cash raises equity value and net debt lowers it", {
    skip_if_not(have_dcf, "compute_dcf missing")
    rich <- compute_dcf(base_stock(total_debt = 0,    cash = 5e9))
    poor <- compute_dcf(base_stock(total_debt = 10e9, cash = 0))
    expect_gt(rich$intrinsic_value, poor$intrinsic_value)
  })

  test_that("rating bands follow upside and carry no recommendation", {
    skip_if_not(have_dcf, "compute_dcf missing")
    cheap <- compute_dcf(base_stock(close = 2))    # price far below value
    dear  <- compute_dcf(base_stock(close = 500))  # price far above value
    expect_equal(cheap$dcf_rating, "Materially above price")
    expect_equal(dear$dcf_rating,  "Materially below price")
    # The app deliberately avoids buy/sell language.
    all_ratings <- c(cheap$dcf_rating, dear$dcf_rating)
    expect_false(any(grepl("buy|sell", all_ratings, ignore.case = TRUE)))
  })

  test_that("a missing macro table falls back to a fixed risk-free rate", {
    skip_if_not(have_dcf, "compute_dcf missing")
    saved <- macro_data
    macro_data <<- NULL
    on.exit(macro_data <<- saved, add = TRUE)
    expect_equal(dcf_risk_free(), 0.04)
  })

  # ── the inputs themselves ────────────────────────────────────────────────
  test_that("SEC financials supply the DCF inputs that were empty", {
    p <- repo_path("data", "sec_financials.csv")
    skip_if_not(file.exists(p), "sec_financials.csv not generated yet")
    d <- read_csv(p, show_col_types = FALSE)
    expect_true(all(c("symbol", "fcf_sec", "debt_equity_sec", "rev_cagr_sec",
                      "total_debt_sec", "cash_sec") %in% names(d)))
    # These were 0/195 before; require real coverage, not merely a column.
    expect_gt(sum(!is.na(d$fcf_sec)), 100)
    expect_gt(sum(!is.na(d$rev_cagr_sec)), 100)
  })

  test_that("normalised FCF differs from a single year for capex-heavy names", {
    p <- repo_path("data", "sec_financials.csv")
    skip_if_not(file.exists(p), "sec_financials.csv not generated yet")
    d <- read_csv(p, show_col_types = FALSE)
    skip_if_not("fcf_latest_sec" %in% names(d), "single-year column absent")
    both <- d %>% filter(!is.na(fcf_sec), !is.na(fcf_latest_sec), fcf_years_sec > 1)
    skip_if(nrow(both) == 0, "no multi-year rows")
    # Averaging must actually change something, or it is not smoothing anything.
    expect_gt(sum(abs(both$fcf_sec - both$fcf_latest_sec) > 1), 0)
  })

  test_that("master_scored carries the joined DCF inputs", {
    p <- repo_path("data", "master_scored.csv")
    skip_if_not(file.exists(p), "master_scored.csv not generated yet")
    m <- read_csv(p, show_col_types = FALSE)
    for (col in c("fcf", "total_debt", "cash", "rev_cagr",
                  "beta", "shares_outstanding")) {
      expect_true(col %in% names(m))
    }
    expect_gt(sum(!is.na(m$fcf)), 100)
  })

  test_that("revenue_growth is no longer a copy of earningsGrowth", {
    p <- repo_path("data", "master_scored.csv")
    skip_if_not(file.exists(p), "master_scored.csv not generated yet")
    m <- read_csv(p, show_col_types = FALSE)
    skip_if_not(all(c("revenue_growth", "earningsGrowth") %in% names(m)))
    both <- m %>% filter(!is.na(revenue_growth), !is.na(earningsGrowth))
    skip_if(nrow(both) == 0, "no comparable rows")
    # They were identical for all 195 rows.
    expect_gt(sum(both$revenue_growth != both$earningsGrowth), 0)
  })
}
