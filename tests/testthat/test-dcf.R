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

  test_that("growth never falls back to the quarterly-EPS proxy", {
    skip_if_not(have_dcf, "compute_dcf missing")
    # revenue_growth is still the earningsGrowth proxy for the ~18 names with no
    # SEC revenue CAGR. Falling back to it produced a 448% "upside" on Seneca
    # Foods off a 533%-style growth reading. Absent a filing-derived CAGR the
    # model must assume a flat conservative rate instead.
    d <- compute_dcf(base_stock(rev_cagr = NA_real_, revenue_growth = 5.33))
    expect_equal(d$growth, DCF_DEFAULT_G)
    expect_true(isTRUE(d$growth_assumed))
    expect_equal(d$basis, "assumed growth")

    # and a real SEC CAGR is used as-is, flagged as observed
    d2 <- compute_dcf(base_stock(rev_cagr = 0.08))
    expect_equal(d2$growth, 0.08)
    expect_false(isTRUE(d2$growth_assumed))
    expect_equal(d2$basis, "SEC XBRL")

    # with neither, it still returns a value rather than erroring
    d3 <- compute_dcf(base_stock(rev_cagr = NA_real_, revenue_growth = NA_real_))
    expect_true(is.finite(d3$intrinsic_value))
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

# ── LBO return sensitivity ──────────────────────────────────────────────────
if (have_pkgs("dplyr", "tibble", "glue")) {
  suppressMessages(library(glue))

  lbo_stock <- function(...) {
    d <- tibble(symbol = "TEST", company = "Test Co", sector = "TECHNOLOGY",
                ebitda = 500e6, market_cap = 5e9, total_debt = 1e9, cash = 200e6,
                fcf = 300e6, shares_outstanding = 100e6, close = 50, beta = 1.1,
                rev_cagr = 0.07, pe_ratio = 18, ev_ebitda = 11,
                master_score = 62, master_percentile = 71, primary_driver = "High IR",
                ret_3m = 0.08, ret_1y = 0.22, rsi_zone = "Neutral",
                short_percent_float = 0.03, squeeze_tier = "No Signal")
    mods <- list(...)
    for (n in names(mods)) d[[n]] <- mods[[n]]
    d
  }
  have_lbo <- exists("compute_lbo") && is.function(compute_lbo)

  test_that("a normal company produces a complete IRR grid", {
    skip_if_not(have_lbo, "compute_lbo missing")
    l <- compute_lbo(lbo_stock())
    expect_false(is.null(l))
    expect_gt(l$entry_mult, 0)
    expect_equal(length(l$grid), length(l$exit_mults))
    expect_true(all(vapply(l$grid, length, integer(1)) == length(l$growth)))
  })

  test_that("a higher exit multiple returns more, holding growth fixed", {
    skip_if_not(have_lbo, "compute_lbo missing")
    l <- compute_lbo(lbo_stock())
    firsts <- vapply(l$grid, function(r) r[1], numeric(1))
    ok <- firsts[!is.na(firsts)]
    skip_if(length(ok) < 2, "not enough finite cells")
    expect_true(all(diff(ok) > 0))
  })

  test_that("faster EBITDA growth returns more, holding exit fixed", {
    skip_if_not(have_lbo, "compute_lbo missing")
    l <- compute_lbo(lbo_stock())
    row <- l$grid[[1]]
    ok <- row[!is.na(row)]
    skip_if(length(ok) < 2, "not enough finite cells")
    expect_true(all(diff(ok) > 0))
  })

  test_that("a company with no sponsor equity left is refused", {
    skip_if_not(have_lbo, "compute_lbo missing")
    # Entry debt is 5x EBITDA. If enterprise value is below that, the structure
    # implies a negative equity cheque and the IRR is meaningless.
    expect_null(compute_lbo(lbo_stock(market_cap = 1e8, total_debt = 0, cash = 0)))
  })

  test_that("missing or non-positive EBITDA yields no LBO", {
    skip_if_not(have_lbo, "compute_lbo missing")
    expect_null(compute_lbo(lbo_stock(ebitda = NA_real_)))
    expect_null(compute_lbo(lbo_stock(ebitda = -1e8)))
    expect_null(compute_lbo(lbo_stock(market_cap = NA_real_)))
  })

  # ── generated brief ───────────────────────────────────────────────────────
  have_pitch <- exists("pitch_bullets") && is.function(pitch_bullets)

  test_that("the brief argues both directions", {
    skip_if_not(have_pitch, "pitch_bullets missing")
    p <- pitch_bullets(lbo_stock())
    expect_true(length(p$supports) > 0)
    expect_true(length(p$against) > 0)
  })

  test_that("the brief never issues a recommendation", {
    skip_if_not(have_pitch, "pitch_bullets missing")
    # The app deliberately dropped buy/sell labels; the brief must not smuggle
    # them back in as prose.
    p <- pitch_bullets(lbo_stock())
    txt <- paste(c(p$supports, p$against), collapse = " ")
    expect_false(grepl("\\b(buy|sell|should own|price target|we recommend)\\b",
                       txt, ignore.case = TRUE))
  })

  test_that("percentile ordinals read correctly", {
    skip_if_not(have_pitch, "pitch_bullets missing")
    # "43th percentile" was the original output.
    for (pctl in c(1, 2, 3, 11, 12, 13, 21, 22, 23, 43, 82, 100)) {
      txt <- paste(unlist(pitch_bullets(lbo_stock(master_percentile = pctl))), collapse = " ")
      expect_false(grepl("\\d+th percentile", txt) &&
                   grepl(paste0("\\b", pctl, "th"), txt) &&
                   pctl %% 10 %in% c(1, 2, 3) && !(pctl %% 100 %in% 11:13))
    }
    expect_match(paste(unlist(pitch_bullets(lbo_stock(master_percentile = 43))), collapse=" "),
                 "43rd percentile")
    expect_match(paste(unlist(pitch_bullets(lbo_stock(master_percentile = 82))), collapse=" "),
                 "82nd percentile")
    expect_match(paste(unlist(pitch_bullets(lbo_stock(master_percentile = 11))), collapse=" "),
                 "11th percentile")
  })

  test_that("a name with no valuation says so rather than staying silent", {
    skip_if_not(have_pitch, "pitch_bullets missing")
    p <- pitch_bullets(lbo_stock(fcf = -1e8))
    expect_true(any(grepl("No discounted cash flow", p$against)))
  })

  test_that("the brief survives entirely missing optional inputs", {
    skip_if_not(have_pitch, "pitch_bullets missing")
    bare <- tibble(symbol = "X", company = "X Co", sector = "TECHNOLOGY",
                   close = 10, master_score = 50, master_percentile = 50)
    expect_error(pitch_bullets(bare, NULL, NA, NULL), NA)
  })
}
