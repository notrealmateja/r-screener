# Can the valuation ratios survive the vendor going dark?
#
# Alpha Vantage paywalled OVERVIEW on 2026-10-06. pe_ratio, pb_ratio, roe and
# profit_margin froze at their 2026-10-05 values and stayed frozen for 30 days
# -- 75% of quality_gate's weight, which is 15% of the ranking -- while the page
# printed them as current. Nothing in the pipeline measured whether they moved,
# and the only visible symptom was a user noticing the page looked the same.
#
# These are now rebuilt from SEC XBRL. The tests that matter are not "did we
# fetch something" but "is what we fetched self-consistent", because the frames
# API answers net income and EPS from separate requests with nothing tying them
# to the same company-year.

data_src <- repo_path("R", "03_data.R")
ms_src   <- repo_path("R", "04_master_score.R")
glb_src  <- repo_path("app", "global.R")
decom <- function(f) paste(grep("^\\s*#", readLines(f, warn = FALSE),
                                invert = TRUE, value = TRUE), collapse = "\n")

# ── The fetch ───────────────────────────────────────────────────────────────

test_that("the frames request can reach units other than USD", {
  skip_if_not(file.exists(data_src), "R/03_data.R not present")
  txt <- decom(data_src)
  # The unit was baked into the path, so EPS (USD-per-shares) and share counts
  # (shares) could not be requested at all.
  expect_false(grepl("SEC_FRAMES\\}/\\{concept\\}/USD/", txt),
               info = "the frames URL has gone back to a hardcoded USD unit")
  expect_match(txt, "sec_frame <- function\\(concept, frame, unit = \"USD\"\\)")
  expect_match(txt, "sec_frame_merge <- function\\(concept, frames, unit = \"USD\"\\)")
  expect_match(txt, 'sec_frame\\(concept, f, unit\\)')
})

test_that("the earnings concepts the ratios need are actually requested", {
  skip_if_not(file.exists(data_src), "R/03_data.R not present")
  txt <- decom(data_src)
  expect_match(txt, '"NetIncomeLoss"')
  expect_match(txt, '"EarningsPerShareDiluted", af, "USD-per-shares"')
  # Basic EPS as a fallback: not every filer reports diluted.
  expect_match(txt, '"EarningsPerShareBasic"')
  for (col in c("net_income_sec", "eps_sec", "shares_sec", "roe_sec", "net_margin_sec"))
    expect_match(txt, paste0(col, "\\s*="), info = col)
})

test_that("net income and EPS are cross-checked before anything is derived", {
  skip_if_not(file.exists(data_src), "R/03_data.R not present")
  txt <- decom(data_src)
  # The quotient is the share count the pair implies. Both live failures are
  # caught by it: ANET answered $3,511 against EPS 2.75 (1,277 shares implied),
  # KYMR +$311M against EPS -3.69 (a negative count).
  expect_match(txt, "implied_sh <- ifelse")
  expect_match(txt, "ni_ok <- !is\\.na\\(implied_sh\\) & implied_sh >= 1e6 & implied_sh <= 1e11")
  # Every derived figure must be gated on it, not just some.
  for (col in c("net_income_sec", "eps_sec", "shares_sec", "roe_sec", "net_margin_sec")) {
    line <- grep(paste0("^\\s*", col, "\\s*="), readLines(data_src, warn = FALSE), value = TRUE)
    expect_length(line, 1L)
    expect_match(line, "ni_ok", info = paste(col, "is not gated on the cross-check"))
  }
})

test_that("the cross-check accepts real filers and rejects the broken ones", {
  # Executed, not grepped: the guard is one expression and its whole value is
  # what it does to actual numbers.
  ok <- function(ni, eps) {
    sh <- if (is.na(ni) || is.na(eps) || eps == 0) NA_real_ else ni / eps
    !is.na(sh) && sh >= 1e6 && sh <= 1e11
  }
  # Real filings, taken from the live universe on 2026-11-04.
  expect_true(ok(1.12010e11, 7.46))    # AAPL
  expect_true(ok(1.01832e11, 13.64))   # MSFT
  expect_true(ok(1.8254e10,  7.28))    # MRK
  expect_true(ok(5.2186e7,   0.15))    # AFRM, thin but real
  expect_true(ok(-8.12e7,   -1.50))    # CROX, a loss year: both terms negative
  expect_true(ok(-3.56148e8,-5.84))    # JAZZ, likewise

  expect_false(ok(3511, 2.75))         # ANET: 1,277 shares implied
  expect_false(ok(3.11351e8, -3.69))   # KYMR: profit against a per-share loss
  expect_false(ok(1e5, 0))             # EPS of zero
  expect_false(ok(NA, 2.0))
  expect_false(ok(1e9, NA))
  expect_false(ok(1e14, 0.0001))       # absurd implied count
})

test_that("the printed ratios are bounded as well as cross-checked", {
  skip_if_not(file.exists(data_src), "R/03_data.R not present")
  txt <- decom(data_src)
  # Neither bound changes a score -- the roe band tops out above 0.30 and the
  # margin band above 0.25 -- but a 794% net margin on the page is a visible
  # lie even when it ranks the same.
  expect_match(txt, "abs\\(ni_v / eq_v\\) <= 2\\.5")
  expect_match(txt, "abs\\(ni_v / rev_v\\) <= 1")
})

# ── The merge ───────────────────────────────────────────────────────────────

test_that("the master score prefers filings over the frozen vendor fields", {
  skip_if_not(file.exists(ms_src), "R/04_master_score.R not present")
  txt <- decom(ms_src)
  for (col in c("net_income_sec", "eps_sec", "shares_sec", "roe_sec", "net_margin_sec"))
    expect_match(txt, col, info = paste(col, "is not carried through the join"))
  # Precedence matches the fcf/debt_equity merge already in this file.
  expect_match(txt, "pe_ratio\\s*= dplyr::coalesce\\(pe_sec,\\s*pe_ratio\\)")
  expect_match(txt, "pb_ratio\\s*= dplyr::coalesce\\(pb_sec,\\s*pb_ratio\\)")
  expect_match(txt, "roe\\s*= dplyr::coalesce\\(roe_sec,\\s*roe\\)")
  expect_match(txt, "profit_margin = dplyr::coalesce\\(net_margin_sec, profit_margin\\)")
  # A loss-making filer has no meaningful P/E and negative book is not a high
  # multiple: both are left missing, which q_pe/q_pb already score as neutral.
  expect_match(txt, "eps_sec > 0")
  expect_match(txt, "equity_sec > 0")
})

test_that("the rebuild happens before the ratios are scored", {
  skip_if_not(file.exists(ms_src), "R/04_master_score.R not present")
  lines <- readLines(ms_src, warn = FALSE)
  rebuild <- grep("pe_ratio\\s*= dplyr::coalesce\\(pe_sec", lines)
  gate    <- grep("quality_gate = round\\(", lines)
  expect_length(rebuild, 1L)
  expect_true(length(gate) >= 1)
  # Scoring the vendor value and then replacing it would leave quality_gate
  # built on the frozen number while the page printed the fresh one.
  expect_lt(rebuild[1], gate[1])
})

test_that("the page can say where its fundamentals came from", {
  skip_if_not(file.exists(ms_src), "R/04_master_score.R not present")
  txt <- decom(ms_src)
  # Substituting a source silently is how this went unnoticed for 30 days.
  expect_match(txt, "fundamentals_from_sec")
  expect_match(txt, "fundamentals_source")
})

test_that("the provenance line states staleness and never errors", {
  skip_if_not(file.exists(glb_src), "app/global.R not present")
  src <- readLines(glb_src, warn = FALSE)
  a <- grep("^fundamentals_provenance <- function", src)
  b <- grep("^bt_leverage_share <- function", src)
  skip_if(!length(a) || !length(b), "helper not found")
  e <- new.env(parent = globalenv())
  eval(parse(text = paste(src[a[1]:(b[1] - 1)], collapse = "\n")), envir = e)
  f <- e$fundamentals_provenance

  base <- data.frame(symbol = paste0("S", 1:10), stringsAsFactors = FALSE)

  # Vendor-only, a month stale: the state the site was actually in, and the
  # one that must be said out loud.
  d <- base; d$last_fetched <- "2026-10-05"
  out <- f(d)
  expect_match(out, "VENDOR DATA ONLY")
  expect_match(out, "2026-10-05")
  expect_match(out, "DAYS AGO")

  # Filings supplying all four.
  d2 <- base; d2$fundamentals_from_sec <- 4
  d2$last_fetched <- format(Sys.Date())
  out2 <- f(d2)
  expect_match(out2, "SEC FILINGS FOR 10 OF 10")
  expect_false(grepl("DAYS AGO", out2))

  # A partial mix must not claim the full set.
  d3 <- base; d3$fundamentals_from_sec <- c(rep(4, 3), rep(1, 7))
  d3$last_fetched <- format(Sys.Date())
  expect_match(f(d3), "SEC FILINGS FOR 3 OF 10")

  # Absence reads as unknown, never as fresh, and never takes the panel down.
  for (bad in list(NULL, base[0, , drop = FALSE], base,
                   transform(base, last_fetched = NA_character_),
                   transform(base, last_fetched = "not a date"))) {
    v <- expect_no_error(f(bad))
    if (!is.null(v)) expect_true(grepl("UNKNOWN|VENDOR|SEC", v))
  }
})
