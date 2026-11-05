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
  # Extraction starts at vendor_age_stats, not at fundamentals_provenance:
  # on 2026-11-05 the age arithmetic moved into its own helpers above it, and
  # an anchor on the caller alone pulled in a function whose dependencies had
  # been left behind. The test errored rather than failing, which is the same
  # colour as a real break but tells you much less.
  a <- grep("^vendor_age_stats <- function", src)
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
  expect_match(out, "DAYS OLD")

  # Filings supplying all four.
  d2 <- base; d2$fundamentals_from_sec <- 4
  d2$last_fetched <- format(Sys.Date())
  out2 <- f(d2)
  expect_match(out2, "SEC FILINGS FOR 10 OF 10")
  expect_false(grepl("DAYS OLD", out2))

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

test_that("the vendor age is reported across rows, not at its freshest one", {
  skip_if_not(file.exists(glb_src), "app/global.R not present")
  src <- readLines(glb_src, warn = FALSE)
  a <- grep("^vendor_age_stats <- function", src)
  b <- grep("^bt_leverage_share <- function", src)
  skip_if(!length(a) || !length(b), "helper not found")
  e <- new.env(parent = globalenv())
  eval(parse(text = paste(src[a[1]:(b[1] - 1)], collapse = "\n")), envir = e)
  stats <- e$vendor_age_stats
  note  <- e$vendor_stale_note
  today <- as.Date("2026-11-05")

  # The real shape on 2026-11-05: av_cache was filled in batches over several
  # weeks, so last_fetched spans 2026-08-29..2026-10-06. Reporting max() named
  # the single freshest row -- 30 days -- and called it the age of the feed,
  # on a label whose only job is to stop the reader being flattered.
  d <- data.frame(last_fetched = c("2026-08-29", "2026-09-26", "2026-09-30",
                                   "2026-10-02", "2026-10-06"))
  v <- stats(d, today)
  expect_equal(v$newest, 30)
  expect_equal(v$oldest, 68)
  expect_equal(v$median, 36)
  expect_equal(v$n, 5)

  n <- note(d, today)
  expect_match(n, "30")
  expect_match(n, "68")
  # The flattering number must not stand alone. If the note ever says only the
  # newest age again, this fails.
  expect_false(identical(n, "VENDOR FIELDS 30 DAYS OLD"))

  # One age for every row needs no range.
  same <- data.frame(last_fetched = rep("2026-09-26", 4))
  expect_match(note(same, today), "^VENDOR FIELDS 40 DAYS OLD$")

  # A current feed carries NO marker, so the marker keeps its meaning.
  fresh <- data.frame(last_fetched = format(today - 0:3))
  expect_null(note(fresh, today))
  # And the boundary is the stated constant, not a number picked here.
  expect_match(paste(src, collapse = "\n"), "VENDOR_STALE_DAYS <- 7")
  expect_null(note(data.frame(last_fetched = format(today - 7)), today))
  expect_false(is.null(note(data.frame(last_fetched = format(today - 8)), today)))

  # Unreadable is unknown, never fresh, and never an error: this renders
  # inside panel headers, so a throw takes the whole table down.
  expect_null(stats(data.frame(last_fetched = c("n/a", "")), today))
  expect_no_error(note(data.frame(last_fetched = "not a date"), today))
  expect_match(note(data.frame(last_fetched = "not a date"), today), "AGE UNKNOWN")
  expect_null(stats(NULL, today))
  expect_match(note(NULL, today), "AGE UNKNOWN")
})

test_that("every panel showing a frozen vendor field carries the marker", {
  app_src <- repo_path("app", "app.R")
  skip_if_not(file.exists(app_src), "app.R not present")
  src <- paste(readLines(app_src, warn = FALSE), collapse = "\n")

  # These four columns are byte-identical passthroughs of av_cache, which
  # stopped refreshing on 2026-10-06. Each surface that prints one says so.
  # Verified against the data: market_cap, earningsGrowth, operating_margin
  # and ev_ebitda are identical in av_cache.csv and master_scored.csv for all
  # 195 symbols, while pe_ratio, pb_ratio, roe and profit_margin are not --
  # those are re-sourced from filings and are deliberately NOT marked stale.
  expect_match(src, 'uiOutput\\("unicorn_badge"')          # Unicorns panel head
  expect_match(src, 'output\\$unicorn_badge <- renderUI')
  expect_match(src, "vendor_note_div\\(master_data,\\n\\s*\"MARKET CAP, EV/EBITDA, OP\\. MARGIN, EPS GROWTH\"\\)")
  expect_match(src, "Earnings Growth \\(%\\)\",\\n\\s*if \\(!is\\.null\\(v\\) && v\\$oldest > VENDOR_STALE_DAYS\\)")
  expect_match(src, "eg_vintage = local\\(\\{")
  expect_match(src, "eg_vintage\\),")                       # and it is RENDERED

  # The Unicorn criteria line is built, not a constant: a constant cannot age,
  # and that line asserts a market-cap threshold read off the frozen column.
  expect_false(grepl('div(class="panel-head-meta","MARKET CAP < $5B', src, fixed = TRUE))

  # The valuation panels, found by a later sweep. These matter more than the
  # tables above: each turns a frozen column into a single headline number.
  expect_match(src, 'vendor_note_div\\(master_data, "SHARES OUTSTANDING, MARKET CAP"\\)')
  expect_match(src, 'vendor_note_div\\(master_data, "MARKET CAP, EBITDA"\\)')
  expect_match(src, 'vendor_note_div\\(master_data, "EV/EBITDA, AND THE DCF AND LBO FIGURES QUOTED ABOVE"\\)')
  expect_match(src, 'output\\$dd_comps_badge <- renderUI')
  expect_match(src, 'uiOutput\\("dd_comps_badge"')
  expect_match(src, "cap: vendor, %\\.0f")          # the Deep Dive header line
})

test_that("no panel claims a provenance it does not have", {
  app_src <- repo_path("app", "app.R")
  skip_if_not(file.exists(app_src), "app.R not present")
  src <- paste(readLines(app_src, warn = FALSE), collapse = "\n")

  # The DCF footnote named the three filing-sourced inputs and stopped there,
  # which read as a provenance statement for the whole model. The headline is
  # equity value DIVIDED BY shares_outstanding -- a frozen vendor column -- and
  # the upside beside it compares that against a live price. Naming only the
  # fresh half of a calculation is worse than naming nothing.
  expect_match(src, "come from SEC XBRL filings, and beta is")
  expect_match(src, "come from the vendor cache instead")
  expect_false(grepl("cash flow, debt and cash come from SEC XBRL filings. A DCF is",
                     src, fixed = TRUE))

  # The research brief said it was assembled from "filings, prices and
  # sentiment" -- three live sources -- while quoting frozen multiples in prose.
  expect_match(src, "filings, prices, vendor fundamentals and")
  expect_false(grepl("same filings, prices and sentiment shown on this",
                     src, fixed = TRUE))

  # The Data sources table advertised a refresh cadence that stopped on
  # 2026-10-06. A provenance table that is itself out of date is the worst
  # place in the app for a stale claim.
  expect_false(grepl("22 tickers/run — free tier is 25 calls/day", src, fixed = TRUE))
  expect_match(src, "<b>Stopped 2026-10-06</b>")
  expect_equal(length(gregexpr("<b>Stopped 2026-10-06</b>", src)[[1]]), 2L)

  # And the indirect leg is disclosed once, in Methodology, rather than badged
  # onto a dozen panels that each carry the same 10% component.
  expect_match(src, "Where the frozen fields still reach the scores")
  expect_match(src, "tenth of the")
})
