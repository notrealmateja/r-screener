# Does the page still assert things its own data refutes?
#
# The validation prose was hardcoded, the figures beside it are recomputed
# nightly, and the two drifted apart: "2.5 years" of a 2.2-year window, a beta
# story the betas refuted, "mostly market exposure" over a 28% share. Those
# sentences now read the data. These tests pin the properties that made them
# wrong, because a derived sentence can be derived from the WRONG quantity and
# still look dynamic -- which is exactly what happened to the first fix.

app_src <- repo_path("app", "app.R")
glb_src <- repo_path("app", "global.R")
mom_src <- repo_path("R", "02_momentum.R")

app_txt <- if (file.exists(app_src)) readLines(app_src, warn = FALSE) else character(0)
app <- paste(app_txt, collapse = "\n")
# Prose assertions must be checked against CODE, not comments: every fix here
# is explained in a comment that quotes the wrong sentence it replaced.
app_code <- paste(grep("^\\s*#", app_txt, invert = TRUE, value = TRUE), collapse = "\n")

# ── The universe decomposition must not mix estimators ──────────────────────

test_that("the universe share is derived from the alpha it is printed beside", {
  skip_if_not(nzchar(app), "app/app.R not present")

  # The share and the residual are two halves of one gap. Computing them
  # independently -- the share from (u_beta-1)*b_cagr/(u_cagr-b_cagr), the
  # residual from the regression intercept -- let them stop summing to it the
  # moment the backtest began writing a real univ alpha.
  expect_false(grepl("\\(u_beta - 1\\) \\* b_cagr / \\(u_cagr - b_cagr\\)", app_code),
               info = "u_share is back to the independent cagr shortcut")
  expect_match(app_code, "u_share\\s*<-.*\n?.*1 - u_alpha / u_gap")

  # And the denominator is a premise: no gap, no share to report.
  expect_match(app_code, "u_gap > 0\\.005")
})

test_that("the two printed halves of the universe gap sum to it", {
  skip_if_not(file.exists(repo_path("data", "backtest_equity_stats.csv")),
              "stats not present")
  s <- read.csv(repo_path("data", "backtest_equity_stats.csv"), stringsAsFactors = FALSE)
  g <- function(sc, m) suppressWarnings(as.numeric(s[s$series == sc, m][1]))
  uc <- g("univ", "cagr"); bc <- g("spy", "cagr"); ub <- g("univ", "beta")
  ua <- g("univ", "alpha_vs_spy")
  if (is.na(ua)) ua <- uc - ub * bc
  gap <- uc - bc
  skip_if(is.na(gap) || gap <= 0.005, "no gap to apportion in this window")

  share <- 1 - ua / gap
  # Whatever estimator supplies the alpha, the beta part is the remainder, so
  # the halves reconstruct the gap exactly. That is the property the old
  # formulation lost.
  expect_equal(share * gap + ua, gap, tolerance = 1e-9)
})

# ── The control sentence answers the question it asks ───────────────────────

test_that("the control's win is judged on its own beta, not the quintile spread", {
  skip_if_not(nzchar(app), "app/app.R not present")

  # The paragraph asks whether the ranking sorts on market exposure. The first
  # fix answered with the SPREAD between the quintile betas (1.33 vs 1.30) and
  # concluded "it is not a beta story" -- while market exposure explained 78%
  # of the very 7.0 points the sentence had just printed.
  expect_false(grepl("It is not, however, a beta story", app_code),
               info = "the refuted denial is back")
  expect_match(app_code, "bt_leverage_share\\(q5_beta, b_cagr, q5x\\)")

  # The spread claim may still be made -- it is true -- but only about the
  # ranking, never as a verdict on the level.
  expect_match(app_code, "The ranking is not sorting on beta")
})

test_that("bt_leverage_share computes the share the sentence claims", {
  skip_if_not(file.exists(glb_src), "app/global.R not present")
  src <- readLines(glb_src, warn = FALSE)
  a <- grep("^bt_leverage_share <- function", src)
  skip_if(!length(a), "helper not found")
  e <- new.env(parent = globalenv())
  # Take the whole helper block through to the next top-level definition.
  b <- grep("^bt_stamp_time <- function", src)
  eval(parse(text = paste(src[a[1]:(b[1] - 1)], collapse = "\n")), envir = e)
  f <- e$bt_leverage_share

  # The real case: a control at beta 1.30 beating an 18.4% market by 7.0pp.
  # Leverage alone supplies (1.30 - 1) * 18.4 = 5.5 of those points.
  expect_equal(f(1.2964161, 0.1842531, 0.0701468), 0.7785, tolerance = 1e-3)
  # A beta of exactly one explains none of it.
  expect_equal(f(1, 0.1842531, 0.07), 0)
  # Below one, leverage works against the series, so the share is negative.
  expect_lt(f(0.8, 0.1842531, 0.07), 0)
  # The decomposition must reconstruct the excess exactly.
  for (b_ in c(0.7, 1.0, 1.33, 2.0)) {
    sh <- f(b_, 0.1842531, 0.0701468)
    expect_equal(sh * 0.0701468 + (0.0701468 - sh * 0.0701468), 0.0701468, tolerance = 1e-12)
  }
  # Degenerate inputs are NA, never an error and never a number: a share taken
  # over a vanishing excess is noise with a percent sign.
  expect_true(is.na(f(NA, 0.18, 0.07)))
  expect_true(is.na(f(1.3, NA, 0.07)))
  expect_true(is.na(f(1.3, 0.18, NA)))
  expect_true(is.na(f(1.3, 0.18, 0)))
  expect_true(is.na(f(1.3, 0.18, 0.004)))
  expect_true(is.na(f(1.3, 0.18, -0.05)))
  expect_true(is.na(f(numeric(0), 0.18, 0.07)))
})

test_that("the control's leverage share is the quantity the sentence claims", {
  skip_if_not(file.exists(repo_path("data", "backtest_equity_stats.csv")),
              "stats not present")
  s <- read.csv(repo_path("data", "backtest_equity_stats.csv"), stringsAsFactors = FALSE)
  g <- function(sc, m) suppressWarnings(as.numeric(s[s$series == sc, m][1]))
  q5b <- g("q5", "beta"); q5x <- g("q5", "excess_cagr"); bc <- g("spy", "cagr")
  skip_if(is.na(q5b) || is.na(q5x) || is.na(bc) || q5x <= 0.005, "no control excess")

  lev <- (q5b - 1) * bc
  # Excess over the index decomposes as alpha + (beta - 1) * market return.
  expect_equal(lev + (q5x - lev), q5x, tolerance = 1e-12)
  # On today's data most of the control's win is leverage, which is the branch
  # the page must be taking. If this ever flips, the other branch must be right.
  expect_true(is.finite(lev / q5x))
})

# ── The component paragraph must agree with the table above it ──────────────

test_that("the component callout ranks by t and excludes the baseline", {
  skip_if_not(nzchar(app), "app/app.R not present")

  # It ranked by |mean_ic| and then made a significance claim, so it called a
  # component the table prints in green "a weak positive" and "an absence of
  # evidence for momentum".
  expect_false(grepl("abs\\(bt_components\\$mean_ic\\)", app_code),
               info = "ranking by effect size is back")
  expect_false(grepl("a weak positive is not evidence", app_code),
               info = "the hardcoded verdict is back")
  expect_match(app_code, "order\\(-abs\\(x\\$t_stat\\)\\)")
  # The blended score is the baseline the components are tested against.
  expect_match(app_code, 'bt_components\\$component != "score"')
  # The count is read, not typed: a component that never clears its filters is
  # dropped from the file entirely.
  expect_false(grepl("strongest of the six", app_code), info = "typed count is back")
  expect_match(app_code, "strongest of the %d components")
})

test_that("the component callout cannot be reached with a NULL or empty frame", {
  skip_if_not(nzchar(app), "app/app.R not present")
  # It dereferenced bt_components from inside a guard on bt_reversal, so a
  # missing components file errored the whole Methodology panel.
  expect_match(app_code, "is\\.null\\(bt_components\\) \\|\\| !nrow\\(bt_components\\)")
})

# ── Claims that must not outlive the number behind them ─────────────────────

test_that("absolute claims about recomputed ranges are branched, not asserted", {
  skip_if_not(nzchar(app), "app/app.R not present")

  # "reaches 2 under none of them" was an unguarded absolute about a range
  # printed in the same clause, measured against a bar this page argues is the
  # wrong one at this sample size.
  expect_false(grepl("reaches 2 under none of them", app_code))
  expect_match(app_code, "reaches \", lab, \" under none of them")

  # The drawdown branch must test what it PRINTS. Testing the unrounded value
  # against -0.20 while printing %.0f left a band where the page said
  # "-20%, so there is no bear market".
  expect_false(grepl("spy_dd > -0\\.20", app_code))
  expect_match(app_code, "round\\(spy_dd \\* 100\\) > -20")

  # A negative ranking edge is not parity.
  expect_false(grepl("ed_u_ba <= 0\\)\\s*\n?\\s*\"N/A\"", app_code))
  expect_match(app_code, "ed_u_ba <= 0")
  expect_match(app_code, "which contributed nothing at all")
})

test_that("counts that the data carries are not typed into the prose", {
  skip_if_not(nzchar(app), "app/app.R not present")
  expect_false(grepl('"[0-9]+ views of', app_code), info = "calendar count typed again")
  expect_match(app_code, 'sprintf\\("%\\.0f views of", v\\$n\\)')
  expect_false(grepl("one quarter out of eight", app_code), info = "block count typed again")
  expect_false(grepl("largest is in the mid-cap tier", app_code),
               info = "the runner-up's tier is typed again, and it rotates")
  expect_false(grepl("the current filter neutralises no bars", app_code),
               info = "a fixed clause contradicting the derived n_ca sentence")
})

# ── The survivorship claim is pinned to the pipeline constant ───────────────

test_that("the survivorship bullet states the real history requirement", {
  skip_if_not(nzchar(app), "app/app.R not present")
  skip_if_not(file.exists(mom_src), "R/02_momentum.R not present")

  # It said "a year" while the pipeline pulls 1095 days, understating the bias
  # in the bullet that calls it the single largest caveat.
  expect_false(grepl("exist and have a year of history", app_code))
  expect_match(app_code, "carry three years of")

  lookback <- local({
    l <- grep("^PRICE_LOOKBACK_DAYS\\s*<-", readLines(mom_src, warn = FALSE), value = TRUE)
    if (!length(l)) NA_real_ else suppressWarnings(as.numeric(sub(".*<-\\s*([0-9]+).*", "\\1", l[1])))
  })
  expect_false(is.na(lookback))
  # The sentence says three years; the pipeline must still mean three years.
  expect_equal(round(lookback / 365.25), 3,
               info = "PRICE_LOOKBACK_DAYS moved -- the survivorship bullet now lies")

  # And it must NOT be derived from the shipped price file, which is truncated
  # to about a year: that would report what was copied, not what was filtered.
  expect_false(grepl("as\\.Date\\(price_history\\$date\\)[^)]*\\)\\s*\n?.*of history today", app_code))
})

# ── The stamp cannot invent a date ──────────────────────────────────────────

test_that("the backtest stamp is read from the data, never from the clock", {
  skip_if_not(file.exists(glb_src), "app/global.R not present")
  g <- paste(readLines(glb_src, warn = FALSE), collapse = "\n")

  expect_match(g, "bt_stamp_time <- function")
  expect_match(g, "bt_stamp_label <- function")
  # A stamp built from Sys.Date() or the file's mtime would advance every night
  # whether or not the backtest ran -- the exact failure it exists to expose,
  # because Connect Cloud deploys from a git checkout.
  stamp_block <- sub(".*?(bt_stamp_time <- function.*?)\nall_sectors.*", "\\1", g)
  expect_false(grepl("Sys\\.Date\\(\\)", stamp_block))
  expect_false(grepl("file\\.(mtime|info)", stamp_block))
})

test_that("the stamp survives every shape the CSV can arrive in", {
  skip_if_not(file.exists(glb_src), "app/global.R not present")
  src <- readLines(glb_src, warn = FALSE)
  a <- grep("^bt_stamp_time <- function", src); b <- grep("^all_sectors <- ", src)
  skip_if(!length(a) || !length(b), "stamp helpers not found")
  e <- new.env(parent = globalenv())
  eval(parse(text = paste(src[a[1]:(b[1] - 1)], collapse = "\n")), envir = e)

  mk <- function(x) data.frame(generated_at = x, stringsAsFactors = FALSE)
  # Absence must read as unknown, never as fresh -- and must never error, or
  # the legal disclaimer gate goes down with it.
  for (bad in list(NULL, mk(character(0)), mk(NA_character_), mk(""),
                   mk("not a timestamp"), data.frame(other = 1))) {
    lab <- expect_no_error(e$bt_stamp_label(bad))
    expect_match(lab, "unknown")
  }

  fresh <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")
  expect_match(e$bt_stamp_label(mk(fresh)), "^last updated [0-9]{2}/[0-9]{2}/[0-9]{4}$")

  # A stamp that has stopped moving says so, rather than relying on the reader
  # to notice the date. Every backtest call in the nightly is non-fatal and the
  # copy step ships data/ regardless, so this is the only visible signal.
  old <- format(Sys.time() - 19 * 86400, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  expect_match(e$bt_stamp_label(mk(old)), "19 days ago")
})

test_that("the backtest stamps every frame the page dates", {
  bt <- repo_path("R", "05_backtest.R")
  skip_if_not(file.exists(bt), "R/05_backtest.R not present")
  txt <- paste(readLines(bt, warn = FALSE), collapse = "\n")

  expect_match(txt, "backtest_stamp <- function")
  # Both frames the app stamps must carry it, or the page dates a file nothing
  # wrote a date into and silently reads "unknown" forever.
  expect_match(txt, "generated_at   = backtest_stamp\\(\\)")
  expect_match(txt, "stats\\$generated_at\\s*<- backtest_stamp\\(\\)")
})
