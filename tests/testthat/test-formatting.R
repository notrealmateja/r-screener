# Returns are stored as decimal fractions (0.0501 = 5.01%).  Three screener
# tables re-formatted them inline without scaling by 100, so a real +5.01%
# return rendered as "+0.1%" and every number on the dashboard looked flat.
test_that("fmt_ret scales decimal fractions to percent", {
  skip_if_not(have_pkgs("dplyr", "readr", "shiny"), "app deps missing")
  # local = environment() (not TRUE) so the definitions land in this test's env
  # even though the call is wrapped. global.R warns when meta.rds is absent,
  # which is expected outside a deployed app directory.
  suppressWarnings(source(repo_path("app", "global.R"), local = environment()))

  expect_equal(fmt_ret(0.0501),  "+5%")
  expect_equal(fmt_ret(0.2743),  "+27.4%")
  expect_equal(fmt_ret(-0.0606), "-6.1%")
  expect_equal(fmt_ret(NA),      "N/A")

  # The regression itself: a 5% return must never render as 0.1%
  expect_false(fmt_ret(0.0501) == "+0.1%")
})

test_that("fmt_ret signs and vectorises correctly", {
  skip_if_not(have_pkgs("dplyr", "readr", "shiny"), "app deps missing")
  # local = environment() (not TRUE) so the definitions land in this test's env
  # even though the call is wrapped. global.R warns when meta.rds is absent,
  # which is expected outside a deployed app directory.
  suppressWarnings(source(repo_path("app", "global.R"), local = environment()))

  expect_true(startsWith(fmt_ret(0.01), "+"))
  expect_true(startsWith(fmt_ret(-0.01), "-"))
  expect_equal(fmt_ret(0), "+0%")
  expect_equal(fmt_ret(c(0.05, -0.05, NA)), c("+5%", "-5%", "N/A"))
})

test_that("fmt_mktcap picks the right magnitude suffix", {
  skip_if_not(have_pkgs("dplyr", "readr", "shiny"), "app deps missing")
  # local = environment() (not TRUE) so the definitions land in this test's env
  # even though the call is wrapped. global.R warns when meta.rds is absent,
  # which is expected outside a deployed app directory.
  suppressWarnings(source(repo_path("app", "global.R"), local = environment()))

  expect_equal(fmt_mktcap(4.5e12), "$4.5T")
  expect_equal(fmt_mktcap(2.8e9),  "$2.8B")
  expect_equal(fmt_mktcap(4.0e7),  "$40M")
  expect_equal(fmt_mktcap(NA),     "N/A")
})
