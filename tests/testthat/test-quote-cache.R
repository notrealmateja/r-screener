# The shared quote cache.
#
# Opening Deep Dive cost about 1.3s on the deployed site, nearly all of it
# waiting on a quote while the header sat empty — meanwhile the ticker tape was
# fetching 25 quotes a minute and discarding them. The cache reuses that work.
# It must never serve a price older than the refresh interval, and must never
# store a junk value, because a cache hit is displayed without further checks.

if (have_pkgs("dplyr")) {
  gl <- repo_path("app", "global.R")
  have_fn <- FALSE
  if (file.exists(gl)) {
    src <- readLines(gl, warn = FALSE)
    from <- grep("^QUOTE_TTL <- ", src)
    if (length(from)) {
      eval(parse(text = paste(src[from[1]:length(src)], collapse = "\n")),
           envir = globalenv())
      have_fn <- exists("quote_cache_put") && exists("quote_cache_get")
    }
  }

  test_that("a stored quote comes back", {
    skip_if_not(have_fn, "cache unavailable")
    quote_cache_put("AAA", 12.5, 1.2, Sys.time())
    e <- quote_cache_get("AAA")
    expect_false(is.null(e))
    expect_equal(e$price, 12.5)
    expect_equal(e$chg, 1.2)
  })

  test_that("several symbols store in one call", {
    skip_if_not(have_fn, "cache unavailable")
    n <- quote_cache_put(c("BBB", "CCC"), c(5, 6), c(0.1, -0.2))
    expect_equal(n, 2)
    expect_equal(quote_cache_get("BBB")$price, 5)
    expect_equal(quote_cache_get("CCC")$price, 6)
  })

  # The property that keeps the cache honest.
  test_that("an entry past its TTL is a miss, not a stale price", {
    skip_if_not(have_fn, "cache unavailable")
    quote_cache_put("DDD", 20)
    expect_false(is.null(quote_cache_get("DDD", ttl = 60)))
    # same entry, asked for 61 seconds later
    later <- Sys.time() + 61
    expect_null(quote_cache_get("DDD", ttl = 60, now = later))
  })

  test_that("junk prices are never stored", {
    skip_if_not(have_fn, "cache unavailable")
    for (bad in list(NA_real_, 0, -4, NaN, Inf)) {
      quote_cache_put("EEE", bad)
      expect_null(quote_cache_get("EEE"),
                  info = paste("stored a bad price:", bad))
    }
  })

  test_that("a mixed batch keeps the good rows and drops the bad", {
    skip_if_not(have_fn, "cache unavailable")
    n <- quote_cache_put(c("FFF", "GGG", "HHH"), c(10, NA, -2), c(1, 1, 1))
    expect_equal(n, 1)
    expect_equal(quote_cache_get("FFF")$price, 10)
    expect_null(quote_cache_get("GGG"))
    expect_null(quote_cache_get("HHH"))
  })

  test_that("unknown or malformed symbols miss cleanly", {
    skip_if_not(have_fn, "cache unavailable")
    expect_null(quote_cache_get("NOPE"))
    expect_null(quote_cache_get(""))
    expect_null(quote_cache_get(NA_character_))
    expect_null(quote_cache_get(c("A", "B")))
  })
}

# Found by the session audit, confirmed by two independent verifiers.
#
# The ticker tape passed a PRE-filter timestamp vector alongside POST-filter
# symbols. quantmod returns an all-NA row for any delisted or halted ticker, so
# that row was dropped from the symbols but NOT from the timestamps, shifting
# every stamp after it onto the wrong stock. Three things broke: the badge
# showed another stock's trade time; the symbol after the gap got NA and
# rendered as a bare "LIVE" with no time, breaking the promise that a price
# always says how old it is; and append_live_point reads that timestamp for the
# chart point's date, so a borrowed earlier stamp made the live point look
# stale and it was silently dropped — leaving the chart disagreeing with the
# header, the exact bug the feature existed to fix.
if (have_pkgs("dplyr")) {
  test_that("a mismatched time vector is refused, not indexed positionally", {
    skip_if_not(have_fn, "cache unavailable")
    t3 <- as.POSIXct(c("2026-09-23 16:00:03", "2026-09-23 16:00:01",
                       "2026-09-23 16:00:02"))
    # three stamps, two symbols: the shape produced by a dropped quote row
    quote_cache_put(c("MIS1", "MIS2"), c(10, 20), c(1, 2), t3)
    for (s in c("MIS1", "MIS2")) {
      e <- quote_cache_get(s)
      expect_false(is.null(e))
      # it must not have taken t3[1] and t3[2] — those belong to other symbols
      expect_false(identical(as.character(e$time), as.character(t3[1])))
      expect_false(identical(as.character(e$time), as.character(t3[2])))
    }
  })

  test_that("an aligned time vector is stored symbol by symbol", {
    skip_if_not(have_fn, "cache unavailable")
    t2 <- as.POSIXct(c("2026-09-23 15:59:00", "2026-09-23 16:00:00"))
    quote_cache_put(c("ALN1", "ALN2"), c(10, 20), c(1, 2), t2)
    expect_equal(as.character(quote_cache_get("ALN1")$time), as.character(t2[1]))
    expect_equal(as.character(quote_cache_get("ALN2")$time), as.character(t2[2]))
  })

  test_that("one timestamp is recycled across every symbol", {
    skip_if_not(have_fn, "cache unavailable")
    one <- as.POSIXct("2026-09-23 16:00:00")
    quote_cache_put(c("RC1", "RC2"), c(10, 20), c(1, 2), one)
    expect_equal(as.character(quote_cache_get("RC1")$time), as.character(one))
    expect_equal(as.character(quote_cache_get("RC2")$time), as.character(one))
  })

  test_that("a mismatched price vector stores nothing at all", {
    skip_if_not(have_fn, "cache unavailable")
    expect_equal(quote_cache_put("ONE", c(10, 20), c(1, 2)), 0L)
    expect_null(quote_cache_get("ONE"))
  })

  test_that("NA and empty symbols are skipped rather than stored or thrown on", {
    skip_if_not(have_fn, "cache unavailable")
    n <- expect_silent(quote_cache_put(c("GOOD", NA_character_, ""), c(1, 2, 3)))
    expect_equal(n, 1)
    expect_equal(quote_cache_get("GOOD")$price, 1)
    expect_null(quote_cache_get("NA"))    # never stored under the literal "NA"
  })
}
