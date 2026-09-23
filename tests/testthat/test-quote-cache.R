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
