# Deep-linking and refresh survival.
#
# Every section lived at one URL, so a refresh always dropped the viewer back
# on Overview. A real path (/deepdive) cannot fix that here: Posit Connect
# Cloud answers any path below the app root with a 404 and the request never
# reaches Shiny, so pushing one would turn a refresh from "wrong pane" into
# "error page". The query string survives, because the path stays at the root.
#
# The ticker half is real R and is executed below. The pane half runs in the
# browser; with no JS engine in this suite it is pinned structurally, and
# driven for real against the running app before release.
#
# Every block registers unconditionally and skips from the inside, so a missing
# dependency is a countable skip rather than a test that silently disappears.

route_app_path <- repo_path("app", "app.R")
route_app <- if (file.exists(route_app_path))
  paste(readLines(route_app_path, warn = FALSE), collapse = "\n") else ""

route_ready <- FALSE
local({
  gl <- repo_path("app", "global.R")
  if (!file.exists(gl) || !have_pkgs("shiny")) return()
  src <- readLines(gl, warn = FALSE)
  from <- grep("^route_ticker <- function", src)
  if (!length(from)) return()
  eval(parse(text = paste(src[from[1]:length(src)], collapse = "\n")),
       envir = globalenv())
  route_ready <<- exists("route_ticker")
})

SYMS <- c("AAPL", "MSFT", "NVDA")

test_that("a known ticker is read out of the query string", {
  skip_if_not(route_ready, "route_ticker unavailable")
  expect_equal(route_ticker("?pane=deepdive&t=NVDA", SYMS), "NVDA")
  expect_equal(route_ticker("?t=AAPL", SYMS), "AAPL")
})

test_that("an unknown ticker is refused rather than applied", {
  skip_if_not(route_ready, "route_ticker unavailable")
  # Silent failure otherwise: selectize given a value it does not know clears
  # the selection, so Deep Dive opens empty with nothing explaining why.
  expect_null(route_ticker("?pane=deepdive&t=NOTREAL", SYMS))
  expect_null(route_ticker("?t=", SYMS))
})

test_that("a missing, empty or absent query string yields nothing", {
  skip_if_not(route_ready, "route_ticker unavailable")
  expect_null(route_ticker(NULL, SYMS))
  expect_null(route_ticker("", SYMS))
  expect_null(route_ticker(NA_character_, SYMS))
  expect_null(route_ticker("?pane=overview", SYMS))
})

test_that("no symbol list means no restore, rather than an error", {
  skip_if_not(route_ready, "route_ticker unavailable")
  # master_data is NULL if the CSVs failed to load. Routing must not turn a
  # degraded app into a crashed one.
  expect_null(route_ticker("?t=AAPL", NULL))
  expect_null(route_ticker("?t=AAPL", character(0)))
})

test_that("the ticker is matched exactly, not by prefix", {
  skip_if_not(route_ready, "route_ticker unavailable")
  expect_null(route_ticker("?t=AAP", SYMS))
  expect_null(route_ticker("?t=AAPL2", SYMS))
})

# ── the pane half, pinned structurally ──────────────────────────────────────

test_that("the routing JS carries no double quote", {
  skip_if_not(nzchar(route_app), "app.R unreadable")
  # It is embedded in an R double-quoted string via HTML(). A single " in a
  # comment ends that string and the whole app stops parsing -- not a subtle
  # failure, but one a reviewer reading JS will not see. It has happened once.
  i <- regexpr("── URL routing ─", route_app)[1]
  expect_true(i > 0)
  block <- substr(route_app, i, i + 3200)
  expect_false(grepl('"', block, fixed = TRUE))
})

test_that("showPane takes an options bag and reports viewer moves", {
  skip_if_not(nzchar(route_app), "app.R unreadable")
  expect_true(grepl("function showPane(name, opts)", route_app, fixed = TRUE))
  expect_true(grepl("if (!opts || !opts.fromUrl) syncUrl(true);", route_app, fixed = TRUE))
})

test_that("a viewer move pushes history; a URL-driven one replaces", {
  skip_if_not(nzchar(route_app), "app.R unreadable")
  # Nothing previously asserted that pushState is used at all, so Back and
  # Forward were constrained only by the author having tried them once.
  expect_true(grepl("if (push) history.pushState(st, '', url); else history.replaceState(st, '', url);",
                    route_app, fixed = TRUE))
  # The ticker sync must be the replacing kind, or Back would walk stocks one
  # keystroke at a time instead of moving between sections.
  expect_true(grepl("Shiny.addCustomMessageHandler('edgeSyncUrl', function(_) { syncUrl(false); });",
                    route_app, fixed = TRUE))
})

test_that("a restore does not push a history entry over itself", {
  skip_if_not(nzchar(route_app), "app.R unreadable")
  expect_true(grepl("showPane(paneExists(pane) ? pane : PANE_DEFAULT, {fromUrl: true});",
                    route_app, fixed = TRUE))
})

test_that("back and forward move the stock, not only the pane", {
  skip_if_not(nzchar(route_app), "app.R unreadable")
  # The server-side restore is once per session, so on Back it is already
  # spent. Without this the address bar names one stock and the screen shows
  # another for the rest of the session.
  expect_true(grepl("applyTicker(q.get('t'));", route_app, fixed = TRUE))
  expect_true(grepl("if (el.selectize.getValue() !== sym) el.selectize.setValue(sym, false);",
                    route_app, fixed = TRUE))
  # And an unknown symbol is still refused on this path too.
  expect_true(grepl("if (!(sym in el.selectize.options)) return;", route_app, fixed = TRUE))
})

test_that("both arms of the initial restore are wired", {
  skip_if_not(nzchar(route_app), "app.R unreadable")
  expect_true(grepl("window.addEventListener('popstate', restoreFromUrl);",
                    route_app, fixed = TRUE))
  expect_true(grepl("document.addEventListener('DOMContentLoaded', restoreFromUrl);",
                    route_app, fixed = TRUE))
  # The else arm matters as much: if the document has already parsed by the
  # time this runs, the listener never fires and deep links stop working.
  expect_true(grepl("else\n      restoreFromUrl();", route_app, fixed = TRUE))
})

test_that("the ticker rides the URL only on the pane that shows one", {
  skip_if_not(nzchar(route_app), "app.R unreadable")
  expect_true(grepl("if (pane === 'deepdive' && ticker) q.set('t', ticker);",
                    route_app, fixed = TRUE))
})

test_that("no code pushes a real path, which Connect Cloud would 404", {
  skip_if_not(nzchar(route_app), "app.R unreadable")
  expect_true(grepl("return location.pathname + '?' + q.toString();",
                    route_app, fixed = TRUE))
  expect_false(grepl("pushState(st, '', '/'", route_app, fixed = TRUE))
})

test_that("the URL restore runs once and the ticker sync skips its init", {
  skip_if_not(nzchar(route_app), "app.R unreadable")
  expect_true(grepl("observeEvent(session$clientData$url_search, once = TRUE, {",
                    route_app, fixed = TRUE))
  # Matched as one block, not as a bare ignoreInit anywhere in the file: other
  # observers carry that flag too, so a loose grep stays green after it is
  # stripped from this one. A mutation proved exactly that.
  expect_true(grepl(paste0("observeEvent(input$dd_ticker, {\n",
                           "    session$sendCustomMessage(\"edgeSyncUrl\", list())\n",
                           "  }, ignoreInit = TRUE)"),
                    route_app, fixed = TRUE))
})

test_that("the deep-link restore is wired end to end", {
  skip_if_not(nzchar(route_app), "app.R unreadable")
  # Asserted past the comma this time: the symbol universe it validates
  # against and the input it writes to were both previously unverified, so the
  # restore could have been wired to a column and an input that do not exist.
  expect_true(grepl("t <- route_ticker(session$clientData$url_search,\n                      if (is.null(master_data)) NULL else master_data$symbol)",
                    route_app, fixed = TRUE))
  expect_true(grepl('updateSelectInput(session, "dd_ticker", selected = t)',
                    route_app, fixed = TRUE))
})

test_that("the restore does not overwrite a stock the viewer just picked", {
  skip_if_not(nzchar(route_app), "app.R unreadable")
  # The pane restore is instant and client-side, so the selector is usable well
  # before this server message lands.
  expect_true(grepl("cur  <- isolate(input$dd_ticker)", route_app, fixed = TRUE))
  expect_true(grepl("if (!is.null(cur) && nzchar(cur) && !identical(cur, dflt)) return()",
                    route_app, fixed = TRUE))
})

test_that("parking the stream detaches it instead of navigating it", {
  skip_if_not(nzchar(route_app), "app.R unreadable")
  # Assigning iframe.src navigates the frame, and a frame navigation pushes an
  # entry onto the PARENT page's session history. Invisible while nothing used
  # history; once panes became addressable every move cost two entries and the
  # first Back was a dead click. Found by driving the app, not by this suite.
  expect_false(grepl("f.src = 'about:blank'", route_app, fixed = TRUE))
  expect_false(grepl("f2.src = f2.dataset.src", route_app, fixed = TRUE))
  expect_true(grepl("window.__edgeStream = {node: f, parent: f.parentNode, next: f.nextSibling}; f.remove();",
                    route_app, fixed = TRUE))
  expect_true(grepl("st.parent.isConnected && !st.parent.querySelector('iframe')",
                    route_app, fixed = TRUE))
})

test_that("a stream mounted after a deep link is parked too", {
  skip_if_not(nzchar(route_app), "app.R unreadable")
  # Shiny mounts the player asynchronously, so a deep link onto another pane
  # has no iframe for showPane to park -- and the player then arrives inside a
  # hidden Overview and plays, with sound, unseen. Reproduced live.
  expect_true(grepl("new MutationObserver(function() {", route_app, fixed = TRUE))
  expect_true(grepl("if (activePane() === 'overview') return;", route_app, fixed = TRUE))
  expect_true(grepl(".observe(host, {childList: true, subtree: true});",
                    route_app, fixed = TRUE))
})
