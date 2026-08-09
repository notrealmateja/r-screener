# Capture dashboard screenshots for the README.
#
# Run locally, never in CI — it needs a headless Chrome and a live app process.
#   Rscript tools/capture_screenshots.R
#
# The disclaimer gate is dismissed via its sessionStorage flag before capture so
# the screenshots show the dashboard itself rather than the modal.
suppressMessages({library(webshot2); library(chromote)})

PORT <- 7799
URL  <- sprintf("http://127.0.0.1:%d", PORT)
OUT  <- "docs"
if (!dir.exists(OUT)) dir.create(OUT, recursive = TRUE)

message("Starting Shiny app on port ", PORT, "...")
app <- callr::r_bg(function(port) {
  shiny::runApp("app", port = port, host = "127.0.0.1", launch.browser = FALSE)
}, args = list(port = PORT), supervise = TRUE)

# Wait for the port to accept connections
ok <- FALSE
for (i in 1:40) {
  Sys.sleep(1)
  con <- suppressWarnings(try(socketConnection("127.0.0.1", PORT, open = "r+",
                                               blocking = FALSE, timeout = 1),
                              silent = TRUE))
  if (!inherits(con, "try-error")) { close(con); ok <- TRUE; break }
}
if (!ok) { app$kill(); stop("app did not start") }
message("App is up.")

shot <- function(file, pane, wait = 9) {
  b <- ChromoteSession$new()
  on.exit(try(b$close(), silent = TRUE), add = TRUE)
  b$Emulation$setDeviceMetricsOverride(width = 1600, height = 1000,
                                       deviceScaleFactor = 2, mobile = FALSE)
  b$Page$navigate(URL); Sys.sleep(3)
  # Dismiss the gate, then switch panes and let Shiny finish rendering
  b$Runtime$evaluate("try{sessionStorage.setItem('edgescreener_disclaimer_v1','ack');}catch(e){}")
  b$Page$navigate(URL); Sys.sleep(wait)
  b$Runtime$evaluate(sprintf("showPane('%s'); window.dispatchEvent(new Event('resize'));", pane))
  Sys.sleep(wait)
  path <- file.path(OUT, file)
  b$screenshot(filename = path, selector = "body")
  message("  wrote ", path)
}

tryCatch({
  shot("dashboard-overview.png",    "overview")
  shot("dashboard-methodology.png", "about", wait = 11)
}, finally = {
  message("Stopping app.")
  app$kill()
})
message("Done.")
