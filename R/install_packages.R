packages <- c(
  "httr", "jsonlite", "tidyquant", "quantmod", "TTR",
  "dplyr", "tidyr", "purrr", "readr", "lubridate",
  "ggplot2", "plotly", "scales", "glue",
  "shiny", "shinydashboard", "DT", "rsconnect",
  "PerformanceAnalytics", "rmarkdown"
)
for (p in packages) {
  if (!requireNamespace(p, quietly=TRUE)) {
    message("Installing: ", p); install.packages(p, dependencies=TRUE, repos="https://cran.rstudio.com/")
  } else message("OK: ", p)
}
message("\nAll packages ready. Run: source('R/05_run_all.R')")
