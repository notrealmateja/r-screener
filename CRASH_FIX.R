################################################################################
# CRASH FIX — fixes syntax error on line ~1826 causing app to crash
# setwd("~/Downloads/EdgeScreener2 2")
# source("CRASH_FIX.R")
################################################################################

setwd("~/Downloads/EdgeScreener2 2")
cat("Reading app.R...\n")
lines <- readLines("app/app.R")
app   <- paste(lines, collapse="\n")
cat("Lines:", length(lines), "\n\n")

# ── FIX 1: inline if() inside list() — not valid R syntax in this context
app <- gsub(
  'xaxis=list(title=if("ret_1m" %in% names(df)) "1M Return (%)" else "Avg Score",',
  'xaxis=list(title=ifelse("ret_1m" %in% names(df), "1M Return (%)", "Avg Score"),',
  app, fixed=TRUE
)

# ── FIX 2: same pattern on the text= line
app <- gsub(
  'text=~paste0(sector, ": ", round(avg_ret,2), if("ret_1m" %in% names(df)) "%" else " pts"),',
  'text=~paste0(sector, ": ", round(avg_ret,2), ifelse("ret_1m" %in% names(df), "%", " pts")),',
  app, fixed=TRUE
)

# ── FIX 3: any other bare if() inside list() calls from the patch
app <- gsub(
  'if("ret_1m" %in% names(df)) "%" else " pts"',
  'ifelse("ret_1m" %in% names(df), "%", " pts")',
  app, fixed=TRUE
)
app <- gsub(
  'if("ret_1m" %in% names(df)) "1M Return (%)" else "Avg Score"',
  'ifelse("ret_1m" %in% names(df), "1M Return (%)", "Avg Score")',
  app, fixed=TRUE
)

# Write fixed app.R
writeLines(strsplit(app, "\n")[[1]], "app/app.R")
cat("✓ app.R fixed and saved\n\n")

# Verify no parse errors before deploying
cat("Checking syntax...\n")
result <- tryCatch({
  parse(file="app/app.R")
  cat("✓ Syntax OK — no errors found\n\n")
  TRUE
}, error=function(e) {
  cat("✗ Still has error:", conditionMessage(e), "\n\n")
  FALSE
})

if (result) {
  cat("=================================================\n")
  cat("  CRASH FIX COMPLETE — safe to deploy!\n\n")
  cat("  rsconnect::deployApp(\n")
  cat("    appDir='~/Downloads/EdgeScreener2 2/app',\n")
  cat("    appName='r-codescreener'\n")
  cat("  )\n")
  cat("=================================================\n")
} else {
  cat("=================================================\n")
  cat("  WARNING: Still has a syntax error.\n")
  cat("  Screenshot the error above and send it.\n")
  cat("=================================================\n")
}
