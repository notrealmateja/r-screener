# =============================================================================
# 05_run_all.R — Full EdgeScreener Pipeline
# source("R/05_run_all.R")
# =============================================================================
SOURCED_BY_MASTER <- TRUE
t0 <- Sys.time()
message("================================================")
message("   EDGESCREENER — BLOOMBERG-STYLE TERMINAL")
message(paste("   Started:", format(t0, "%Y-%m-%d %H:%M:%S")))
message("================================================\n")

for (d in c("data","output","app")) if (!dir.exists(d)) dir.create(d, recursive=TRUE)

source("R/01_fundamentals.R")
source("R/02_momentum.R")
source("R/03_data.R")
source("R/04_master_score.R")

fund_data <- tryCatch(run_module1(), error=function(e){message("M1 ERROR: ",e$message); NULL})
mom_data  <- tryCatch(run_module2(fund_data$symbol), error=function(e){message("M2 ERROR: ",e$message); NULL})
m3_data   <- tryCatch(run_module3(fund_data$symbol), error=function(e){message("M3 ERROR: ",e$message); NULL})
master    <- tryCatch(build_master_score(fund_data, mom_data, m3_data$squeeze),
                      error=function(e){message("M4 ERROR: ",e$message); NULL})

saveRDS(list(last_updated=format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
             elapsed=round(difftime(Sys.time(),t0,units="mins"),1),
             n_stocks=if(!is.null(master)) nrow(master) else 0),
        "data/meta.rds")

t1 <- Sys.time()
message("\n================================================")
message("   PIPELINE COMPLETE")
message(paste("   Time:", round(difftime(t1,t0,units="mins"),1), "minutes"))
if (!is.null(master)) message(paste("   Stocks scored:", nrow(master)))
message("================================================")
message("\nLaunch: shiny::runApp('app/')")
