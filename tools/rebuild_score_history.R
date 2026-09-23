#!/usr/bin/env Rscript
# Rebuild data/score_history.csv from git.
#
# The pipeline has committed data/master_scored.csv every night since
# 2026-05-05, so several months of daily scores already exist as git objects.
# Nothing read them, which meant the site could show what a stock scores but
# never whether that score was rising or falling. This walks the file's history
# and turns those commits into one table.
#
# Rerunnable: it rebuilds from scratch each time. The nightly appends to the
# same file through merge_history(), so running this again only restores what
# git already holds.
#
# Usage: Rscript tools/rebuild_score_history.R [output_path]

suppressMessages({ library(dplyr); library(readr) })

out_path <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else "data/score_history.csv"
target   <- "data/master_scored.csv"

# Two calls rather than one combined format string. system2() splits arguments
# on whitespace, so "--format=%H %cd" reaches git as two arguments, and a
# separator like "|" is eaten by the shell. Both lists come back in the same
# order, so they zip together safely.
git_log <- function(fmt, extra = character(0))
  system2("git", c("log", paste0("--format=", fmt), extra, "--", target),
          stdout = TRUE)
sha_list  <- git_log("%H")
date_list <- git_log("%cd", "--date=format:%Y-%m-%d")
if (!length(sha_list) || length(sha_list) != length(date_list))
  stop("No usable git history for ", target)

commits <- tibble(sha = sha_list, date = as.Date(date_list))

# Several runs can land on one day; keep the last commit for each date, which
# is the one whose numbers the site actually served.
commits <- commits %>% group_by(date) %>% slice(1) %>% ungroup() %>% arrange(date)
message("Reading ", nrow(commits), " daily snapshots from git...")

read_snapshot <- function(sha, date) {
  txt <- tryCatch(system2("git", c("show", paste0(sha, ":", target)),
                          stdout = TRUE, stderr = FALSE),
                  error = function(e) character(0))
  if (length(txt) < 2) return(NULL)
  d <- tryCatch(suppressWarnings(read_csv(I(txt), show_col_types = FALSE,
                                          progress = FALSE)),
                error = function(e) NULL)
  if (is.null(d) || !all(c("symbol", "master_score") %in% names(d))) return(NULL)
  d %>%
    filter(!is.na(symbol), !is.na(master_score)) %>%
    transmute(
      date       = date,
      symbol     = as.character(symbol),
      master_score = as.numeric(master_score),
      rating     = if ("rating" %in% names(d)) as.character(rating) else NA_character_
    ) %>%
    mutate(
      # Rank 1 is the best score. Universe size travels with the row because it
      # changed from 50 to 195 on 2026-08-05, and a rank move across that
      # boundary would be an artefact of the list growing, not of the stock.
      rank       = rank(-master_score, ties.method = "min"),
      n_universe = n()
    )
}

rows <- lapply(seq_len(nrow(commits)),
               function(i) read_snapshot(commits$sha[i], commits$date[i]))
ok <- Filter(Negate(is.null), rows)
message("  usable snapshots: ", length(ok), " of ", nrow(commits))
if (!length(ok)) stop("No snapshot could be parsed")

hist <- bind_rows(ok) %>%
  distinct(symbol, date, .keep_all = TRUE) %>%
  arrange(symbol, date)

write_csv(hist, out_path)
message(sprintf("Wrote %s: %s rows, %d symbols, %s to %s (%.0f KB)",
                out_path, format(nrow(hist), big.mark = ","),
                n_distinct(hist$symbol), min(hist$date), max(hist$date),
                file.size(out_path) / 1024))
uni <- hist %>% distinct(date, n_universe) %>% count(n_universe)
message("  universe sizes seen: ",
        paste(sprintf("%d names on %d days", uni$n_universe, uni$n), collapse = "; "))
