# Shared setup. Every module guards its auto-run behind SOURCED_BY_MASTER, so
# set it in the global environment before sourcing anything — otherwise loading
# a module would kick off a live pipeline run.
assign("SOURCED_BY_MASTER", TRUE, envir = globalenv())

repo_root <- local({
  d <- normalizePath(".", mustWork = FALSE)
  while (!file.exists(file.path(d, "README.md")) && dirname(d) != d) d <- dirname(d)
  d
})

repo_path <- function(...) file.path(repo_root, ...)

# Source a file only if its library dependencies are present, so a partial local
# install skips the affected tests rather than erroring the whole suite.
have_pkgs <- function(...) all(vapply(c(...), requireNamespace, logical(1), quietly = TRUE))
