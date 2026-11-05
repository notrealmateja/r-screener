# The repo root and R/ had accumulated dead copies of live files: a 1981-line
# app.R at the root shadowing app/app.R (the only one Connect Cloud deploys), a
# stale GitHub Actions workflow saved as "R/daily update", and six-month-old
# CSVs under R/data and R/app on a schema the pipeline no longer writes.
#
# Two of those were not merely untidy.
#
#   * "R/daily update" is YAML. The README's documented manual-pipeline command
#     sources every file list.files() returns from R/, so the documented command
#     died on "R/daily update:1:13: unexpected symbol" before it ever reached
#     run_module1(). Dead debris, live breakage.
#
#   * A second app.R is worse than clutter. An edit made to the wrong copy looks
#     applied and ships nothing -- which is how the news-staleness fix came to
#     exist in app/app.R while the root copy kept both the undated panel and the
#     duplicate output$news_feed that app/app.R:3887 records as removed.
#
# These tests guard the cleaned-up state. They assert over TRACKED files: the
# repo can only control what it commits, and an untracked scratch copy in
# somebody's working directory must not fail the suite for everyone.

git_tracked <- function() {
  out <- suppressWarnings(
    system2("git", c("-C", shQuote(repo_root), "ls-files"),
            stdout = TRUE, stderr = FALSE))
  if (!length(out) || !is.null(attr(out, "status"))) character(0) else out
}

test_that("every tracked file under R/ is an R source the pipeline can parse", {
  tracked <- git_tracked()
  skip_if_not(length(tracked) > 0, "git ls-files unavailable")

  in_r <- grep("^R/", tracked, value = TRUE)
  expect_true(length(in_r) > 0, info = "no tracked files under R/ at all")

  # Not an R source by name. "R/daily update" (YAML), "R/.DS_Store" and the
  # stale CSVs under R/data and R/app all failed here.
  not_r <- in_r[!grepl("[.]R$", in_r)]
  expect_identical(
    not_r, character(0),
    info = paste("R/ holds only pipeline modules; the README's documented",
                 "command sources everything list.files() returns from it.",
                 "Found:", paste(not_r, collapse = ", ")))

  # Name alone is not enough: the command source()s these, so they must parse.
  unparseable <- Filter(
    function(f) inherits(tryCatch(parse(repo_path(f)), error = function(e) e),
                         "error"),
    in_r[grepl("[.]R$", in_r)])
  expect_identical(unparseable, character(0),
                   info = paste("unparseable:", paste(unparseable, collapse = ", ")))
})

test_that("only one app.R is tracked, and it is the one the workflow deploys", {
  tracked <- git_tracked()
  skip_if_not(length(tracked) > 0, "git ls-files unavailable")

  app_rs <- tracked[basename(tracked) == "app.R"]
  expect_identical(
    app_rs, "app/app.R",
    info = paste("exactly one app.R may be committed, at app/app.R; a second",
                 "copy makes an edit to the wrong file look applied.",
                 "Found:", paste(app_rs, collapse = ", ")))

  # Make the dependency explicit rather than implied: the deploy names this file.
  wf <- repo_path(".github", "workflows", "daily-update.yml")
  skip_if_not(file.exists(wf), "workflow not present")
  txt <- paste(readLines(wf, warn = FALSE), collapse = "\n")

  expect_match(txt, 'writeManifest\\(\\s*appDir\\s*=\\s*"app"\\s*,\\s*appPrimaryDoc\\s*=\\s*"app\\.R"',
               info = "the manifest must be written for appDir 'app' with app.R as primary doc")
  expect_match(txt, 'appDir\\s*=\\s*"app"\\s*,[^)]*appName',
               info = "deployApp must deploy appDir 'app'")
})

test_that("the README's pipeline command survives Rscript -e, backslashes and all", {
  readme <- repo_path("README.md")
  lines  <- readLines(readme, warn = FALSE)

  cmd <- grep("list\\.files\\(\\s*\"R\"", lines, value = TRUE)
  expect_true(length(cmd) == 1,
              info = "expected exactly one documented list.files(\"R\") command")

  pat <- sub('.*list\\.files\\(\\s*"R"\\s*,\\s*pattern\\s*=\\s*"([^"]*)".*', "\\1", cmd)
  expect_false(identical(pat, cmd),
               info = "the documented command must filter R/ by pattern, not source everything")

  # Rscript -e collapses backslash pairs before R parses the expression, so a
  # regex written as "\\.R$" reaches R as "\.R$" and dies with
  #   '\.' is an unrecognized escape in character string
  # It takes FOUR backslashes to survive, which nobody will maintain correctly.
  # A bracket class needs none, so require that the pattern carries no
  # backslash at all.
  expect_false(grepl("\\\\", pat, fixed = FALSE),
               info = paste("pattern must avoid backslashes: Rscript -e eats them.",
                            "Use a bracket class like [.]R$ instead. Got:", pat))

  # With no backslash to collapse, what R receives is byte-identical to what the
  # README shows, so evaluating the pattern here is faithful to the real run.
  sel <- list.files(repo_path("R"), pattern = pat)
  tracked <- git_tracked()
  if (length(tracked)) {
    want <- basename(grep("^R/[^/]*[.]R$", tracked, value = TRUE))
    expect_setequal(sel, want)
  }
  expect_true(length(sel) > 0, info = "the documented command would source nothing")
})
