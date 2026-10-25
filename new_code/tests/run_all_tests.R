# Run all new_code tests: unit tests (offline), offline pipeline tests
# (synthetic data, incl. a synthetic PMI workbook), and — only when OECD
# requests are currently allowed — the live data-layer smoke test.
#
# Usage:
#   Rscript --vanilla new_code/tests/run_all_tests.R            # offline only
#   Rscript --vanilla new_code/tests/run_all_tests.R --with-live # + OECD live
#
# Exit status is non-zero if any included suite fails.

args <- commandArgs(trailingOnly = TRUE)
with_live <- "--with-live" %in% args

repo_root <- normalizePath(
  file.path(getwd(), if (dir.exists(file.path(getwd(), "new_code", "tests"))) "." else "..")
)
tests_dir <- file.path(repo_root, "new_code", "tests")

suites <- c(
  unit = file.path(tests_dir, "test_pseudo_realtime_helpers.R"),
  offline_pipeline = file.path(tests_dir, "test_offline_pipeline.R")
)
if (with_live) {
  suites <- c(suites, live_smoke = file.path(tests_dir, "smoke_data_layer.R"))
}

# The suites run with --vanilla, so they never read the project .Rprofile and
# renv never activates for them. Locate the project renv library for this R
# version (renv/library/<platform>/R-<major.minor>/<arch>) and hand it to the
# child processes via R_LIBS; on hosts where packages live in the default
# library this resolves to nothing and behaviour is unchanged.
test_child_libs <- function() {
  candidates <- list.dirs(file.path(repo_root, "renv", "library"), recursive = TRUE)
  leaves <- candidates[grepl("/R-[0-9]+\\.[0-9]+/[^/]+$", candidates)]
  want <- sprintf("R-%s.%s", R.version$major, sub("\\..*$", "", R.version$minor))
  matched <- leaves[grepl(paste0("/", want, "/[^/]+$"), leaves)]
  if (length(matched) == 0) return(NULL)
  paste0("R_LIBS=", matched[[1]])
}

failed <- character()
for (nm in names(suites)) {
  cat("\n>>>>>>>>>>", nm, "<<<<<<<<<<\n")
  status <- system2(
    "Rscript", c("--vanilla", shQuote(suites[[nm]])),
    stdout = "", stderr = "",
    env = test_child_libs()
  )
  if (status != 0) failed <- c(failed, nm)
  cat(">>>>>>>>>>", nm, ifelse(status == 0, "OK", paste("FAILED (exit", status, ")")), "<<<<<<<<<<\n")
}

cat("\n=============================================\n")
if (length(failed) == 0) {
  cat("ALL SUITES PASSED\n")
  quit(status = 0)
} else {
  cat("FAILED SUITES:", paste(failed, collapse = ", "), "\n")
  quit(status = 1)
}
