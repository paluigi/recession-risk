# OECD-independent smoke tests: synthetic PMI workbook, as-of predictor
# assembly, gap cutoff, and the event-grid/backtest plumbing on synthetic
# vintage archives. Runs offline in seconds.
#
# Usage: Rscript --vanilla new_code/tests/test_offline_pipeline.R

repo_root <- normalizePath(
  file.path(getwd(), if (dir.exists(file.path(getwd(), "new_code", "tests"))) "." else "..")
)
script_path <- file.path(repo_root, "new_code", "pseudo_realtime_ESI_vs_PMI.R")
stopifnot(file.exists(script_path))

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(lubridate)
  library(zoo)
})

exprs <- parse(script_path)
env <- new.env(parent = globalenv())
assign("library", function(...) invisible(NULL), envir = env)
assign("install.packages", function(...) invisible(NULL), envir = env)
assign("Sys.which", function(x) "/usr/bin/java", envir = env)
assign("dir.create", function(...) invisible(TRUE), envir = env)
assign("set.seed", function(...) invisible(NULL), envir = env)
for (i in seq_along(exprs)) {
  d0 <- deparse(exprs[[i]][[1]], nlines = 1)[1]
  if (identical(d0, "cat")) break
  eval(exprs[[i]], envir = env)
}

n_fail <- 0L
expect <- function(label, cond) {
  if (isTRUE(cond)) cat("PASS:", label, "\n")
  else { cat("FAIL:", label, "\n"); n_fail <<- n_fail + 1L }
}

smoke_dir <- file.path(tempdir(), "offline_pipeline")
dir.create(smoke_dir, recursive = TRUE, showWarnings = FALSE)

###############################################################################
# A. Synthetic PMI workbook in the exact production layout
###############################################################################

cat("\n== A. PMI workbook ==\n")
pmi_path <- file.path(smoke_dir, "pmi_servizi_composito.xlsx")
sheets <- c(CompEMU = "EA", CompGER = "DE", CompFRA = "FR",
            CompITA = "IT", CompSPA = "ES")

# month labels in the workbook style: Jan98 ... (capitalised 3 letters + yy)
month_seq <- seq(as.Date("2015-01-01"), to = Sys.Date(), by = "month")
month_labels <- paste0(
  substr(months(month_seq), 1, 3),
  format(month_seq, "%y")
)

wb <- openxlsx::createWorkbook()
for (sh in names(sheets)) {
  openxlsx::addWorksheet(wb, sh)
  openxlsx::writeData(wb, sh, "S&P GLOBAL PMI: COMPOSITE - OUTPUT", xy = c(2, 5))
  set.seed(which(names(sheets) == sh))
  n_m <- length(month_labels)
  vals <- round(
    50 + c(rep(3, 30), rep(-10, 4), rep(6, 12), cumsum(rnorm(n_m - 46, 0, 1.5))),
    1
  )
  openxlsx::writeData(
    wb, sh,
    data.frame(A = month_labels, B = vals, stringsAsFactors = FALSE),
    xy = c(1, 10), colNames = FALSE
  )
}
openxlsx::saveWorkbook(wb, pmi_path, overwrite = TRUE)
expect("workbook written", file.exists(pmi_path))

pmi <- env$read_pmi_workbook(pmi_path)
expect("PMI 5 geos", setequal(unique(pmi$geo), c("EA", "DE", "FR", "IT", "ES")))
expect("PMI full coverage", nrow(pmi) >= 5 * 100)
expect("PMI release Date", inherits(pmi$pmi_release_date, "Date"))
expect("PMI values plausible", all(pmi$pmi_composite > 20 & pmi$pmi_composite < 90))
# spot-check month parse: Jan15 -> 2015-01
jan15 <- pmi %>% filter(geo == "EA", month == zoo::as.yearmon("2015-01"))
expect("Jan15 parsed to 2015-01", nrow(jan15) == 1)

# wrong series name must fail
wb_bad <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb_bad, "CompEMU")
openxlsx::writeData(wb_bad, "CompEMU", "WRONG SERIES NAME", xy = c(2, 5))
openxlsx::writeData(wb_bad, "CompEMU",
                    data.frame(A = "Jan20", B = 50), xy = c(1, 10), colNames = FALSE)
bad_path <- file.path(smoke_dir, "pmi_bad.xlsx")
openxlsx::saveWorkbook(wb_bad, bad_path, overwrite = TRUE)
expect("bad series name rejected",
       inherits(try(env$read_pmi_workbook(bad_path), silent = TRUE), "try-error"))

###############################################################################
# B. As-of predictor assembly
###############################################################################

cat("\n== B. As-of predictors ==\n")
ciss_synth <- expand.grid(
  geo = c("EA", "DE", "FR", "IT", "ES"),
  time = seq(as.Date("2015-01-01"), to = Sys.Date(), by = "day"),
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  mutate(ciss_daily = stats::runif(n(), 0.01, 0.4))

esi_synth <- expand.grid(
  geo = c("EA", "DE", "FR", "IT", "ES"),
  month = zoo::as.yearmon(month_seq),
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  mutate(
    esi = 100 + stats::runif(n(), -10, 10),
    esi_release_date = env$release_date_from_fixed_day(month, 29)
  )

as_of <- as.Date("2026-04-30")
pred <- env$build_predictors_as_of(ciss_synth, esi_synth, pmi, as_of)

expect("CISS 5 geos", n_distinct(pred$ciss$geo) == 5)
expect("CISS no future quarters", max(pred$ciss$quarter) <= zoo::as.yearqtr(as_of))
expect("CISS current quarter partial allowed",
       zoo::as.yearqtr(as_of) %in% pred$ciss$quarter)
expect("ESI no future quarters", max(pred$esi$quarter) <= zoo::as.yearqtr(as_of))
expect("PMI no future quarters", max(pred$pmi$quarter) <= zoo::as.yearqtr(as_of))
# complete-quarter rule: ESI for the as-of quarter (2026 Q2) must be absent
# (April month published day 29 -> only 1 month of the quarter available)
expect("ESI partial quarter excluded",
       !(zoo::as.yearqtr("2026 Q2") %in% pred$esi$quarter))
expect("PMI partial quarter excluded",
       !(zoo::as.yearqtr("2026 Q2") %in% pred$pmi$quarter))

###############################################################################
# C. Event grid + gap cutoff on a synthetic multi-vintage archive
###############################################################################

cat("\n== C. Event grid and gap ==\n")
mk_lvls <- function(base, n_q) {
  # smooth levels with a COVID-like dip and a mild recession (pad/truncate
  # the growth path to n_q)
  g <- c(rep(0.3, 16), -0.4, -9.0, 2.5, rep(0.4, 12), -0.5, -0.6, 0.3, rep(0.35, 40))
  g <- g[seq_len(n_q)]
  out <- numeric(n_q)
  lv <- base
  for (i in seq_len(n_q)) {
    lv <- lv * (1 + g[i] / 100)
    out[i] <- lv
  }
  out
}
qs <- zoo::as.yearqtr(seq(as.Date("2015-01-01"), by = "quarter", length.out = 44))
editions <- seq(as.Date("2020-01-01"), to = as.Date("2026-09-01"), by = "month")
# small revision noise per edition
set.seed(7)
synth <- bind_rows(lapply(editions, function(ed) {
  n_avail <- sum(qs <= zoo::as.yearqtr(ed) - 0.25 * 3)  # ~3-quarter lag
  if (n_avail < 4) return(NULL)
  tibble(
    geo = rep(c("DE", "FR", "IT", "ES"), each = n_avail),
    vintage_date = ed,
    quarter = rep(qs[seq_len(n_avail)], 4),
    gdp_level = as.vector(sapply(
      c(300, 250, 200, 150),
      function(base) mk_lvls(base, n_avail) * (1 + stats::rnorm(1, 0, 0.001))
    ))
  )
}))
proc_synth <- env$compute_growth_and_recession(synth)
grid_synth <- env$build_event_grid(proc_synth)
expect("synthetic grid has events", nrow(grid_synth) > 20)
expect("grid event_date is Date", inherits(grid_synth$event_date, "Date"))
dups <- grid_synth %>% count(geo, target_quarter) %>% filter(n > 1)
expect("no duplicate events", nrow(dups) == 0)

# gap cutoff
expect("gap cutoff 5y",
       isTRUE(all.equal(
         env$training_cutoff_quarter(zoo::as.yearqtr("2026 Q1")),
         zoo::as.yearqtr("2021 Q1")
       )))

# gdp_panel_as_of uses the latest edition <= date
p_a <- env$gdp_panel_as_of(proc_synth, as.Date("2023-06-15"))
ed_le <- max(editions[editions <= as.Date("2023-06-15")])
p_ref <- proc_synth %>% filter(vintage_date == ed_le)
expect("panel = latest edition panel", nrow(p_a) == nrow(p_ref))

###############################################################################
cat("\n=============================================\n")
if (n_fail == 0L) {
  cat("OFFLINE PIPELINE TEST: ALL PASSED\n")
  quit(status = 0)
} else {
  cat("OFFLINE PIPELINE TEST:", n_fail, "FAILURES\n")
  quit(status = 1)
}
