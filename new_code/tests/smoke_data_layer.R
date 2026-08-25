# Data-layer smoke test for pseudo_realtime_ESI_vs_PMI.R
#
# Exercises with REAL data (no brms / no RJSDMX — the host R < 4.2 cannot
# install RJSDMX; the Eurostat/ECB keys are identical to the ones already
# proven in the production scripts):
#   1. the OECD country-vintage downloader (live editions, curl path, cache);
#   2. growth/recession computation on the downloaded editions;
#   3. the event-grid builder on the real country archive;
#   4. a synthetic S&P-structure PMI workbook (full readxl path);
#   5. as-of predictor assembly with synthetic CISS/ESI/PMI inputs;
#   6. the 5-year-gap training cutoff logic.
#
# Usage: Rscript --vanilla new_code/tests/smoke_data_layer.R
#        (run from the repo root; downloads into a temp OUTPUT_DIR)

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

# ---- load helper definitions from the main script ---------------------------
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
cat("helpers loaded:", length(ls(env)), "objects\n")

n_fail <- 0L
expect <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("PASS:", label, "\n")
  } else {
    cat("FAIL:", label, "\n")
    n_fail <<- n_fail + 1L
  }
}

# Redirect OUTPUT_DIR and the OECD cache to a temp dir for the smoke test.
smoke_dir <- file.path(tempdir(), "smoke_esi_vs_pmi")
dir.create(smoke_dir, recursive = TRUE, showWarnings = FALSE)
env$OECD_CACHE_FILE <- file.path(smoke_dir, "oecd_gdp_vintages_raw.csv")

###############################################################################
# 1. OECD downloader: live edition through the curl path (rate-limit aware)
###############################################################################

cat("\n== 1. OECD editions download (live) ==\n")
# Tight retry budget for the smoke test: the OECD endpoint rate-limits
# aggressively; we do not want the suite to sleep for minutes.
env$OECD_MAX_ATTEMPTS <- 2L
env$OECD_BACKOFF_START <- 10

ed1 <- try(env$fetch_oecd_edition("202506"), silent = TRUE)
live_ok <- !inherits(ed1, "try-error") && !is.null(ed1) && nrow(ed1) > 0
if (live_ok) {
  expect("edition 202506 downloaded", TRUE)
  expect("edition has REF_AREA", "REF_AREA" %in% names(ed1))
  expect("edition has TIME_PERIOD + OBS_VALUE",
         all(c("TIME_PERIOD", "OBS_VALUE") %in% names(ed1)))
  cat("rows:", nrow(ed1), "| areas:", paste(unique(ed1$REF_AREA), collapse = ","),
      "| quarters:", length(unique(ed1$TIME_PERIOD)), "\n")
  ed2 <- ed1  # one live edition is enough; second comes from the seed below
} else {
  cat("SKIP: live OECD fetch rate-limited; using seed file instead\n")
  # SMOKE_SEED_CSV can point to a previously downloaded edition CSV
  seed_csv <- Sys.getenv("SMOKE_SEED_CSV", "/tmp/ed_probe.csv")
  if (!file.exists(seed_csv)) {
    cat("FAIL: no seed CSV at", seed_csv, "\n")
    n_fail <- n_fail + 1L
    ed1 <- NULL
    ed2 <- NULL
  } else {
    ed1 <- utils::read.csv(seed_csv, stringsAsFactors = FALSE, check.names = FALSE)
    ed2 <- ed1
  }
}

###############################################################################
# 2. Cache-seeded archive + growth/recession processing
###############################################################################

cat("\n== 2. Country vintage archive (cache + processing) ==\n")
# Seed the cache with the downloaded edition; download_country_gdp_vintages()
# then exercises its cache path without further OECD requests.
# Restrict the edition window so nothing beyond the seeded edition is needed.
env$OECD_EDITIONS_FROM <- "202506"
env$OECD_EDITIONS_TO <- "202506"
if (!is.null(ed1)) {
  geo_lookup <- setNames(env$oecd_geo_map$geo, env$oecd_geo_map$oecd_code)
  seed_rows <- ed1 %>%
    transmute(
      geo = unname(geo_lookup[REF_AREA]),
      edition = "202506",
      quarter = as.character(TIME_PERIOD),
      gdp_level = suppressWarnings(as.numeric(OBS_VALUE))
    ) %>%
    filter(!is.na(geo), is.finite(gdp_level))
  utils::write.csv(seed_rows, env$OECD_CACHE_FILE, row.names = FALSE)
}

vintages <- env$download_country_gdp_vintages()
expect("4 countries present", setequal(unique(vintages$geo), c("DE", "FR", "IT", "ES")))
expect("vintage dates are Date", inherits(vintages$vintage_date, "Date"))
expect("cache file written", file.exists(env$OECD_CACHE_FILE))

processed <- env$compute_growth_and_recession(vintages)
expect("growth computed", any(is.finite(processed$gdp_growth)))
expect("recession labels present", all(processed$recession %in% c(0L, 1L) | is.na(processed$recession)))

# Growth sanity: the COVID contraction must be strongly negative everywhere
for (g in c("DE", "FR", "IT", "ES")) {
  covid_g <- processed %>%
    filter(geo == !!g, quarter == zoo::as.yearqtr("2020 Q2"), is.finite(gdp_growth))
  expect(paste0(g, " 2020Q2 growth < -5 (COVID)"), nrow(covid_g) > 0 && all(covid_g$gdp_growth < -5))
}
cat("DE 2020Q2 growth:", round(
  processed %>% filter(geo == "DE", quarter == zoo::as.yearqtr("2020 Q2")) %>% pull(gdp_growth), 2),
  "\n")

###############################################################################
# 3. Event grid on the real country archive
###############################################################################

cat("\n== 3. Event grid ==\n")
# Two editions sharing the same latest quarter (2025 Q3 in 202506, 2025 Q4 in
# 202508): expect exactly one event per geo per new quarter.
grid <- env$build_event_grid(processed)
expect("event grid has rows", nrow(grid) > 0)
expect("event_date is Date", inherits(grid$event_date, "Date"))
dups <- grid %>% count(geo, target_quarter) %>% filter(n > 1)
expect("no duplicate (geo, quarter) events", nrow(dups) == 0)
cat("events:", nrow(grid), "| target quarters:", paste(sort(unique(format(grid$target_quarter))), collapse = " "), "\n")

###############################################################################
# 4. Synthetic PMI workbook in the exact production structure
###############################################################################

cat("\n== 4. PMI workbook (synthetic, production layout) ==\n")
pmi_path <- file.path(smoke_dir, "pmi_servizi_composito.xlsx")
sheets <- c(CompEMU = "EA", CompGER = "DE", CompFRA = "FR",
            CompITA = "IT", CompSPA = "ES")
wb <- openxlsx::createWorkbook()
# month labels in the workbook style: Jan98 ... (capitalised 3 letters + yy)
month_seq <- seq(as.Date("2018-01-01"), to = Sys.Date(), by = "month")
month_labels <- paste0(substr(months(month_seq), 1, 3), format(month_seq, "%y"))
for (sh in names(sheets)) {
  openxlsx::addWorksheet(wb, sh)
  # production layout: rows 1-9 metadata, B5 = series name, data from row 10
  openxlsx::writeData(wb, sh, "S&P GLOBAL PMI: COMPOSITE - OUTPUT",
                      xy = c(2, 5))
  n_m <- length(month_labels)
  set.seed(which(names(sheets) == sh))
  pmi_vals <- round(50 + c(rep(2, 12), rep(-8, 4), rep(5, 8),
                           cumsum(rnorm(n_m - 24, 0, 1.5))), 1)
  openxlsx::writeData(
    wb, sh,
    data.frame(A = month_labels, B = pmi_vals, stringsAsFactors = FALSE),
    xy = c(1, 10), colNames = FALSE
  )
}
openxlsx::saveWorkbook(wb, pmi_path, overwrite = TRUE)
expect("workbook written", file.exists(pmi_path))

pmi <- env$read_pmi_workbook(pmi_path)
expect("PMI: 5 geos", setequal(unique(pmi$geo), c("EA", "DE", "FR", "IT", "ES")))
expect("PMI: monthly coverage", nrow(pmi) >= 5 * 90)
expect("PMI: release dates Date", inherits(pmi$pmi_release_date, "Date"))
expect("PMI: values plausible", all(pmi$pmi_composite > 20 & pmi$pmi_composite < 90))
cat("PMI rows:", nrow(pmi), "| months per geo:", nrow(pmi) / 5, "\n")

###############################################################################
# 5. As-of predictor assembly (synthetic CISS + real-shaped ESI + PMI above)
###############################################################################

cat("\n== 5. As-of predictors ==\n")
ciss_synth <- expand.grid(
  geo = c("EA", "DE", "FR", "IT", "ES"),
  time = seq(as.Date("2018-01-01"), to = Sys.Date(), by = "day"),
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  mutate(ciss_daily = stats::runif(n(), 0.01, 0.4))

esi_synth <- expand.grid(
  geo = c("EA", "DE", "FR", "IT", "ES"),
  month = zoo::as.yearmon(seq(as.Date("2018-01-01"), to = Sys.Date(), by = "month")),
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  mutate(
    esi = 100 + stats::runif(n(), -10, 10),
    esi_release_date = env$release_date_from_fixed_day(month, 29)
  )

as_of <- max(grid$event_date)
pred <- env$build_predictors_as_of(ciss_synth, esi_synth, pmi, as_of)
expect("CISS as-of rows for 5 geos", n_distinct(pred$ciss$geo) == 5)
expect("CISS cut respected", all(
  (pred$ciss %>% filter(geo == "EA"))$quarter <= zoo::as.yearqtr(as_of)
))
expect("ESI latest quarter <= as-of quarter",
       max(pred$esi$quarter) <= zoo::as.yearqtr(as_of))
expect("PMI latest quarter <= as-of quarter",
       max(pred$pmi$quarter) <= zoo::as.yearqtr(as_of))
cat("as-of", format(as_of), "-> ciss rows:", nrow(pred$ciss),
    "| esi rows:", nrow(pred$esi), "| pmi rows:", nrow(pred$pmi), "\n")

###############################################################################
# 6. Gap-design cutoff
###############################################################################

cat("\n== 6. Training gap ==\n")
tq <- zoo::as.yearqtr("2026 Q1")
cut <- env$training_cutoff_quarter(tq)
expect("cutoff = target - 5y", isTRUE(all.equal(cut, zoo::as.yearqtr("2021 Q1"))))

###############################################################################
cat("\n=============================================\n")
if (n_fail == 0L) {
  cat("SMOKE TEST: ALL PASSED\n")
} else {
  cat("SMOKE TEST:", n_fail, "FAILURES\n")
  quit(status = 1)
}
