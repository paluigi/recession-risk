# Dashboard data layer: fit the hierarchical Bayesian logit on the latest
# data and persist the numbers the site dashboard renders.
#
# DESIGN (as agreed for the continuously-updating tracker)
#   * Train on data up to (max_quarter - DASHBOARD_GAP_YEARS) — the same
#     5-year-gap design as the production scripts — and infer on everything.
#   * Rows are tagged: "in_sample" (quarter <= cutoff), "out_of_sample"
#     (held-out quarters that already have a GDP label) and "nowcast"
#     (quarters after the last published GDP quarter whose three months of
#     monthly predictor data are all published — the nowcast window; these
#     rows have no GDP label and extend to the latest available data).
#   * No forward projections: the last row per geo is the nowcast.
#   * Scaling parameters are computed on the TRAINING window only (no
#     look-ahead), unlike the production scripts which scale on the full
#     sample; this matches the pseudo-real-time backtest.
#
# DATA SOURCES (identical to new_code/recessione_CISS_ESI.R)
#   GDP  Eurostat namq_10_gdp/Q.CLV_PCH_PRE.SCA.B1GQ.<geo>   (QoQ % change)
#   CISS ECB      CISS/D.<geo>.Z0Z.4F.EC.SS_CIN.IDX          (daily -> quarterly)
#   ESI  Eurostat ei_bssi_m_r2/M.BS-ESI-I.SA.<geo>           (monthly -> quarterly)
#   PMI  S&P workbook (production only), read via new_code/pmi_io.R
#
# USAGE
#   DASHBOARD_PREDICTOR=esi Rscript --vanilla new_code/update_dashboard_data.R
#   DASHBOARD_PREDICTOR=pmi Rscript --vanilla new_code/update_dashboard_data.R
#
#   Run on a host with RJSDMX/brms (production). Outputs land in
#   dashboard/data/ and are committed so the Quarto site can render from
#   artifacts without re-downloading or re-fitting.

# --- rJava / Java 16+ compatibility (MUST precede library(RJSDMX)) ----------
options(java.parameters = c(
  getOption("java.parameters"),
  "--add-opens=java.base/java.util=ALL-UNNAMED"
))

###############################################################################
# 0. PACKAGES
###############################################################################

required_packages <- c(
  "RJSDMX",
  "dplyr",
  "tibble",
  "lubridate",
  "brms",
  "zoo",
  "readxl"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(RJSDMX)
library(dplyr)
library(tibble)
library(lubridate)
library(brms)
library(zoo)

if (Sys.which("java") == "") {
  stop("Java is not available in the PATH. RJSDMX requires Java 8 or later.")
}

###############################################################################
# 1. SETTINGS
###############################################################################

# Predictor arm: "esi" (public data, default) or "pmi" (production only).
dashboard_predictor <- tolower(Sys.getenv("DASHBOARD_PREDICTOR", "esi"))
if (!dashboard_predictor %in% c("esi", "pmi")) {
  stop("DASHBOARD_PREDICTOR must be 'esi' or 'pmi', got: ",
       dashboard_predictor)
}

areas <- c("EA", "DE", "FR", "IT", "ES")

gdp_geo_map <- tibble(
  eurostat_code = c("EA", "DE", "FR", "IT", "ES"),
  geo           = c("EA", "DE", "FR", "IT", "ES")
)

esi_geo_map <- tibble(
  eurostat_code = c("EA21", "DE", "FR", "IT", "ES"),
  geo           = c("EA",   "DE", "FR", "IT", "ES")
)

ciss_geo_map <- tibble(
  ecb_code = c("U2", "DE", "FR", "IT", "ES"),
  geo      = c("EA", "DE", "FR", "IT", "ES")
)

# Gap design: train up to (max_quarter - DASHBOARD_GAP_YEARS) — identical to
# the production cutoff_date <- max_date - 5.
DASHBOARD_GAP_YEARS <- 5

# Bayesian controls (production specification).
BRMS_CHAINS <- 2L
BRMS_CORES <- 2L
BRMS_ITER <- 2000L
BRMS_SEED <- 123

# PMI workbook locations (production first, then a local test copy) — same
# paths as recessione_CISS_PMI_composito.R / pseudo_realtime_ESI_vs_PMI.R.
network_pmi_file <- "//home/group/main/891ac/private/ac/PMI/pmi_servizi_composito.xlsx"
local_pmi_file <- file.path(getwd(), "pmi_servizi_composito.xlsx")

###############################################################################
# 2. PATHS
###############################################################################

# Resolve the repo root: prefer the directory of the invoked script
# (update_dashboard_data.R lives in new_code/), fall back to the working
# directory. The first candidate containing dashboard/ wins.
find_repo_root <- function(candidates) {
  for (cand in candidates) {
    if (nzchar(cand) && dir.exists(file.path(cand, "dashboard"))) {
      return(normalizePath(cand))
    }
  }
  stop(
    "Could not locate the repository root (no dashboard/ directory found). ",
    "Tried: ", paste(candidates, collapse = ", ")
  )
}

repo_root_candidates <- c()
file_args <- commandArgs(trailingOnly = FALSE)[
  grepl("^--file=", commandArgs(trailingOnly = FALSE))
]
if (length(file_args) > 0) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_args[[1]])))
  repo_root_candidates <- c(repo_root_candidates, dirname(script_dir))
}
repo_root_candidates <- c(repo_root_candidates, getwd(), dirname(getwd()))

REPO_ROOT <- find_repo_root(repo_root_candidates)

OUTPUT_DIR <- file.path(REPO_ROOT, "dashboard", "data")
MODEL_CACHE_DIR <- file.path(OUTPUT_DIR, "brms_cache")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MODEL_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

###############################################################################
# 3. SDMX HELPERS (verbatim from new_code/recessione_CISS_ESI.R)
###############################################################################

normalize_sdmx_table <- function(x, label) {
  if (isTRUE(attr(x, "IS_ERROR"))) {
    error_objects <- attr(x, "ERROR_OBJECTS")
    stop(
      paste0(
        "RJSDMX error while downloading ", label, ".\n",
        paste(error_objects, collapse = "\n")
      )
    )
  }

  if (!is.data.frame(x)) {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
  }

  names(x) <- names(x) |>
    gsub("[^A-Za-z0-9]+", "_", x = _) |>
    tolower()

  x <- as_tibble(x)

  required_columns <- c("time_period", "obs_value")
  missing_columns <- setdiff(required_columns, names(x))

  if (length(missing_columns) > 0) {
    stop(
      paste0(
        label, " is missing expected columns: ",
        paste(missing_columns, collapse = ", "),
        ". Available columns: ",
        paste(names(x), collapse = ", ")
      )
    )
  }

  x
}

download_sdmx_table <- function(provider, id, label,
                                start = "", end = "",
                                gregorian_time = FALSE,
                                fail_if_empty = TRUE) {
  raw <- tryCatch(
    RJSDMX::getTimeSeriesTable(
      provider = provider,
      id = id,
      start = start,
      end = end,
      gregorianTime = gregorian_time
    ),
    error = function(e) {
      stop(
        paste0(
          "Download failed for ", label, ".\n",
          "Provider: ", provider, "\n",
          "Key: ", id, "\n",
          "Message: ", conditionMessage(e)
        )
      )
    }
  )

  result <- normalize_sdmx_table(raw, label)

  if (fail_if_empty && nrow(result) == 0) {
    stop(
      paste0(
        "The query returned no observations for ", label, ".\n",
        "Key: ", id
      )
    )
  }

  result
}

# Parses SDMX quarterly labels: "2025-Q3", "2025Q3", or gregorian dates.
parse_sdmx_quarter <- function(x) {
  x <- trimws(as.character(x))
  normalized <- rep(NA_character_, length(x))

  q_idx <- grepl("Q[1-4]$", x, ignore.case = TRUE)
  if (any(q_idx)) {
    normalized[q_idx] <- gsub("-?Q", " Q", x[q_idx], ignore.case = TRUE)
  }

  g_idx <- !q_idx & nzchar(x)
  if (any(g_idx)) {
    normalized[g_idx] <- format(as.yearqtr(as.Date(x[g_idx])))
  }

  as.yearqtr(normalized, format = "%Y Q%q")
}

# Parses SDMX monthly labels: "2026-04" or gregorian dates.
parse_sdmx_month <- function(x) {
  x <- trimws(as.character(x))
  normalized <- rep(NA_character_, length(x))

  short_idx <- nchar(x) == 7L
  if (any(short_idx)) {
    normalized[short_idx] <- x[short_idx]
  }

  long_idx <- !short_idx & nzchar(x)
  if (any(long_idx)) {
    normalized[long_idx] <- format(as.yearmon(as.Date(x[long_idx])))
  }

  as.yearmon(normalized, format = "%Y-%m")
}

###############################################################################
# 4. DASHBOARD HELPERS (pure; unit-tested in new_code/tests/)
###############################################################################

# Gap-design cutoff: training quarters satisfy quarter <= cutoff.
dashboard_cutoff_quarter <- function(max_quarter,
                                     gap_years = DASHBOARD_GAP_YEARS) {
  max_quarter - gap_years
}

# Which (geo, quarter) cells of a monthly series have all three months
# published? Used to gate the nowcast extension: a quarter after the last
# published GDP quarter enters the panel only when its monthly predictor is
# complete (same rule as the pseudo-real-time backtest).
complete_quarter_flags <- function(monthly) {
  monthly %>%
    group_by(geo, quarter) %>%
    summarise(n_months = n_distinct(month), .groups = "drop") %>%
    mutate(complete = n_months >= 3L)
}

# Segment tagging for the dashboard plot.
assign_segments <- function(rows, cutoff_quarter) {
  bad <- rows$quarter <= cutoff_quarter & !rows$has_gdp
  if (any(bad)) {
    stop("assign_segments: training-window rows without GDP label at ",
         format(rows$quarter[which(bad)[1]]))
  }
  rows %>%
    mutate(
      segment = case_when(
        quarter <= cutoff_quarter ~ "in_sample",
        has_gdp ~ "out_of_sample",
        TRUE ~ "nowcast"
      )
    )
}

# Training-window scaling parameters (no look-ahead), in the spirit of the
# backtest's safe_scale_parameters.
scale_parameters <- function(x, label) {
  x <- x[is.finite(x)]
  if (length(x) < 2) {
    stop("Not enough finite observations to scale ", label, ".")
  }
  sd_x <- sd(x)
  if (!is.finite(sd_x) || sd_x == 0) {
    stop("Zero variance in ", label, "; cannot standardize.")
  }
  list(mean = mean(x), sd = sd_x)
}

validate_probabilities_csv <- function(rows) {
  required <- c(
    "predictor", "geo", "quarter", "prob_recession",
    "lower_95", "upper_95", "segment", "recession_actual"
  )
  missing_columns <- setdiff(required, names(rows))
  if (length(missing_columns) > 0) {
    stop("probabilities CSV missing columns: ",
         paste(missing_columns, collapse = ", "))
  }
  allowed_segments <- c("in_sample", "out_of_sample", "nowcast")
  if (!all(rows$segment %in% allowed_segments)) {
    stop("probabilities CSV has unexpected segment values: ",
         paste(setdiff(unique(rows$segment), allowed_segments),
               collapse = ", "))
  }
  probs <- c(rows$prob_recession, rows$lower_95, rows$upper_95)
  probs <- probs[!is.na(probs)]
  if (any(probs < 0 | probs > 1)) {
    stop("probabilities CSV has probabilities outside [0, 1].")
  }
  if (any(rows$lower_95 > rows$prob_recession + 1e-12,
          rows$upper_95 < rows$prob_recession - 1e-12,
          na.rm = TRUE)) {
    stop("probabilities CSV has intervals not containing the point estimate.")
  }
  parsed_quarters <- as.yearqtr(rows$quarter, format = "%Y-Q%q")
  if (any(is.na(parsed_quarters))) {
    stop("probabilities CSV has unparsable quarter labels.")
  }
  TRUE
}

###############################################################################
# 5. DOWNLOADS (RJSDMX; same keys as the production scripts)
###############################################################################

available_providers <- RJSDMX::getProviders()
required_providers <- c("EUROSTAT", "ECB")
missing_providers <- setdiff(required_providers, available_providers)
if (length(missing_providers) > 0) {
  stop(
    "Required RJSDMX providers not available: ",
    paste(missing_providers, collapse = ", ")
  )
}

read_gdp_series <- function(eurostat_code, output_geo) {
  query_id <- paste0("namq_10_gdp/Q.CLV_PCH_PRE.SCA.B1GQ.", eurostat_code)
  raw <- download_sdmx_table(
    provider = "EUROSTAT",
    id = query_id,
    label = paste0("GDP Eurostat - ", eurostat_code)
  )
  raw %>%
    transmute(
      geo = output_geo,
      quarter = parse_sdmx_quarter(time_period),
      gdp_growth = suppressWarnings(as.numeric(obs_value))
    ) %>%
    filter(!is.na(quarter), is.finite(gdp_growth)) %>%
    group_by(geo, quarter) %>%
    summarise(gdp_growth = mean(gdp_growth, na.rm = TRUE), .groups = "drop")
}

read_ciss_series <- function(ecb_code, output_geo) {
  query_id <- paste0("CISS/D.", ecb_code, ".Z0Z.4F.EC.SS_CIN.IDX")
  raw <- download_sdmx_table(
    provider = "ECB",
    id = query_id,
    label = paste0("CISS ECB - ", ecb_code),
    start = "1998-01-01",
    gregorian_time = TRUE
  )
  raw %>%
    transmute(
      geo = output_geo,
      time = as.Date(time_period),
      ciss_daily = suppressWarnings(as.numeric(obs_value))
    ) %>%
    filter(!is.na(time), is.finite(ciss_daily))
}

read_esi_series <- function(eurostat_code, output_geo) {
  query_id <- paste0("ei_bssi_m_r2/M.BS-ESI-I.SA.", eurostat_code)
  raw <- download_sdmx_table(
    provider = "EUROSTAT",
    id = query_id,
    label = paste0("ESI Eurostat - ", eurostat_code),
    gregorian_time = TRUE
  )
  raw %>%
    transmute(
      geo = output_geo,
      month = parse_sdmx_month(time_period),
      esi = suppressWarnings(as.numeric(obs_value))
    ) %>%
    filter(!is.na(month), is.finite(esi)) %>%
    group_by(geo, month) %>%
    summarise(esi = mean(esi, na.rm = TRUE), .groups = "drop")
}

###############################################################################
# 6. EXECUTION
###############################################################################

cat("\n=== Downloading GDP ===\n")
gdp <- bind_rows(
  lapply(
    seq_len(nrow(gdp_geo_map)),
    function(i) {
      read_gdp_series(
        eurostat_code = gdp_geo_map$eurostat_code[i],
        output_geo = gdp_geo_map$geo[i]
      )
    }
  )
) %>%
  arrange(geo, quarter)

cat("\n=== Downloading CISS ===\n")
ciss <- bind_rows(
  lapply(
    seq_len(nrow(ciss_geo_map)),
    function(i) {
      read_ciss_series(
        ecb_code = ciss_geo_map$ecb_code[i],
        output_geo = ciss_geo_map$geo[i]
      )
    }
  )
) %>%
  mutate(quarter = as.yearqtr(time)) %>%
  group_by(geo, quarter) %>%
  summarise(ciss = mean(ciss_daily, na.rm = TRUE), .groups = "drop") %>%
  filter(is.finite(ciss)) %>%
  arrange(geo, quarter)

cat("\n=== Downloading ", toupper(dashboard_predictor), " ===\n", sep = "")
if (dashboard_predictor == "esi") {
  sentiment_monthly <- bind_rows(
    lapply(
      seq_len(nrow(esi_geo_map)),
      function(i) {
        read_esi_series(
          eurostat_code = esi_geo_map$eurostat_code[i],
          output_geo = esi_geo_map$geo[i]
        )
      }
    )
  ) %>%
    arrange(geo, month)
  sentiment_col <- "esi"
} else {
  source(file.path(REPO_ROOT, "new_code", "pmi_io.R"), local = TRUE)
  pmi_file <- resolve_pmi_file(network_pmi_file, local_pmi_file)
  cat("PMI workbook: ", pmi_file, "\n", sep = "")
  sentiment_monthly <- read_pmi_workbook(pmi_file) %>%
    rename(pmi = pmi_composite) %>%
    select(geo, month, pmi)
  sentiment_col <- "pmi"
}

sentiment_quarterly <- sentiment_monthly %>%
  mutate(quarter = as.yearqtr(month)) %>%
  group_by(geo, quarter) %>%
  summarise(
    sentiment = mean(.data[[sentiment_col]], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(is.finite(sentiment)) %>%
  arrange(geo, quarter)

# GDP labels: technical recession = two consecutive negative QoQ quarters.
gdp_labelled <- gdp %>%
  group_by(geo) %>%
  mutate(recession = as.integer(gdp_growth < 0 & lag(gdp_growth) < 0)) %>%
  ungroup()

max_gdp_quarter <- gdp_labelled %>%
  group_by(geo) %>%
  summarise(max_gdp_quarter = max(quarter), .groups = "drop")

# Historical rows: quarters with GDP label + both predictors.
historical <- gdp_labelled %>%
  inner_join(ciss, by = c("geo", "quarter")) %>%
  inner_join(sentiment_quarterly, by = c("geo", "quarter")) %>%
  filter(!is.na(recession)) %>%
  mutate(has_gdp = TRUE)

# Nowcast extension: quarters AFTER the last published GDP quarter with both
# predictors present and a complete monthly indicator (3 of 3 months).
complete_flags <- sentiment_monthly %>%
  mutate(quarter = as.yearqtr(month)) %>%
  complete_quarter_flags()

nowcast <- ciss %>%
  inner_join(sentiment_quarterly, by = c("geo", "quarter")) %>%
  inner_join(complete_flags, by = c("geo", "quarter")) %>%
  inner_join(max_gdp_quarter, by = "geo") %>%
  filter(complete, quarter > max_gdp_quarter) %>%
  transmute(geo, quarter, ciss, sentiment, recession = NA_real_, has_gdp = FALSE)

panel <- bind_rows(
  historical %>%
    transmute(geo, quarter, ciss, sentiment, recession, has_gdp),
  nowcast
) %>%
  arrange(geo, quarter) %>%
  # Explicit factor levels keep brms' grouping consistent between the
  # training data and the prediction newdata.
  mutate(geo = factor(geo, levels = areas))

max_quarter <- max(panel$quarter)
cutoff_quarter <- dashboard_cutoff_quarter(max_quarter)

cat("\nPanel coverage:\n")
print(
  panel %>%
    group_by(geo) %>%
    summarise(
      first_quarter = min(quarter),
      last_quarter = max(quarter),
      rows = n(),
      nowcast_rows = sum(!has_gdp),
      .groups = "drop"
    )
)
cat("Cutoff quarter (train <= cutoff):", format(cutoff_quarter), "\n")

training <- panel %>%
  filter(has_gdp, quarter <= cutoff_quarter, !is.na(recession))

ciss_scale <- scale_parameters(training$ciss, "CISS")
sentiment_scale <- scale_parameters(training$sentiment,
                                    toupper(dashboard_predictor))

panel_scaled <- panel %>%
  mutate(
    ciss_scaled = (ciss - ciss_scale$mean) / ciss_scale$sd,
    sentiment_scaled = (sentiment - sentiment_scale$mean) / sentiment_scale$sd
  )

training_scaled <- panel_scaled %>%
  filter(geo %in% training$geo, quarter <= cutoff_quarter, has_gdp,
         !is.na(recession))

cat("\n=== Fitting hierarchical Bayesian logit ===\n")
model_formula <- brmsformula(
  recession ~ ciss_scaled + sentiment_scaled +
    (1 + ciss_scaled + sentiment_scaled | geo),
  family = bernoulli(link = "logit")
)

brms_model <- brm(
  formula = model_formula,
  data = training_scaled,
  prior = c(
    prior(normal(0, 5), class = "Intercept"),
    prior(normal(0, 2), class = "b")
  ),
  chains = BRMS_CHAINS,
  cores = BRMS_CORES,
  iter = BRMS_ITER,
  seed = BRMS_SEED,
  silent = 2,
  refresh = 0,
  file = file.path(MODEL_CACHE_DIR, paste0("dashboard_", dashboard_predictor, "_fit")),
  file_refit = "on_change"
)

cat("\n=== Predicting all rows ===\n")
predictions <- fitted(brms_model, newdata = panel_scaled, type = "response")

results <- panel_scaled %>%
  transmute(
    predictor = dashboard_predictor,
    geo,
    quarter,
    prob_recession = as.numeric(predictions[, "Estimate"]),
    lower_95 = as.numeric(predictions[, "Q2.5"]),
    upper_95 = as.numeric(predictions[, "Q97.5"]),
    has_gdp,
    recession_actual = recession
  ) %>%
  assign_segments(cutoff_quarter = cutoff_quarter)

# gdp_growth is needed by the site for recession shading; attach the actual
# label as 0/1/NA (NA for nowcast rows).
results <- results %>%
  mutate(
    recession_actual = ifelse(has_gdp, recession_actual, NA_real_),
    quarter_label = zoo::format.yearqtr(quarter, format = "%Y-Q%q")
  ) %>%
  select(-quarter) %>%
  rename(quarter = quarter_label)

validate_probabilities_csv(results)

probabilities_file <- file.path(
  OUTPUT_DIR, paste0("probabilities_", dashboard_predictor, ".csv")
)
write.csv(
  results,
  probabilities_file,
  row.names = FALSE,
  na = ""
)

meta <- tibble(
  predictor = dashboard_predictor,
  fitted_at_utc = format(
    as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%d %H:%M:%S UTC"
  ),
  cutoff_quarter = zoo::format.yearqtr(cutoff_quarter, format = "%Y-Q%q"),
  max_panel_quarter = zoo::format.yearqtr(max_quarter, format = "%Y-Q%q"),
  training_rows = nrow(training_scaled),
  training_recessions = sum(training_scaled$recession == 1L),
  scaling = "training-window (no look-ahead)",
  r_version = R.version.string,
  brms_version = as.character(packageVersion("brms"))
)
write.csv(
  meta,
  file.path(OUTPUT_DIR, paste0("meta_", dashboard_predictor, ".csv")),
  row.names = FALSE,
  na = ""
)

capture.output(
  summary(brms_model),
  file = file.path(
    OUTPUT_DIR, paste0("model_summary_", dashboard_predictor, ".txt")
  )
)

# Tidy population-level effects (consumed by short-note.qmd).
fixef(brms_model) |>
  as.data.frame() |>
  tibble::rownames_to_column("parameter") |>
  write.csv(
    file.path(OUTPUT_DIR, paste0("fixed_effects_", dashboard_predictor, ".csv")),
    row.names = FALSE
  )
capture.output(
  sessionInfo(),
  file = file.path(OUTPUT_DIR, "session_info.txt")
)

cat("\nDashboard data written to ", OUTPUT_DIR, ":\n", sep = "")
cat("  - ", basename(probabilities_file), "\n", sep = "")
cat("  - meta_", dashboard_predictor, ".csv\n", sep = "")
cat("  - model_summary_", dashboard_predictor, ".txt\n", sep = "")
cat("Done.\n")
