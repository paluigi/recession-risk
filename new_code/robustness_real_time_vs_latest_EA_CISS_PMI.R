# Euro Area recession risk: real-time versus latest-vintage history.
#
# The exercise is limited to the Euro Area and uses:
#   1. Eurostat quarterly GDP vintages (ei_na_q_vtg)
#   2. ECB daily CISS for the Euro Area
#   3. S&P Global Euro Area Composite PMI - Output from the Excel workbook
#
# For each selected GDP revision date (REVDATE), the script:
#   - reconstructs GDP growth and the technical-recession label using that
#     exact GDP vintage;
#   - cuts CISS observations at the revision date;
#   - makes each monthly PMI observation available only from the 26th of its
#     reference month, implementing the assumption that PMI is normally
#     released after the 25th;
#   - builds quarterly CISS and PMI predictors using only information available
#     by the revision date;
#   - estimates an expanding-window Bayesian logit on quarters strictly before
#     the target quarter;
#   - predicts the recession probability for the latest GDP quarter available
#     in that vintage.
#
# The script then constructs a second, retrospective series in the style of
# Furno and Giannone:
#   - "Real time" is the expanding-window probability produced at the first GDP
#     release for each quarter, with GDP labels and predictors available then;
#   - "Latest vintage" is an in-sample probability obtained by estimating one
#     Bayesian logit on the latest GDP vintage and the latest complete history of
#     CISS and PMI.
#
# The gap between the two series therefore combines the effect of GDP revisions
# and the effect of estimating the latest-vintage model on the full sample.
# This mirrors the in-sample versus pseudo-out-of-sample comparison shown by
# Furno and Giannone, adapted here to quarterly technical-recession labels.
#
# Default backtest design: first GDP release observed for each new quarter.
# Set BACKTEST_SCOPE <- "all_vintages" to retain every revision-date run in the
# detailed output. The plotted real-time history still uses the first release of
# each target quarter so that there is exactly one probability per quarter.

# --- rJava / Java 16+ compatibility (must precede library(RJSDMX)) ----------
# Restart R first if rJava/RJSDMX has already been loaded in the session.
options(java.parameters = unique(c(
  getOption("java.parameters"),
  "--add-opens=java.base/java.util=ALL-UNNAMED"
)))

###############################################################################
# 0. PACKAGES
###############################################################################

required_packages <- c(
  "RJSDMX",
  "dplyr",
  "tidyr",
  "tibble",
  "lubridate",
  "ggplot2",
  "patchwork",
  "brms",
  "zoo",
  "scales",
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
library(tidyr)
library(tibble)
library(lubridate)
library(ggplot2)
library(patchwork)
library(brms)
library(zoo)
library(scales)
library(readxl)

if (Sys.which("java") == "") {
  stop("Java is not available in PATH. RJSDMX requires Java 8 or later.")
}

###############################################################################
# 1. USER SETTINGS
###############################################################################

# PMI workbook.
network_pmi_file <- c('//home/group/main/891ac/private/ac/PMI/pmi_servizi_composito.xlsx')

local_pmi_file <- file.path(getwd(), "pmi_servizi_composito.xlsx")

if (file.exists(network_pmi_file)) {
  pmi_file <- network_pmi_file
} else if (file.exists(local_pmi_file)) {
  pmi_file <- local_pmi_file
} else {
  stop(
    paste0(
      "PMI workbook not found. Checked:\n",
      "  1. ", network_pmi_file, "\n",
      "  2. ", local_pmi_file
    )
  )
}

# The Euro Area composite PMI is in this sheet.
PMI_SHEET <- "CompEMU"

# Availability assumption requested by the user:
# an observation for month m becomes usable on day 26 of month m.
PMI_RELEASE_DAY <- 26L

# CISS is cut at REVDATE. Increase this to 1 if a conservative one-day
# publication lag is preferred.
CISS_RELEASE_LAG_DAYS <- 0L

# Require all three monthly PMI observations to construct a quarterly mean.
# This prevents a partial-quarter PMI average from being treated as a full
# quarterly predictor.
REQUIRE_COMPLETE_PMI_QUARTER <- TRUE

# "first_release" estimates one model for the earliest vintage containing each
# new GDP quarter. "all_vintages" estimates one model for every revision date.
BACKTEST_SCOPE <- "first_release"

# Minimum amount of usable expanding-window history.
MIN_TRAINING_OBS <- 40L
MIN_RECESSION_EVENTS <- 2L

# Bayesian model controls. Repeated vintage fits are expensive, so the script
# caches fitted models by revision date and target quarter.
BRMS_CHAINS <- 2L
detected_cores <- parallel::detectCores(logical = TRUE)
BRMS_CORES <- if (is.na(detected_cores)) 1L else min(2L, detected_cores)
BRMS_ITER <- 2000L
BRMS_WARMUP <- 1000L
BRMS_ADAPT_DELTA <- 0.95
BRMS_MAX_TREEDEPTH <- 12L
BRMS_BACKEND <- NULL  # Set to "cmdstanr" if configured and preferred.
CACHE_MODELS <- TRUE

OUTPUT_DIR <- file.path(getwd(), "real_time_vs_latest_EA_output")
MODEL_CACHE_DIR <- file.path(OUTPUT_DIR, "brms_cache")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MODEL_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

set.seed(123)

###############################################################################
# 2. GENERIC HELPERS
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

resolve_column <- function(x, candidates, label) {
  found <- candidates[candidates %in% names(x)]

  if (length(found) == 0) {
    stop(
      paste0(
        "Cannot identify ", label, ". Tried: ",
        paste(candidates, collapse = ", "),
        ". Available columns: ",
        paste(names(x), collapse = ", ")
      )
    )
  }

  found[1]
}

parse_sdmx_quarter <- function(x) {
  x <- trimws(as.character(x))
  normalized <- rep(NA_character_, length(x))

  q_idx <- grepl("Q[1-4]$", x, ignore.case = TRUE)
  if (any(q_idx)) {
    normalized[q_idx] <- gsub("-?Q", " Q", x[q_idx], ignore.case = TRUE)
  }

  date_idx <- !q_idx & nzchar(x)
  if (any(date_idx)) {
    normalized[date_idx] <- format(as.yearqtr(as.Date(x[date_idx])))
  }

  as.yearqtr(normalized, format = "%Y Q%q")
}

is_consecutive_quarter <- function(current_quarter, previous_quarter) {
  gap <- 4 * (as.numeric(current_quarter) - as.numeric(previous_quarter))
  !is.na(gap) & abs(gap - 1) < 1e-8
}

safe_scale_parameters <- function(x, label) {
  mu <- mean(x, na.rm = TRUE)
  sigma <- stats::sd(x, na.rm = TRUE)

  if (!is.finite(mu) || !is.finite(sigma) || sigma <= 0) {
    stop(paste0("Cannot standardize ", label, ": invalid mean or standard deviation."))
  }

  list(mean = mu, sd = sigma)
}

clamp_probability <- function(x, eps = 1e-6) {
  pmin(pmax(x, eps), 1 - eps)
}

binary_auc <- function(y, p) {
  ok <- is.finite(y) & is.finite(p)
  y <- y[ok]
  p <- p[ok]

  if (length(unique(y)) < 2) {
    return(NA_real_)
  }

  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  ranks <- rank(p, ties.method = "average")

  (sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

score_probabilities <- function(data, outcome_column, label) {
  y <- data[[outcome_column]]
  p <- data$prob_recession
  ok <- is.finite(y) & is.finite(p)
  y <- y[ok]
  p <- clamp_probability(p[ok])

  if (length(y) == 0) {
    return(
      tibble(
        comparison = label,
        observations = 0L,
        recession_rate = NA_real_,
        brier_score = NA_real_,
        log_score = NA_real_,
        auc = NA_real_
      )
    )
  }

  tibble(
    comparison = label,
    observations = length(y),
    recession_rate = mean(y),
    brier_score = mean((p - y)^2),
    log_score = -mean(y * log(p) + (1 - y) * log(1 - p)),
    auc = binary_auc(y, p)
  )
}

###############################################################################
# 3. PMI HELPERS
###############################################################################

parse_pmi_month <- function(x) {
  x <- toupper(trimws(as.character(x)))

  month_codes <- c(
    "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
    "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
  )

  month_number <- match(substr(x, 1, 3), month_codes)
  two_digit_year <- suppressWarnings(as.integer(substr(x, 4, 5)))

  full_year <- ifelse(
    two_digit_year >= 70,
    1900L + two_digit_year,
    2000L + two_digit_year
  )

  valid <- !is.na(month_number) & !is.na(two_digit_year)
  normalized <- rep(NA_character_, length(x))
  normalized[valid] <- sprintf(
    "%04d-%02d",
    as.integer(full_year[valid]),
    as.integer(month_number[valid])
  )

  as.yearmon(normalized, format = "%Y-%m")
}

read_ea_pmi <- function(path, sheet) {
  available_sheets <- readxl::excel_sheets(path)

  if (!sheet %in% available_sheets) {
    stop(
      paste0(
        "PMI sheet ", sheet, " not found. Available sheets: ",
        paste(available_sheets, collapse = ", ")
      )
    )
  }

  series_name <- readxl::read_excel(
    path = path,
    sheet = sheet,
    range = "B5:B5",
    col_names = FALSE,
    col_types = "text"
  )[[1]][1]

  if (
    is.na(series_name) ||
      !grepl("PMI: COMPOSITE - OUTPUT", series_name, ignore.case = TRUE)
  ) {
    stop(
      paste0(
        "Unexpected series in column B of sheet ", sheet, ".\n",
        "Expected: S&P GLOBAL PMI: COMPOSITE - OUTPUT\n",
        "Found: ", series_name
      )
    )
  }

  raw <- readxl::read_excel(
    path = path,
    sheet = sheet,
    skip = 9,
    col_names = FALSE,
    guess_max = 10000,
    .name_repair = "unique"
  )

  if (ncol(raw) < 2) {
    stop(paste0("Sheet ", sheet, " does not contain columns A and B."))
  }

  raw <- raw[, 1:2]
  names(raw) <- c("month_label", "pmi_composite")

  parsed <- raw %>%
    transmute(
      month_label = trimws(as.character(month_label)),
      month = parse_pmi_month(month_label),
      pmi_composite = suppressWarnings(as.numeric(pmi_composite))
    )

  bad_dates <- parsed %>%
    filter(
      !is.na(pmi_composite),
      !is.na(month_label),
      nzchar(month_label),
      is.na(month)
    )

  if (nrow(bad_dates) > 0) {
    stop(
      paste0(
        "Unrecognized month labels in sheet ", sheet, ": ",
        paste(head(unique(bad_dates$month_label), 10), collapse = ", ")
      )
    )
  }

  result <- parsed %>%
    filter(!is.na(month), is.finite(pmi_composite)) %>%
    group_by(month) %>%
    summarise(
      pmi_composite = mean(pmi_composite, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      month_start = as.Date(month, frac = 0),
      pmi_release_date = make_date(
        year(month_start),
        month(month_start),
        PMI_RELEASE_DAY
      ),
      quarter = as.yearqtr(month)
    ) %>%
    arrange(month)

  if (nrow(result) == 0) {
    stop("No usable Euro Area composite-PMI observations were found.")
  }

  result
}

###############################################################################
# 4. PROVIDER CHECK
###############################################################################

available_providers <- RJSDMX::getProviders()
required_providers <- c("EUROSTAT", "ECB")
missing_providers <- setdiff(required_providers, available_providers)

if (length(missing_providers) > 0) {
  stop(
    paste0(
      "Required RJSDMX providers not available: ",
      paste(missing_providers, collapse = ", ")
    )
  )
}

###############################################################################
# 5. DOWNLOAD EURO AREA GDP VINTAGES
###############################################################################

# Dataset dimensions:
#   FREQ.REVDATE.S_ADJ.UNIT.GEO
# Empty REVDATE means all revision dates.
#   Q          = quarterly
#   SCA        = seasonally and calendar adjusted
#   CLV05_MEUR = chain-linked volumes (2005), million euro
#   EA         = changing-composition Euro Area aggregate
vintage_query_id <- "ei_na_q_vtg/Q..SCA.CLV05_MEUR.EA"

raw_gdp_vintages <- download_sdmx_table(
  provider = "EUROSTAT",
  id = vintage_query_id,
  label = "Euro Area quarterly GDP vintages"
)

revdate_column <- resolve_column(
  raw_gdp_vintages,
  candidates = c("revdate", "rev_date", "revision_date"),
  label = "GDP vintage revision-date column"
)

gdp_vintages <- raw_gdp_vintages %>%
  transmute(
    vintage_date = as.Date(.data[[revdate_column]]),
    quarter = parse_sdmx_quarter(time_period),
    gdp_level = suppressWarnings(as.numeric(obs_value))
  ) %>%
  filter(
    !is.na(vintage_date),
    !is.na(quarter),
    is.finite(gdp_level)
  ) %>%
  group_by(vintage_date, quarter) %>%
  summarise(gdp_level = mean(gdp_level, na.rm = TRUE), .groups = "drop") %>%
  group_by(vintage_date) %>%
  arrange(quarter, .by_group = TRUE) %>%
  mutate(
    previous_quarter = lag(quarter),
    consecutive_level = is_consecutive_quarter(quarter, previous_quarter),
    gdp_growth = if_else(
      consecutive_level,
      100 * (gdp_level / lag(gdp_level) - 1),
      NA_real_
    ),
    previous_growth = lag(gdp_growth),
    consecutive_growth = is_consecutive_quarter(quarter, lag(quarter)),
    recession = case_when(
      is.na(gdp_growth) | is.na(previous_growth) ~ NA_integer_,
      consecutive_growth & gdp_growth < 0 & previous_growth < 0 ~ 1L,
      consecutive_growth ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-previous_quarter, -consecutive_level, -previous_growth,
         -consecutive_growth) %>%
  ungroup() %>%
  arrange(vintage_date, quarter)

if (nrow(gdp_vintages) == 0) {
  stop("No usable GDP-vintage observations were downloaded.")
}

cat("\nGDP vintage coverage:\n")
print(
  gdp_vintages %>%
    summarise(
      first_vintage = min(vintage_date),
      last_vintage = max(vintage_date),
      vintage_dates = n_distinct(vintage_date),
      first_quarter = min(quarter),
      last_quarter = max(quarter)
    )
)

###############################################################################
# 6. DOWNLOAD EURO AREA DAILY CISS
###############################################################################

raw_ciss <- download_sdmx_table(
  provider = "ECB",
  id = "CISS/D.U2.Z0Z.4F.EC.SS_CIN.IDX",
  label = "Euro Area CISS",
  start = "1998-01-01",
  gregorian_time = TRUE
)

ciss_daily <- raw_ciss %>%
  transmute(
    time = as.Date(time_period),
    ciss_daily = suppressWarnings(as.numeric(obs_value))
  ) %>%
  filter(!is.na(time), is.finite(ciss_daily)) %>%
  group_by(time) %>%
  summarise(ciss_daily = mean(ciss_daily, na.rm = TRUE), .groups = "drop") %>%
  mutate(quarter = as.yearqtr(time)) %>%
  arrange(time)

if (nrow(ciss_daily) == 0) {
  stop("No usable Euro Area CISS observations were downloaded.")
}

###############################################################################
# 7. READ EURO AREA COMPOSITE PMI
###############################################################################

pmi_monthly <- read_ea_pmi(pmi_file, PMI_SHEET)

cat("\nPMI availability assumption:\n")
cat(
  "Each monthly observation becomes available on day ",
  PMI_RELEASE_DAY,
  " of its reference month.\n",
  sep = ""
)
cat(
  "PMI coverage: ",
  format(min(pmi_monthly$month), "%b %Y"),
  " to ",
  format(max(pmi_monthly$month), "%b %Y"),
  " (", nrow(pmi_monthly), " months).\n",
  sep = ""
)

###############################################################################
# 8. BUILD THE VINTAGE CALENDAR
###############################################################################

vintage_calendar <- gdp_vintages %>%
  filter(is.finite(gdp_growth), !is.na(recession)) %>%
  group_by(vintage_date) %>%
  summarise(
    target_quarter = max(quarter),
    target_gdp_growth = gdp_growth[which.max(quarter)],
    target_recession = recession[which.max(quarter)],
    .groups = "drop"
  ) %>%
  mutate(
    quarter_end = as.Date(target_quarter, frac = 1),
    days_after_quarter_end = as.integer(vintage_date - quarter_end)
  ) %>%
  arrange(vintage_date)

if (nrow(vintage_calendar) < 2) {
  stop("The vintage archive contains too few usable revision dates.")
}

# The latest quarter already present in the first archived vintage cannot be
# evaluated as a genuine first release because its earlier vintages are absent.
initial_vintage_date <- min(vintage_calendar$vintage_date)
initial_archive_quarter <- vintage_calendar %>%
  filter(vintage_date == initial_vintage_date) %>%
  pull(target_quarter) %>%
  max()

vintage_calendar <- vintage_calendar %>%
  filter(target_quarter > initial_archive_quarter) %>%
  group_by(target_quarter) %>%
  arrange(vintage_date, .by_group = TRUE) %>%
  mutate(release_round = row_number()) %>%
  ungroup()

if (BACKTEST_SCOPE == "first_release") {
  selected_vintages <- vintage_calendar %>%
    group_by(target_quarter) %>%
    slice_min(vintage_date, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(vintage_date)
} else if (BACKTEST_SCOPE == "all_vintages") {
  selected_vintages <- vintage_calendar %>%
    arrange(vintage_date)
} else {
  stop("BACKTEST_SCOPE must be either 'first_release' or 'all_vintages'.")
}

if (nrow(selected_vintages) == 0) {
  stop("No GDP vintages remain after applying the backtest selection.")
}

cat("\nPseudo-real-time design:\n")
cat("Scope: ", BACKTEST_SCOPE, "\n", sep = "")
cat("Selected model runs: ", nrow(selected_vintages), "\n", sep = "")
cat(
  "Target quarters: ",
  format(min(selected_vintages$target_quarter), "%Y Q%q"),
  " to ",
  format(max(selected_vintages$target_quarter), "%Y Q%q"),
  "\n",
  sep = ""
)

###############################################################################
# 9. AS-OF PREDICTORS
###############################################################################

build_predictors_as_of <- function(vintage_date) {
  ciss_cutoff <- vintage_date - days(CISS_RELEASE_LAG_DAYS)

  ciss_as_of <- ciss_daily %>%
    filter(time <= ciss_cutoff) %>%
    group_by(quarter) %>%
    summarise(
      ciss = mean(ciss_daily, na.rm = TRUE),
      ciss_days_available = n(),
      ciss_last_date = max(time),
      .groups = "drop"
    ) %>%
    filter(is.finite(ciss))

  pmi_as_of <- pmi_monthly %>%
    filter(pmi_release_date <= vintage_date) %>%
    group_by(quarter) %>%
    summarise(
      pmi_composite = mean(pmi_composite, na.rm = TRUE),
      pmi_months_available = n_distinct(month),
      pmi_last_release_date = max(pmi_release_date),
      .groups = "drop"
    ) %>%
    filter(is.finite(pmi_composite))

  if (REQUIRE_COMPLETE_PMI_QUARTER) {
    pmi_as_of <- pmi_as_of %>%
      filter(pmi_months_available == 3L)
  }

  inner_join(ciss_as_of, pmi_as_of, by = "quarter") %>%
    arrange(quarter)
}

###############################################################################
# 10. ONE PSEUDO-REAL-TIME MODEL RUN
###############################################################################

fit_one_vintage <- function(as_of_date, target_quarter, release_round) {
  cat(
    "\n[", format(as_of_date), "] Target ",
    format(target_quarter, "%Y Q%q"),
    " | release round ", release_round, "\n",
    sep = ""
  )

  gdp_as_of <- gdp_vintages %>%
    filter(.data$vintage_date == .env$as_of_date) %>%
    select(quarter, gdp_level, gdp_growth, recession)

  x_as_of <- build_predictors_as_of(as_of_date)

  model_data <- gdp_as_of %>%
    inner_join(x_as_of, by = "quarter") %>%
    filter(
      !is.na(recession),
      is.finite(gdp_growth),
      is.finite(ciss),
      is.finite(pmi_composite)
    ) %>%
    arrange(quarter)

  training_data <- model_data %>%
    filter(quarter < target_quarter)

  target_data <- model_data %>%
    filter(quarter == target_quarter)

  if (nrow(target_data) != 1) {
    stop(
      paste0(
        "Target predictor row is missing or duplicated for ",
        format(target_quarter, "%Y Q%q"),
        " at vintage ", format(as_of_date), "."
      )
    )
  }

  if (nrow(training_data) < MIN_TRAINING_OBS) {
    stop(
      paste0(
        "Only ", nrow(training_data), " training observations are available; ",
        MIN_TRAINING_OBS, " are required."
      )
    )
  }

  recession_events <- sum(training_data$recession == 1L)
  non_recession_events <- sum(training_data$recession == 0L)

  if (
    recession_events < MIN_RECESSION_EVENTS ||
      non_recession_events < MIN_RECESSION_EVENTS
  ) {
    stop(
      paste0(
        "Insufficient outcome variation: ", recession_events,
        " recession observations and ", non_recession_events,
        " non-recession observations."
      )
    )
  }

  # Standardization is estimated only on information used to fit this vintage.
  ciss_scale <- safe_scale_parameters(training_data$ciss, "CISS")
  pmi_scale <- safe_scale_parameters(training_data$pmi_composite, "PMI")

  training_scaled <- training_data %>%
    mutate(
      ciss_scaled = (ciss - ciss_scale$mean) / ciss_scale$sd,
      pmi_scaled = (pmi_composite - pmi_scale$mean) / pmi_scale$sd
    )

  target_scaled <- target_data %>%
    mutate(
      ciss_scaled = (ciss - ciss_scale$mean) / ciss_scale$sd,
      pmi_scaled = (pmi_composite - pmi_scale$mean) / pmi_scale$sd
    )

  model_formula <- brmsformula(
    recession ~ ciss_scaled + pmi_scaled,
    family = bernoulli(link = "logit")
  )

  model_file_stub <- file.path(
    MODEL_CACHE_DIR,
    paste0(
      "ea_pseudo_rt_",
      format(as_of_date, "%Y%m%d"),
      "_",
      gsub(" ", "", format(target_quarter, "%YQ%q"))
    )
  )

  brm_arguments <- list(
    formula = model_formula,
    data = training_scaled,
    prior = c(
      prior(normal(0, 5), class = "Intercept"),
      prior(normal(0, 2), class = "b")
    ),
    chains = BRMS_CHAINS,
    cores = BRMS_CORES,
    iter = BRMS_ITER,
    warmup = BRMS_WARMUP,
    seed = 123,
    silent = 2,
    refresh = 0,
    control = list(
      adapt_delta = BRMS_ADAPT_DELTA,
      max_treedepth = BRMS_MAX_TREEDEPTH
    )
  )

  if (!is.null(BRMS_BACKEND)) {
    brm_arguments$backend <- BRMS_BACKEND
  }

  if (CACHE_MODELS) {
    brm_arguments$file <- model_file_stub
    brm_arguments$file_refit <- "on_change"
  }

  fitted_model <- do.call(brm, brm_arguments)

  probability_draws <- as.numeric(
    posterior_epred(fitted_model, newdata = target_scaled)
  )

  result <- target_scaled %>%
    transmute(
      vintage_date = as_of_date,
      target_quarter = quarter,
      release_round = as.integer(release_round),
      days_after_quarter_end = as.integer(
        as_of_date - as.Date(quarter, frac = 1)
      ),
      gdp_growth_vintage = gdp_growth,
      recession_vintage = recession,
      ciss_as_of = ciss,
      pmi_as_of = pmi_composite,
      ciss_last_date,
      pmi_last_release_date,
      pmi_months_available,
      training_observations = nrow(training_scaled),
      training_recessions = recession_events,
      ciss_training_mean = ciss_scale$mean,
      ciss_training_sd = ciss_scale$sd,
      pmi_training_mean = pmi_scale$mean,
      pmi_training_sd = pmi_scale$sd,
      prob_recession = mean(probability_draws),
      lower_95 = unname(quantile(probability_draws, 0.025)),
      upper_95 = unname(quantile(probability_draws, 0.975)),
      status = "ok",
      error_message = NA_character_
    )

  if (!CACHE_MODELS) {
    rm(fitted_model)
    invisible(gc())
  }

  result
}

###############################################################################
# 11. RUN THE BACKTEST
###############################################################################

backtest_rows <- vector("list", nrow(selected_vintages))

for (i in seq_len(nrow(selected_vintages))) {
  current_vintage <- selected_vintages$vintage_date[i]
  current_target <- selected_vintages$target_quarter[i]
  current_round <- selected_vintages$release_round[i]

  backtest_rows[[i]] <- tryCatch(
    fit_one_vintage(
      as_of_date = current_vintage,
      target_quarter = current_target,
      release_round = current_round
    ),
    error = function(e) {
      warning(
        paste0(
          "Backtest run failed for vintage ", current_vintage,
          " and target ", format(current_target, "%Y Q%q"),
          ": ", conditionMessage(e)
        ),
        call. = FALSE
      )

      tibble(
        vintage_date = current_vintage,
        target_quarter = current_target,
        release_round = as.integer(current_round),
        days_after_quarter_end = as.integer(
          current_vintage - as.Date(current_target, frac = 1)
        ),
        gdp_growth_vintage = NA_real_,
        recession_vintage = NA_integer_,
        ciss_as_of = NA_real_,
        pmi_as_of = NA_real_,
        ciss_last_date = as.Date(NA),
        pmi_last_release_date = as.Date(NA),
        pmi_months_available = NA_integer_,
        training_observations = NA_integer_,
        training_recessions = NA_integer_,
        ciss_training_mean = NA_real_,
        ciss_training_sd = NA_real_,
        pmi_training_mean = NA_real_,
        pmi_training_sd = NA_real_,
        prob_recession = NA_real_,
        lower_95 = NA_real_,
        upper_95 = NA_real_,
        status = "failed",
        error_message = conditionMessage(e)
      )
    }
  )
}

backtest_all <- bind_rows(backtest_rows)
backtest_results <- backtest_all %>%
  filter(status == "ok", is.finite(prob_recession))

if (nrow(backtest_results) == 0) {
  failed_export <- backtest_all %>%
    mutate(target_quarter = format(target_quarter, "%Y-Q%q"))

  write.csv(
    failed_export,
    file.path(OUTPUT_DIR, "pseudo_real_time_all_runs.csv"),
    row.names = FALSE,
    na = ""
  )

  stop(
    paste0(
      "All pseudo-real-time model runs failed. Inspect: ",
      file.path(OUTPUT_DIR, "pseudo_real_time_all_runs.csv")
    )
  )
}

###############################################################################
# 12. FIT THE LATEST-VINTAGE IN-SAMPLE SERIES
###############################################################################

# "Latest vintage" is defined as follows:
#   1. take the most recent GDP vintage available in ei_na_q_vtg;
#   2. join it to the latest complete quarterly CISS and PMI histories;
#   3. estimate one Bayesian logit on the full usable sample;
#   4. calculate fitted recession probabilities for every usable quarter.
#
# This is the quarterly analogue of the in-sample line overlaid on the expanding-
# window pseudo-real-time line in Furno and Giannone. It is intentionally
# retrospective: future observations can affect the estimated coefficients, but
# never the quarter-specific CISS or PMI value.

latest_vintage_date <- max(gdp_vintages$vintage_date)

latest_gdp_snapshot <- gdp_vintages %>%
  filter(vintage_date == latest_vintage_date) %>%
  select(
    quarter,
    gdp_level_latest = gdp_level,
    gdp_growth_latest = gdp_growth,
    recession_latest = recession
  ) %>%
  arrange(quarter)

# Use an information date after the last observation in every input source so
# build_predictors_as_of() returns the current complete historical predictors.
latest_information_date <- max(
  c(
    latest_vintage_date,
    max(ciss_daily$time, na.rm = TRUE),
    max(pmi_monthly$pmi_release_date, na.rm = TRUE)
  ),
  na.rm = TRUE
)

latest_predictors <- build_predictors_as_of(latest_information_date)

latest_model_data <- latest_gdp_snapshot %>%
  inner_join(latest_predictors, by = "quarter") %>%
  filter(
    !is.na(recession_latest),
    is.finite(gdp_growth_latest),
    is.finite(ciss),
    is.finite(pmi_composite)
  ) %>%
  arrange(quarter)

if (nrow(latest_model_data) < MIN_TRAINING_OBS) {
  stop(
    paste0(
      "Only ", nrow(latest_model_data),
      " usable observations are available for the latest-vintage model; ",
      MIN_TRAINING_OBS, " are required."
    )
  )
}

latest_recession_events <- sum(latest_model_data$recession_latest == 1L)
latest_non_recession_events <- sum(latest_model_data$recession_latest == 0L)

if (
  latest_recession_events < MIN_RECESSION_EVENTS ||
    latest_non_recession_events < MIN_RECESSION_EVENTS
) {
  stop(
    paste0(
      "Insufficient outcome variation in the latest GDP vintage: ",
      latest_recession_events, " recession observations and ",
      latest_non_recession_events, " non-recession observations."
    )
  )
}

# The latest-vintage line is standardized on its own full estimation sample.
latest_ciss_scale <- safe_scale_parameters(latest_model_data$ciss, "latest CISS")
latest_pmi_scale <- safe_scale_parameters(
  latest_model_data$pmi_composite,
  "latest PMI"
)

latest_model_scaled <- latest_model_data %>%
  mutate(
    ciss_scaled = (ciss - latest_ciss_scale$mean) / latest_ciss_scale$sd,
    pmi_scaled = (pmi_composite - latest_pmi_scale$mean) / latest_pmi_scale$sd
  )

latest_formula <- brmsformula(
  recession_latest ~ ciss_scaled + pmi_scaled,
  family = bernoulli(link = "logit")
)

latest_model_file_stub <- file.path(
  MODEL_CACHE_DIR,
  paste0(
    "ea_latest_vintage_full_sample_",
    format(latest_vintage_date, "%Y%m%d")
  )
)

latest_brm_arguments <- list(
  formula = latest_formula,
  data = latest_model_scaled,
  prior = c(
    prior(normal(0, 5), class = "Intercept"),
    prior(normal(0, 2), class = "b")
  ),
  chains = BRMS_CHAINS,
  cores = BRMS_CORES,
  iter = BRMS_ITER,
  warmup = BRMS_WARMUP,
  seed = 123,
  silent = 2,
  refresh = 0,
  control = list(
    adapt_delta = BRMS_ADAPT_DELTA,
    max_treedepth = BRMS_MAX_TREEDEPTH
  )
)

if (!is.null(BRMS_BACKEND)) {
  latest_brm_arguments$backend <- BRMS_BACKEND
}

if (CACHE_MODELS) {
  latest_brm_arguments$file <- latest_model_file_stub
  latest_brm_arguments$file_refit <- "on_change"
}

cat(
  "\nFitting latest-vintage full-sample model (GDP vintage ",
  format(latest_vintage_date),
  ")...\n",
  sep = ""
)

latest_model <- do.call(brm, latest_brm_arguments)

latest_draws <- posterior_epred(
  latest_model,
  newdata = latest_model_scaled
)

# posterior_epred() is normally draws x observations. The extra dimension is
# removed defensively in case brms returns draws x observations x response.
if (length(dim(latest_draws)) == 3L) {
  latest_draws <- latest_draws[, , 1L, drop = TRUE]
}

if (is.null(dim(latest_draws))) {
  latest_draws <- matrix(latest_draws, ncol = 1L)
}

latest_probability_summary <- tibble(
  prob_recession_latest = colMeans(latest_draws),
  lower_95_latest = apply(
    latest_draws,
    2L,
    quantile,
    probs = 0.025,
    names = FALSE
  ),
  upper_95_latest = apply(
    latest_draws,
    2L,
    quantile,
    probs = 0.975,
    names = FALSE
  )
)

latest_series <- bind_cols(
  latest_model_scaled %>%
    transmute(
      quarter,
      latest_vintage_date = latest_vintage_date,
      gdp_growth_latest,
      recession_latest,
      ciss_latest = ciss,
      pmi_latest = pmi_composite,
      ciss_last_date_latest = ciss_last_date,
      pmi_last_release_date_latest = pmi_last_release_date,
      pmi_months_available_latest = pmi_months_available
    ),
  latest_probability_summary
) %>%
  arrange(quarter)

###############################################################################
# 13. BUILD THE REAL-TIME AND LATEST-VINTAGE COMPARISON
###############################################################################

# Even when BACKTEST_SCOPE == "all_vintages", the historical real-time line uses
# the first successful release for each target quarter. This prevents multiple
# probabilities for the same x-axis date and matches the interpretation of an
# estimate that an analyst could first have produced in real time.
real_time_series <- backtest_results %>%
  group_by(target_quarter) %>%
  arrange(vintage_date, release_round, .by_group = TRUE) %>%
  slice_head(n = 1L) %>%
  ungroup() %>%
  transmute(
    quarter = target_quarter,
    real_time_vintage_date = vintage_date,
    real_time_release_round = release_round,
    days_after_quarter_end,
    gdp_growth_real_time = gdp_growth_vintage,
    recession_real_time = recession_vintage,
    ciss_real_time = ciss_as_of,
    pmi_real_time = pmi_as_of,
    ciss_last_date_real_time = ciss_last_date,
    pmi_last_release_date_real_time = pmi_last_release_date,
    pmi_months_available_real_time = pmi_months_available,
    training_observations_real_time = training_observations,
    training_recessions_real_time = training_recessions,
    prob_recession_real_time = prob_recession,
    lower_95_real_time = lower_95,
    upper_95_real_time = upper_95
  ) %>%
  arrange(quarter)

comparison_series <- latest_series %>%
  left_join(real_time_series, by = "quarter") %>%
  mutate(
    growth_revision = gdp_growth_latest - gdp_growth_real_time,
    classification_revised = case_when(
      is.na(recession_latest) | is.na(recession_real_time) ~ NA,
      recession_latest != recession_real_time ~ TRUE,
      TRUE ~ FALSE
    ),
    probability_revision =
      prob_recession_latest - prob_recession_real_time,
    absolute_probability_revision = abs(probability_revision)
  ) %>%
  arrange(quarter)

common_comparison <- comparison_series %>%
  filter(
    is.finite(prob_recession_latest),
    is.finite(prob_recession_real_time)
  )

if (nrow(common_comparison) == 0) {
  stop(
    "There are no common quarters between the real-time and latest-vintage series."
  )
}

max_gap_row <- common_comparison %>%
  slice_max(absolute_probability_revision, n = 1L, with_ties = FALSE)

comparison_statistics <- tibble(
  latest_gdp_vintage = latest_vintage_date,
  observations_in_common = nrow(common_comparison),
  first_common_quarter = format(
    min(common_comparison$quarter),
    "%Y-Q%q"
  ),
  last_common_quarter = format(
    max(common_comparison$quarter),
    "%Y-Q%q"
  ),
  correlation = cor(
    common_comparison$prob_recession_latest,
    common_comparison$prob_recession_real_time
  ),
  mean_probability_revision = mean(
    common_comparison$probability_revision
  ),
  mean_absolute_probability_revision = mean(
    common_comparison$absolute_probability_revision
  ),
  root_mean_squared_probability_revision = sqrt(
    mean(common_comparison$probability_revision^2)
  ),
  maximum_absolute_probability_revision =
    max_gap_row$absolute_probability_revision,
  maximum_revision_quarter = format(max_gap_row$quarter, "%Y-Q%q"),
  latest_probability_at_maximum_revision =
    max_gap_row$prob_recession_latest,
  real_time_probability_at_maximum_revision =
    max_gap_row$prob_recession_real_time
)

scores <- bind_rows(
  score_probabilities(
    real_time_series %>%
      rename(
        prob_recession = prob_recession_real_time,
        recession_vintage = recession_real_time
      ),
    outcome_column = "recession_vintage",
    label = "Real-time probability vs contemporaneous GDP-vintage outcome"
  ),
  score_probabilities(
    common_comparison %>%
      transmute(
        prob_recession = prob_recession_real_time,
        recession_latest
      ),
    outcome_column = "recession_latest",
    label = "Real-time probability vs latest-vintage outcome"
  ),
  score_probabilities(
    latest_series %>%
      transmute(
        prob_recession = prob_recession_latest,
        recession_latest
      ),
    outcome_column = "recession_latest",
    label = "Latest-vintage in-sample probability vs latest-vintage outcome"
  ),
  score_probabilities(
    common_comparison %>%
      transmute(
        prob_recession = prob_recession_latest,
        recession_latest
      ),
    outcome_column = "recession_latest",
    label = "Latest-vintage probability on the common real-time sample"
  )
)

cat("\nReal-time versus latest-vintage comparison:\n")
print(comparison_statistics)

cat("\nProbability score summary:\n")
print(scores)

cat("\nGDP classification revisions on common quarters:\n")
print(
  common_comparison %>%
    count(classification_revised, name = "observations")
)

###############################################################################
# 14. EXPORT TABLES AND OBJECTS
###############################################################################

backtest_export <- backtest_results %>%
  mutate(target_quarter = format(target_quarter, "%Y-Q%q"))

all_runs_export <- backtest_all %>%
  mutate(target_quarter = format(target_quarter, "%Y-Q%q"))

vintage_calendar_export <- vintage_calendar %>%
  mutate(target_quarter = format(target_quarter, "%Y-Q%q"))

real_time_export <- real_time_series %>%
  mutate(quarter = format(quarter, "%Y-Q%q"))

latest_export <- latest_series %>%
  mutate(quarter = format(quarter, "%Y-Q%q"))

comparison_export <- comparison_series %>%
  mutate(quarter = format(quarter, "%Y-Q%q"))

write.csv(
  real_time_export,
  file.path(OUTPUT_DIR, "recession_probability_real_time.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  latest_export,
  file.path(OUTPUT_DIR, "recession_probability_latest_vintage.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  comparison_export,
  file.path(OUTPUT_DIR, "recession_probability_comparison.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  backtest_export,
  file.path(OUTPUT_DIR, "pseudo_real_time_results_all_selected_runs.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  all_runs_export,
  file.path(OUTPUT_DIR, "pseudo_real_time_all_runs_including_failures.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  scores,
  file.path(OUTPUT_DIR, "real_time_vs_latest_scores.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  comparison_statistics,
  file.path(OUTPUT_DIR, "real_time_vs_latest_summary_statistics.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  vintage_calendar_export,
  file.path(OUTPUT_DIR, "gdp_vintage_calendar.csv"),
  row.names = FALSE,
  na = ""
)

saveRDS(
  list(
    real_time_series = real_time_series,
    latest_series = latest_series,
    comparison_series = comparison_series,
    scores = scores,
    comparison_statistics = comparison_statistics,
    latest_vintage_date = latest_vintage_date,
    latest_information_date = latest_information_date
  ),
  file.path(OUTPUT_DIR, "real_time_vs_latest_results.rds")
)

capture.output(
  summary(latest_model),
  file = file.path(OUTPUT_DIR, "latest_vintage_model_summary.txt")
)

capture.output(
  sessionInfo(),
  file = file.path(OUTPUT_DIR, "session_info.txt")
)

###############################################################################
# 15. VISUALIZATION: FURNO-STYLE OVERLAY
###############################################################################

latest_plot_data <- latest_series %>%
  transmute(
    quarter,
    date = as.Date(quarter),
    series = "Latest vintage (full-sample)",
    probability = prob_recession_latest,
    lower_95 = lower_95_latest,
    upper_95 = upper_95_latest
  )

real_time_plot_data <- real_time_series %>%
  transmute(
    quarter,
    date = as.Date(quarter),
    series = "Real time (first release)",
    probability = prob_recession_real_time,
    lower_95 = lower_95_real_time,
    upper_95 = upper_95_real_time
  )

probability_plot_data <- bind_rows(
  latest_plot_data,
  real_time_plot_data
) %>%
  mutate(
    series = factor(
      series,
      levels = c(
        "Latest vintage (full-sample)",
        "Real time (first release)"
      )
    )
  )

recession_rectangles <- latest_gdp_snapshot %>%
  filter(
    recession_latest == 1L,
    quarter %in% latest_series$quarter
  ) %>%
  transmute(
    xmin = as.Date(quarter),
    xmax = as.Date(quarter + 0.25) - 1,
    ymin = 0,
    ymax = 1
  )

series_colours <- c(
  "Latest vintage (full-sample)" = "#0072B2",
  "Real time (first release)" = "#0072B2"
)

series_fills <- c(
  "Latest vintage (full-sample)" = "#56B4E9",
  "Real time (first release)" = "#0072B2"
)

series_linetypes <- c(
  "Latest vintage (full-sample)" = "solid",
  "Real time (first release)" = "dashed"
)

probability_plot <- ggplot() +
  geom_rect(
    data = recession_rectangles,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "grey80",
    alpha = 0.55
  ) +
  geom_ribbon(
    data = probability_plot_data,
    aes(
      x = date,
      ymin = lower_95,
      ymax = upper_95,
      fill = series,
      group = series
    ),
    alpha = 0.13,
    colour = NA
  ) +
  geom_line(
    data = probability_plot_data,
    aes(
      x = date,
      y = probability,
      colour = series,
      linetype = series,
      group = series
    ),
    linewidth = 0.95
  ) +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  scale_colour_manual(values = series_colours) +
  scale_fill_manual(values = series_fills) +
  scale_linetype_manual(values = series_linetypes) +
  labs(
    title = "Euro Area recession probability: latest vintage and real time",
    subtitle = paste0(
      "Latest: full-sample fit using GDP vintage ",
      latest_vintage_date,
      ". Real time: expanding-window fit at each first GDP release; ",
      "PMI available from day ", PMI_RELEASE_DAY, "."
    ),
    x = NULL,
    y = "Recession probability",
    colour = NULL,
    fill = NULL,
    linetype = NULL,
    caption = paste0(
      "Grey bars identify technical recessions in the latest GDP vintage. ",
      "Bands are 95% Bayesian credible intervals."
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  ) +
  guides(
    fill = "none",
    colour = guide_legend(override.aes = list(linewidth = 1.1))
  )

probability_revision_plot <- ggplot(
  common_comparison,
  aes(
    x = as.Date(quarter),
    y = probability_revision
  )
) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line(linewidth = 0.8) +
  geom_point(
    aes(shape = classification_revised),
    size = 1.8
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  labs(
    title = "Revision in estimated recession probability",
    subtitle = "Latest-vintage probability minus first-release real-time probability",
    x = NULL,
    y = "Probability revision",
    shape = "GDP recession label revised"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

final_plot <- probability_plot / probability_revision_plot +
  plot_layout(heights = c(1.6, 0.8))

print(final_plot)

ggsave(
  filename = file.path(
    OUTPUT_DIR,
    "recession_probability_real_time_vs_latest.png"
  ),
  plot = final_plot,
  width = 13,
  height = 9,
  dpi = 300
)

ggsave(
  filename = file.path(
    OUTPUT_DIR,
    "recession_probability_real_time_vs_latest.pdf"
  ),
  plot = final_plot,
  width = 13,
  height = 9
)

cat("\nCompleted. Outputs saved in:\n", OUTPUT_DIR, "\n", sep = "")
