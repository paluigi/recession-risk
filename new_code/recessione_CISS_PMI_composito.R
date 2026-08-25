# Hierarchical Bayesian nowcasting of Euro Area recession risk.
#
# Data sources:
#   1. GDP (real QoQ % change, SCA) from EUROSTAT via RJSDMX
#   2. CISS (daily) from ECB via RJSDMX
#   3. S&P Global Composite PMI - Output (monthly) from Excel
#
# Target variable: technical recession = two consecutive quarters of
# negative GDP growth.
#
# Areas: Euro Area (EA), Germany (DE), France (FR), Italy (IT), Spain (ES).

# --- rJava / Java 16+ compatibility (MUST precede library(RJSDMX)) ----------
# If rJava was already used in this R session, restart R before running.
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
  stop("Java is not available in the PATH. RJSDMX requires Java 8 or later.")
}

###############################################################################
# 1. SETTINGS
###############################################################################

gdp_geo_map <- tibble(
  eurostat_code = c("EA", "DE", "FR", "IT", "ES"),
  geo           = c("EA", "DE", "FR", "IT", "ES")
)

ciss_geo_map <- tibble(
  ecb_code = c("U2", "DE", "FR", "IT", "ES"),
  geo      = c("EA", "DE", "FR", "IT", "ES")
)

pmi_sheet_map <- tribble(
  ~sheet,    ~geo,
  "CompEMU", "EA",
  "CompGER", "DE",
  "CompFRA", "FR",
  "CompITA", "IT",
  "CompSPA", "ES"
)

# Main path requested by the user.
network_pmi_file <- c('//home/group/main/891ac/private/ac/PMI/pmi_servizi_composito.xlsx')

# Optional fallback: useful when the workbook is copied to the working folder.
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

set.seed(123)

###############################################################################
# 2. SDMX HELPERS
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

###############################################################################
# 3. PMI EXCEL HELPERS
###############################################################################

# Workbook structure verified in the attached file:
#   - rows 1:9 contain metadata
#   - column A contains labels such as Jan98
#   - column B is S&P GLOBAL PMI: COMPOSITE - OUTPUT
#   - columns C:G contain other composite-PMI sub-indicators and are not used

parse_pmi_month <- function(x) {
  x <- toupper(trimws(as.character(x)))

  month_codes <- c(
    "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
    "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
  )

  month_number <- match(substr(x, 1, 3), month_codes)
  two_digit_year <- suppressWarnings(as.integer(substr(x, 4, 5)))

  # The workbook covers 1998-2026. This rule is locale-independent and also
  # handles later updates while two-digit labels are retained.
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

read_pmi_series <- function(sheet, output_geo) {
  # Validate that column B still contains the requested series.
  series_name <- readxl::read_excel(
    path = pmi_file,
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

  # Skip the nine metadata rows. Only columns A and B are needed.
  raw <- readxl::read_excel(
    path = pmi_file,
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
      geo = output_geo,
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
    filter(
      !is.na(month),
      is.finite(pmi_composite)
    ) %>%
    group_by(geo, month) %>%
    summarise(
      pmi_composite = mean(pmi_composite, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(month)

  if (nrow(result) == 0) {
    stop(paste0("No usable composite-PMI observations in sheet ", sheet, "."))
  }

  cat(
    sprintf(
      "PMI %-7s -> %s | %s to %s | %d monthly observations\n",
      sheet,
      output_geo,
      format(min(result$month), "%b %Y"),
      format(max(result$month), "%b %Y"),
      nrow(result)
    )
  )

  result
}

###############################################################################
# 4. RJSDMX PROVIDER CHECK
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
# 5. DOWNLOAD QUARTERLY GDP FROM EUROSTAT
###############################################################################

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
    filter(
      !is.na(quarter),
      is.finite(gdp_growth)
    ) %>%
    group_by(geo, quarter) %>%
    summarise(gdp_growth = mean(gdp_growth, na.rm = TRUE), .groups = "drop")
}

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

cat("\nGDP coverage:\n")
print(
  gdp %>%
    group_by(geo) %>%
    summarise(
      first_quarter = min(quarter),
      last_quarter = max(quarter),
      observations = n(),
      .groups = "drop"
    )
)

###############################################################################
# 6. DOWNLOAD DAILY CISS FROM THE ECB
###############################################################################

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
    filter(
      !is.na(time),
      is.finite(ciss_daily)
    )
}

ciss_daily <- bind_rows(
  lapply(
    seq_len(nrow(ciss_geo_map)),
    function(i) {
      read_ciss_series(
        ecb_code = ciss_geo_map$ecb_code[i],
        output_geo = ciss_geo_map$geo[i]
      )
    }
  )
)

ciss <- ciss_daily %>%
  mutate(quarter = as.yearqtr(time)) %>%
  group_by(geo, quarter) %>%
  summarise(ciss = mean(ciss_daily, na.rm = TRUE), .groups = "drop") %>%
  filter(is.finite(ciss)) %>%
  arrange(geo, quarter)

cat("\nCISS coverage:\n")
print(
  ciss %>%
    group_by(geo) %>%
    summarise(
      first_quarter = min(quarter),
      last_quarter = max(quarter),
      observations = n(),
      .groups = "drop"
    )
)

###############################################################################
# 7. READ MONTHLY COMPOSITE PMI FROM EXCEL
###############################################################################

available_pmi_sheets <- readxl::excel_sheets(pmi_file)
missing_pmi_sheets <- setdiff(pmi_sheet_map$sheet, available_pmi_sheets)

if (length(missing_pmi_sheets) > 0) {
  stop(
    paste0(
      "The PMI workbook is missing these sheets: ",
      paste(missing_pmi_sheets, collapse = ", "),
      ". Available sheets: ",
      paste(available_pmi_sheets, collapse = ", ")
    )
  )
}

pmi_monthly <- bind_rows(
  lapply(
    seq_len(nrow(pmi_sheet_map)),
    function(i) {
      read_pmi_series(
        sheet = pmi_sheet_map$sheet[i],
        output_geo = pmi_sheet_map$geo[i]
      )
    }
  )
)

# Same aggregation rule used for the former monthly ESI: quarterly mean.
pmi <- pmi_monthly %>%
  mutate(quarter = as.yearqtr(month)) %>%
  group_by(geo, quarter) %>%
  summarise(
    pmi_composite = mean(pmi_composite, na.rm = TRUE),
    pmi_months_available = n_distinct(month),
    .groups = "drop"
  ) %>%
  filter(is.finite(pmi_composite)) %>%
  arrange(geo, quarter)

cat("\nComposite PMI quarterly coverage:\n")
print(
  pmi %>%
    group_by(geo) %>%
    summarise(
      first_quarter = min(quarter),
      last_quarter = max(quarter),
      observations = n(),
      min_months_per_quarter = min(pmi_months_available),
      max_months_per_quarter = max(pmi_months_available),
      .groups = "drop"
    )
)

###############################################################################
# 8. COMBINE DATA AND CALCULATE RECESSION RISK
###############################################################################

df <- gdp %>%
  inner_join(ciss, by = c("geo", "quarter")) %>%
  inner_join(pmi, by = c("geo", "quarter")) %>%
  arrange(geo, quarter) %>%
  group_by(geo) %>%
  mutate(
    recession = ifelse(
      gdp_growth < 0 & lag(gdp_growth, 1) < 0,
      1,
      0
    )
  ) %>%
  drop_na() %>%
  ungroup()

if (nrow(df) == 0) {
  stop("GDP, CISS and composite PMI have no common usable observations.")
}

# Standardize the predictors as in the original specification.
df <- df %>%
  mutate(
    ciss_scaled = scale(ciss)[, 1],
    pmi_scaled = scale(pmi_composite)[, 1]
  )

max_date <- max(df$quarter)
cutoff_date <- max_date - 5

train_data <- df %>% filter(quarter <= cutoff_date)
inference_data <- df

if (nrow(train_data) == 0) {
  stop("No training observations are available before the five-year cutoff.")
}

###############################################################################
# 9. HIERARCHICAL BAYESIAN LOGIT MODEL
###############################################################################

formula <- brmsformula(
  recession ~ ciss_scaled + pmi_scaled +
    (1 + ciss_scaled + pmi_scaled | geo),
  family = bernoulli(link = "logit")
)

brms_model <- brm(
  formula = formula,
  data = train_data,
  prior = c(
    prior(normal(0, 5), class = "Intercept"),
    prior(normal(0, 2), class = "b")
  ),
  chains = 2,
  cores = 2,
  iter = 2000,
  seed = 123,
  silent = 2,
  refresh = 0
)

summary(brms_model)

###############################################################################
# 10. OUT-OF-SAMPLE INFERENCE
###############################################################################

predictions <- fitted(
  brms_model,
  newdata = inference_data,
  type = "response"
)

df_results <- inference_data %>%
  mutate(
    prob_recession = predictions[, "Estimate"],
    lower_95 = predictions[, "Q2.5"],
    upper_95 = predictions[, "Q97.5"],
    is_forecast = ifelse(
      quarter > cutoff_date,
      "Inferred (Last 5 Yrs)",
      "Training Data"
    )
  )

###############################################################################
# 11. VISUALIZATION
###############################################################################

plot_geo <- function(data, region_name, is_main = FALSE) {
  p_data <- data %>% filter(geo == region_name)

  ggplot(p_data, aes(x = as.Date(quarter))) +
    geom_rect(
      data = p_data %>% filter(recession == 1),
      aes(
        xmin = as.Date(quarter) - 45,
        xmax = as.Date(quarter) + 45,
        ymin = 0,
        ymax = 1
      ),
      fill = "grey80",
      alpha = 0.5,
      inherit.aes = FALSE
    ) +
    geom_line(
      aes(y = prob_recession, color = is_forecast),
      linewidth = 1
    ) +
    geom_ribbon(
      aes(ymin = lower_95, ymax = upper_95, fill = is_forecast),
      alpha = 0.2
    ) +
    geom_vline(
      xintercept = as.Date(cutoff_date),
      linetype = "dashed",
      color = "red"
    ) +
    scale_y_continuous(
      labels = scales::percent_format(),
      limits = c(0, 1)
    ) +
    scale_color_manual(
      values = c(
        "Training Data" = "#004B87",
        "Inferred (Last 5 Yrs)" = "#E37222"
      )
    ) +
    scale_fill_manual(
      values = c(
        "Training Data" = "#004B87",
        "Inferred (Last 5 Yrs)" = "#E37222"
      )
    ) +
    labs(
      title = region_name,
      y = "Recession Probability",
      x = "",
      color = "Data Period",
      fill = "Data Period"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(
        face = "bold",
        size = ifelse(is_main, 16, 12)
      )
    )
}

p_ea <- plot_geo(df_results, "EA", is_main = TRUE) +
  labs(
    subtitle = paste0(
      "Grey bars indicate actual historical recessions ",
      "(2 consecutive quarters of negative growth). ",
      "Red dashed line denotes the 5-year data cut-off."
    )
  )

p_de <- plot_geo(df_results, "DE")
p_fr <- plot_geo(df_results, "FR")
p_it <- plot_geo(df_results, "IT")
p_es <- plot_geo(df_results, "ES")

country_grid <- (p_de | p_fr) / (p_it | p_es) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

final_plot <- p_ea / country_grid +
  plot_layout(heights = c(1, 1.5))

final_plot
