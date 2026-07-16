# Hierarchical Bayesian nowcasting of Euro Area recession risk.
#
# Data sources (all fetched via RJSDMX -> SDMX 2.1 web services):
#   1. GDP (real QoQ % change, SCA) from EUROSTAT  -> namq_10_gdp
#   2. CISS (daily) from ECB                        -> CISS
#   3. ESI (monthly, SA) from EUROSTAT              -> ei_bssi_m_r2
#
# Target variable: technical recession = two consecutive quarters of
# negative GDP growth.
#
# Areas: Euro Area (EA), Germany (DE), France (FR), Italy (IT), Spain (ES).

# --- rJava / Java 16+ compatibility (MUST precede library(RJSDMX)) ----------
# RJSDMX delegates to rJava. On Java 16+ (e.g. 17/21) strong module
# encapsulation blocks rJava's reflective access to java.util.TreeMap$Entry,
# which makes Map-returning RJSDMX calls fail with:
#   IllegalAccessException: class RJavaTools cannot access a member of class
#   java.util.TreeMap$Entry (in module java.base) with modifiers "public"
# Re-opening the java.util module to unnamed modules fixes it. Use a character
# vector (NOT a paste0 string) so a trailing space cannot make the JVM misparse
# the target as "ALL-UNNAMED " -> "Unknown module". JVM options only take effect
# when the JVM is first created, so this must run before library(RJSDMX) / the
# first rJava call. If rJava was already used in this R session, restart R.
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
  "tidyr",
  "tibble",
  "lubridate",
  "ggplot2",
  "patchwork",
  "brms",
  "zoo",
  "scales"
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

# RJSDMX requires Java 8 or later.
if (Sys.which("java") == "") {
  stop("Java is not available in the PATH. RJSDMX requires Java 8 or later.")
}

###############################################################################
# 1. SETTINGS
###############################################################################

# Eurostat SDMX geo codes for the Euro Area differ by dataset:
#   - GDP (namq_10_gdp) exposes the changing-composition code "EA".
#   - ESI (ei_bssi_m_r2) has NO "EA" code on SDMX; EA21 is used instead.
# The ECB uses "U2" for the Euro Area. All are relabelled to "EA" internally.
gdp_geo_map <- tibble(
  eurostat_code = c("EA", "DE", "FR", "IT", "ES"),
  geo           = c("EA", "DE", "FR", "IT", "ES")
)

esi_geo_map <- tibble(
  eurostat_code = c("EA21",  "DE", "FR", "IT", "ES"),
  geo           = c("EA",    "DE", "FR", "IT", "ES")
)

ciss_geo_map <- tibble(
  ecb_code = c("U2", "DE", "FR", "IT", "ES"),
  geo      = c("EA", "DE", "FR", "IT", "ES")
)

set.seed(123)

###############################################################################
# 2. SDMX HELPERS
###############################################################################

# RJSDMX::getTimeSeriesTable returns a data frame with a TIME_PERIOD and an
# OBS_VALUE column (plus metadata). This normalises names/casing and validates
# the minimal contract the download functions rely on.
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

# Single wrapper around getTimeSeriesTable with clear error reporting.
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

# Parses SDMX quarterly time labels: "2025-Q3", "2025Q3", or gregorian "2025-07-01".
# Always returns a yearqtr vector. NOTE: we cannot initialise with
# rep(as.yearqtr(NA_real_), n) -- rep() strips the yearqtr class and yields plain
# numeric, which later breaks dplyr joins with "incompatible types" (double vs
# yearqtr). Instead we normalise to a character vector and apply ONE final
# as.yearqtr() so the class is guaranteed.
parse_sdmx_quarter <- function(x) {
  x <- trimws(as.character(x))
  normalized <- rep(NA_character_, length(x))

  q_idx <- grepl("Q[1-4]$", x, ignore.case = TRUE)
  if (any(q_idx)) {
    # "2025-Q3" / "2025Q3" -> "2025 Q3"
    normalized[q_idx] <- gsub("-?Q", " Q", x[q_idx], ignore.case = TRUE)
  }

  g_idx <- !q_idx & nzchar(x)
  if (any(g_idx)) {
    # gregorian "2025-07-01" -> "2025 Q3"
    normalized[g_idx] <- format(as.yearqtr(as.Date(x[g_idx])))
  }

  as.yearqtr(normalized, format = "%Y Q%q")
}

# Parses SDMX monthly time labels: "2026-04" or gregorian "2026-04-01".
# Same rep()-avoidance as parse_sdmx_quarter; always returns a yearmon vector.
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
# 3. RJSDMX PROVIDER CHECK
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
# 4. DOWNLOAD QUARTERLY GDP FROM EUROSTAT VIA RJSDMX
###############################################################################

# Key structure (FREQ.UNIT.S_ADJ.NA_ITEM.GEO):
#   Q            = quarterly frequency
#   CLV_PCH_PRE  = chain-linked volumes, % change on previous period (real QoQ)
#                  NOTE: the bulk-API code "PCH_PRE" is INVALID on the SDMX API.
#   SCA          = seasonally and calendar adjusted
#   B1GQ         = GDP at market prices
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
    summarise(first_quarter = min(quarter),
              last_quarter = max(quarter),
              observations = n(),
              .groups = "drop")
)

###############################################################################
# 5. DOWNLOAD DAILY CISS FROM THE ECB VIA RJSDMX
###############################################################################

# Key (CISS.D.REF_AREA.Z0Z.4F.EC.SS_CIN.IDX):
#   D       = daily frequency
#   SS_CIN  = Composite Indicator of Systemic Stress
#   IDX     = index
# The original monthly key "...SS_CIN.B" does not exist on SDMX (404); only the
# daily series is available. Daily values are collapsed monthly then quarterly.
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
    summarise(first_quarter = min(quarter),
              last_quarter = max(quarter),
              observations = n(),
              .groups = "drop")
)

###############################################################################
# 6. DOWNLOAD MONTHLY ESI FROM EUROSTAT VIA RJSDMX
###############################################################################

# Key (FREQ.INDIC.S_ADJ.GEO):
#   M          = monthly frequency
#   BS-ESI-I   = Economic Sentiment Indicator
#   SA         = seasonally adjusted
# There is no "unit" dimension in this dataset. The Euro Area uses "EA21"
# because the changing-composition code "EA" is not exposed on SDMX for ESI.
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
    filter(
      !is.na(month),
      is.finite(esi)
    ) %>%
    group_by(geo, month) %>%
    summarise(esi = mean(esi, na.rm = TRUE), .groups = "drop")
}

esi <- bind_rows(
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
  mutate(quarter = as.yearqtr(month)) %>%
  group_by(geo, quarter) %>%
  summarise(esi = mean(esi, na.rm = TRUE), .groups = "drop") %>%
  filter(is.finite(esi)) %>%
  arrange(geo, quarter)

cat("\nESI coverage:\n")
print(
  esi %>%
    group_by(geo) %>%
    summarise(first_quarter = min(quarter),
              last_quarter = max(quarter),
              observations = n(),
              .groups = "drop")
)

## 7. Combine data and calculate recession risk
# Merge all datasets
df <- gdp %>%
  inner_join(ciss, by = c("geo", "quarter")) %>%
  inner_join(esi, by = c("geo", "quarter")) %>%
  arrange(geo, quarter) %>%
  group_by(geo) %>%
  mutate(
    # Recession definition: current quarter < 0 AND previous quarter < 0
    recession = ifelse(gdp_growth < 0 & lag(gdp_growth, 1) < 0, 1, 0)
  ) %>%
  drop_na() %>% # Drop NAs created by lag and missing series overlaps
  ungroup()

# Standardize the predictors to help the Bayesian model converge faster
df <- df %>%
  mutate(
    ciss_scaled = scale(ciss)[, 1],
    esi_scaled = scale(esi)[, 1]
  )

# Hierarchical Bayesian Logit model
# Determine the cut-off date (5 years ago from the maximum available date in the dataset)
max_date <- max(df$quarter)
cutoff_date <- max_date - 5 # 5 years subtraction on yearqtr object

# Split data
train_data <- df %>% filter(quarter <= cutoff_date)
inference_data <- df # We infer on the whole set

# Hierarchical Bayesian Logit Model using brms
# Random intercepts and random slopes for each geo (Euro Area and countries)
formula <- brmsformula(
  recession ~ ciss_scaled + esi_scaled + (1 + ciss_scaled + esi_scaled | geo),
  family = bernoulli(link = "logit")
)

# Fit the model (this may take a minute or two depending on your machine)
# We set a seed for reproducibility.
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

# OUt of sample inference
# Extract predicted probabilities (Estimate = mean of posterior)
predictions <- fitted(brms_model, newdata = inference_data, type = "response")

df_results <- inference_data %>%
  mutate(
    prob_recession = predictions[, "Estimate"],
    lower_95 = predictions[, "Q2.5"],
    upper_95 = predictions[, "Q97.5"],
    is_forecast = ifelse(quarter > cutoff_date, "Inferred (Last 5 Yrs)", "Training Data")
  )

# Data visualization
# Function to generate a plot for a specific geography
plot_geo <- function(data, region_name, is_main = FALSE) {
  p_data <- data %>% filter(geo == region_name)

  p <- ggplot(p_data, aes(x = as.Date(quarter))) +
    # Actual Recession shaded areas
    geom_rect(
      data = p_data %>% filter(recession == 1),
      aes(
        xmin = as.Date(quarter) - 45, xmax = as.Date(quarter) + 45,
        ymin = 0, ymax = 1
      ),
      fill = "grey80", alpha = 0.5, inherit.aes = FALSE
    ) +
    # Recession Probability line
    geom_line(aes(y = prob_recession, color = is_forecast), linewidth = 1) +
    # 95% Credible Intervals
    geom_ribbon(aes(ymin = lower_95, ymax = upper_95, fill = is_forecast), alpha = 0.2) +
    geom_vline(xintercept = as.Date(cutoff_date), linetype = "dashed", color = "red") +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    scale_color_manual(values = c("Training Data" = "#004B87", "Inferred (Last 5 Yrs)" = "#E37222")) +
    scale_fill_manual(values = c("Training Data" = "#004B87", "Inferred (Last 5 Yrs)" = "#E37222")) +
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
      plot.title = element_text(face = "bold", size = ifelse(is_main, 16, 12))
    )

  return(p)
}

# 1. Main plot for Euro Area
p_ea <- plot_geo(df_results, "EA", is_main = TRUE) +
  labs(subtitle = "Grey bars indicate actual historical recessions (2 consecutive quarters of negative growth). Red dashed line denotes the 5-year data cut-off.")

# 2. Country plots
p_de <- plot_geo(df_results, "DE")
p_fr <- plot_geo(df_results, "FR")
p_it <- plot_geo(df_results, "IT")
p_es <- plot_geo(df_results, "ES")

# Combine country plots into a 2x2 grid
country_grid <- (p_de | p_fr) / (p_it | p_es) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Final layout: EA on top, 2x2 grid below
final_plot <- p_ea / country_grid + plot_layout(heights = c(1, 1.5))

final_plot
