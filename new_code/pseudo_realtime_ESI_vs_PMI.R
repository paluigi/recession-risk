# Pseudo-real-time comparison: ESI-based vs PMI-based recession-risk nowcasts.
#
# This script runs the two recession-risk specifications of new_code/
# (recessione_CISS_ESI.R and recessione_CISS_PMI_composito.R) in a unified
# pseudo-real-time backtest over the Euro Area and the four largest member
# states (DE, FR, IT, ES), and compares their real-time forecasting
# performance with proper scoring rules and Diebold-Mariano tests.
#
# ---------------------------------------------------------------------------
# DATA SOURCES
# ---------------------------------------------------------------------------
# GDP (target variable, real-time):
#   * Euro Area aggregate : Eurostat ei_na_q_vtg (quarterly GDP vintages,
#                           CLV05_MEUR levels, changing-composition "EA"),
#                           downloaded via RJSDMX exactly like the existing
#                           robustness scripts.
#   * Countries (DE FR IT ES): OECD "Short-term economic statistics
#                           revisions" (DSD_STES_REVISIONS@DF_STES_REVISIONS,
#                           v4.0), the MEI vintage archive: monthly editions
#                           1999-02 -> present. Key
#                           DEU+FRA+ITA+ESP.Q.B1GQ_Q.XDC._T.<EDITION>
#                           returns the full history of seasonally adjusted
#                           GDP volume LEVELS in national currency as
#                           published in that edition. QoQ growth and the
#                           technical-recession label are computed within
#                           each edition so that training labels are exactly
#                           what a forecaster saw at that edition date.
#   Eurostat does NOT disseminate country-level GDP vintages through its API
#   (ei_na_q_vtg covers EU/EA aggregates only; the PEEI euroind_vtg tables
#   are Excel/browser-only), so the OECD MEI archive is the public source of
#   country vintages. See new_code/data_sources_pseudo_realtime.md.
#
# CISS (predictor, daily, ECB):        CISS/D.<geo>.Z0Z.4F.EC.SS_CIN.IDX
# ESI  (predictor, monthly, Eurostat): ei_bssi_m_r2/M.BS-ESI-I.SA.<geo>
# PMI  (predictor, monthly, S&P Global): pmi_servizi_composito.xlsx workbook
#   (sheets CompEMU/CompGER/CompFRA/CompITA/CompSPA). The workbook contains
#   licensed S&P Global data that CANNOT leave the production environment;
#   the script looks for it first on the production network path and then in
#   the working directory, exactly like recessione_CISS_PMI_composito.R.
#
# ---------------------------------------------------------------------------
# EVENT GRID (pseudo-real-time design)
# ---------------------------------------------------------------------------
# Evaluation events are the OECD MEI edition dates (edition month -> as-of
# date = 1st of the following month) at which at least one area's latest
# available GDP quarter changes, i.e. the first nowcast opportunity for each
# new quarter. At each event date V:
#   * countries use the GDP panel of the latest OECD edition <= V;
#   * the Euro Area uses the latest Eurostat vintage <= V;
#   * CISS is cut at V; ESI and PMI months enter only once published
#     (day-29-of-reference-month rule for ESI with optional calendar
#     override file; day-26 rule for PMI), and quarterly means require all
#     three months of the quarter (complete-quarter rule);
#   * the hierarchical Bayesian logit is estimated ONCE per approach on the
#     pooled 5-area panel with a 5-YEAR GAP: for each area, training rows
#     satisfy quarter <= target_quarter(area) - 5 years, replicating the
#     train/inference split of the production scripts (max_date - 5);
#   * the model nowcasts the recession probability of each area's latest
#     GDP quarter, for BOTH approaches:
#       ESI: recession ~ ciss + esi + (1 + ciss + esi | geo)
#       PMI: recession ~ ciss + pmi + (1 + ciss + pmi | geo)
# Each (area, quarter) is scored once, at its first nowcast opportunity.
#
# Comparison:
#   * Brier score, log score, AUC per area and pooled, both approaches;
#   * Diebold-Mariano tests of equal predictive ability on the paired loss
#     differentials (Brier and log losses) with Newey-West HAC variance
#     (lag 1) and the Harvey et al. (1997) small-sample correction, run per
#     area and on the pooled sample.
#
# IMPORTANT LIMITATIONS (in line with the existing robustness scripts):
#   * Eurostat's ESI dataset supplies the currently available history, not
#     historical ESI vintages: ESI is real-time with respect to publication
#     availability, but values may include later revisions.
#   * PMI values are not revised by S&P Global, so their as-of treatment is
#     exact; the PMI history cannot be copied out of production.
#   * OECD MEI editions are monthly snapshots of the OECD MEI database, not
#     national first releases; they are the best publicly available archive
#     of country GDP vintages. EA vintages come from Eurostat directly.
#   * The OECD endpoint enforces aggressive rate limits; downloads are
#     retried with exponential backoff and cached in OUTPUT_DIR, so re-runs
#     skip the bulk download.

# --- rJava / Java 16+ compatibility (MUST precede library(RJSDMX)) ----------
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

# Areas: the Euro Area aggregate plus the four largest euro-area economies.
areas <- c("EA", "DE", "FR", "IT", "ES")

# Eurostat ESI geo codes: ei_bssi_m_r2 exposes EA21 (there is no
# changing-composition "EA" code on SDMX); countries use ISO2 codes.
esi_geo_map <- tibble(
  geo           = c("EA",    "DE", "FR", "IT", "ES"),
  eurostat_code = c("EA21",  "DE", "FR", "IT", "ES")
)

# ECB CISS uses U2 for the Euro Area.
ciss_geo_map <- tibble(
  geo      = c("EA",  "DE", "FR", "IT", "ES"),
  ecb_code = c("U2",  "DE", "FR", "IT", "ES")
)

# OECD STES revisions areas (ISO3) with their internal geo labels.
oecd_geo_map <- tibble(
  geo       = c("DE",  "FR",   "IT",   "ES"),
  oecd_code = c("DEU", "FRA",  "ITA",  "ESP")
)

# First evaluation event (as-of date). The OECD edition grid and the
# Eurostat EA archive both have ample history before this point.
EVAL_START <- as.Date("2015-01-01")

# Gap design: for each area, training quarters satisfy
# quarter <= target_quarter - TRAINING_GAP_YEARS (literal replication of the
# production scripts' cutoff_date <- max_date - 5).
TRAINING_GAP_YEARS <- 5

MIN_TRAINING_OBS     <- 40L   # minimum pooled training rows per run
MIN_RECESSION_EVENTS <- 2L    # min recession and non-recession rows

# Publication-timing assumptions for monthly indicators (same as the
# existing robustness scripts).
ESI_RELEASE_DAY <- 29L                # ESI published ~day 29 of ref month
PMI_RELEASE_DAY <- 26L                # S&P composite PMI ~day 26 of ref month
CISS_RELEASE_LAG_DAYS <- 0L           # CISS cut at the event date
REQUIRE_COMPLETE_MONTHLY_QUARTER <- TRUE

# Bayesian model controls (one hierarchical fit per approach per event).
BRMS_CHAINS <- 2L
detected_cores <- parallel::detectCores(logical = TRUE)
BRMS_CORES <- if (is.na(detected_cores)) 1L else min(2L, detected_cores)
BRMS_ITER <- 2000L
BRMS_WARMUP <- 1000L
BRMS_ADAPT_DELTA <- 0.95
BRMS_MAX_TREEDEPTH <- 12L
BRMS_BACKEND <- NULL                  # set "cmdstanr" if preferred
CACHE_MODELS <- TRUE

# OECD download controls (the endpoint rate-limits aggressively).
OECD_EDITIONS_FROM <- "201412"        # first edition month to archive
OECD_EDITIONS_TO <- NULL              # last edition month; NULL = current
OECD_CURL_PAUSE <- 8                  # seconds between edition requests
OECD_MAX_ATTEMPTS <- 6L               # retries per edition (429 backoff)
OECD_BACKOFF_START <- 45              # first backoff seconds

# PMI workbook location (production network path first, then working dir).
network_pmi_file <- '//home/group/main/891ac/private/ac/PMI/pmi_servizi_composito.xlsx'
local_pmi_file <- file.path(getwd(), "pmi_servizi_composito.xlsx")

# Optional exact ESI release calendar (month,release_date CSV), as in the
# existing robustness scripts.
ESI_RELEASE_CALENDAR_FILE <- file.path(getwd(), "esi_release_calendar.csv")

OUTPUT_DIR <- file.path(
  getwd(),
  "pseudo_realtime_ESI_vs_PMI_output"
)
MODEL_CACHE_DIR <- file.path(OUTPUT_DIR, "brms_cache")
OECD_CACHE_FILE <- file.path(OUTPUT_DIR, "oecd_gdp_vintages_raw.csv")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MODEL_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

set.seed(123)

###############################################################################
# 2. GENERIC HELPERS  (SDMX table handling, parsers, metrics)
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

  names(x) <- tolower(gsub("[^A-Za-z0-9]+", "_", names(x)))

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
        ". Available columns: ", paste(names(x), collapse = ", ")
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

parse_sdmx_month <- function(x) {
  x <- trimws(as.character(x))
  normalized <- rep(NA_character_, length(x))

  short_idx <- grepl("^[0-9]{4}-[0-9]{2}$", x)
  if (any(short_idx)) {
    normalized[short_idx] <- x[short_idx]
  }

  date_idx <- !short_idx & nzchar(x)
  if (any(date_idx)) {
    parsed_dates <- suppressWarnings(as.Date(x[date_idx]))
    normalized[date_idx] <- ifelse(
      is.na(parsed_dates),
      NA_character_,
      format(parsed_dates, "%Y-%m")
    )
  }

  as.yearmon(normalized, format = "%Y-%m")
}

is_consecutive_quarter <- function(current_quarter, previous_quarter) {
  gap <- 4 * (as.numeric(current_quarter) - as.numeric(previous_quarter))
  !is.na(gap) & abs(gap - 1) < 1e-8
}

release_date_from_fixed_day <- function(month, release_day) {
  month_start <- as.Date(month, frac = 0)
  month_end <- ceiling_date(month_start, unit = "month") - days(1)
  candidate <- month_start + days(release_day - 1L)

  as.Date(
    pmin(as.numeric(candidate), as.numeric(month_end)),
    origin = "1970-01-01"
  )
}

safe_scale_parameters <- function(x, label) {
  mu <- mean(x, na.rm = TRUE)
  sigma <- stats::sd(x, na.rm = TRUE)

  if (!is.finite(mu) || !is.finite(sigma) || sigma <= 0) {
    stop(paste0("Cannot standardize ", label, ": invalid mean or sd."))
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
        scope = label,
        observations = 0L,
        recession_rate = NA_real_,
        brier_score = NA_real_,
        log_score = NA_real_,
        auc = NA_real_
      )
    )
  }

  tibble(
    scope = label,
    observations = length(y),
    recession_rate = mean(y),
    brier_score = mean((p - y)^2),
    log_score = -mean(y * log(p) + (1 - y) * log(1 - p)),
    auc = binary_auc(y, p)
  )
}

###############################################################################
# 2b. DIEBOLD-MARIANO TEST (HAC variance + Harvey small-sample correction)
###############################################################################

# Diebold-Mariano (1995) test of equal predictive ability for two series of
# out-of-sample forecast losses of the same events. The loss differential
# d_t = loss1_t - loss2_t has mean d_bar; the statistic is
# d_bar / sqrt(Newey-West HAC variance / n) with lag.order autocovariance
# terms, corrected for small samples following Harvey, Leybourne and
# Newbold (1997): DM* = sqrt[(n + 1 - 2h + h(h-1)/n) / n] * DM, where h is
# the forecast horizon (h = 1 for the one-step nowcasts used here; the HAC
# lag.order is a separate tuning choice). Positive statistics mean
# loss1 > loss2 (method 2 better).
diebold_mariano_test <- function(loss1, loss2,
                                 lag.order = 1L,
                                 h = 1L,
                                 alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)
  loss1 <- as.numeric(loss1)
  loss2 <- as.numeric(loss2)

  ok <- is.finite(loss1) & is.finite(loss2)
  loss1 <- loss1[ok]
  loss2 <- loss2[ok]
  n <- length(loss1)

  if (n < 3L) {
    return(
      tibble(
        n_obs = n,
        mean_loss1 = if (n) mean(loss1) else NA_real_,
        mean_loss2 = if (n) mean(loss2) else NA_real_,
        mean_loss_diff = if (n) mean(loss1 - loss2) else NA_real_,
        dm_statistic = NA_real_,
        harvey_statistic = NA_real_,
        p_value = NA_real_,
        alternative = alternative
      )
    )
  }

  d <- loss1 - loss2
  d_bar <- mean(d)

  # Newey-West HAC variance of the mean.
  dc <- d - d_bar
  gamma0 <- mean(dc^2)
  hac_var <- gamma0
  L <- max(0L, as.integer(lag.order))
  if (L >= 1L && n > L) {
    for (l in seq_len(L)) {
      gamma_l <- mean(dc[(l + 1L):n] * dc[1L:(n - l)])
      hac_var <- hac_var + 2 * (1 - l / (L + 1)) * gamma_l
    }
  }
  hac_var <- hac_var / n

  if (!is.finite(hac_var) || hac_var <= 0) {
    return(
      tibble(
        n_obs = n,
        mean_loss1 = mean(loss1),
        mean_loss2 = mean(loss2),
        mean_loss_diff = d_bar,
        dm_statistic = NA_real_,
        harvey_statistic = NA_real_,
        p_value = NA_real_,
        alternative = alternative
      )
    )
  }

  dm <- d_bar / sqrt(hac_var)

  # Harvey, Leybourne, Newbold (1997) small-sample correction.
  harvey <- dm * sqrt((n + 1L - 2L * h + h * (h - 1L) / n) / n)

  p <- switch(
    alternative,
    two.sided = 2 * stats::pnorm(-abs(harvey)),
    less = stats::pnorm(harvey),
    greater = 1 - stats::pnorm(harvey)
  )

  tibble(
    n_obs = n,
    mean_loss1 = mean(loss1),
    mean_loss2 = mean(loss2),
    mean_loss_diff = d_bar,
    dm_statistic = dm,
    harvey_statistic = harvey,
    p_value = p,
    alternative = alternative
  )
}

# Loss helpers: quadratic (Brier) and log losses of probability nowcasts
# against the same binary outcome vector.
brier_losses <- function(y, p) {
  ok <- is.finite(y) & is.finite(p)
  y <- y[ok]
  p <- clamp_probability(p[ok])
  (p - y)^2
}

log_losses <- function(y, p) {
  ok <- is.finite(y) & is.finite(p)
  y <- y[ok]
  p <- clamp_probability(p[ok])
  -(y * log(p) + (1 - y) * log(1 - p))
}

###############################################################################
# 3. GDP VINTAGE ARCHIVES
###############################################################################

# --- 3a. Euro Area: Eurostat ei_na_q_vtg via RJSDMX ----------------------
# Dimensions FREQ.REVDATE.S_ADJ.UNIT.GEO; empty REVDATE = all revision dates.
# Levels (CLV05_MEUR) of the changing-composition EA aggregate.
download_ea_gdp_vintages <- function() {
  raw <- download_sdmx_table(
    provider = "EUROSTAT",
    id = "ei_na_q_vtg/Q..SCA.CLV05_MEUR.EA",
    label = "Euro Area quarterly GDP vintages"
  )

  revdate_column <- resolve_column(
    raw,
    candidates = c("revdate", "rev_date", "revision_date"),
    label = "GDP vintage revision-date column"
  )

  raw %>%
    transmute(
      geo = "EA",
      vintage_date = as.Date(.data[[revdate_column]]),
      quarter = parse_sdmx_quarter(time_period),
      gdp_level = suppressWarnings(as.numeric(obs_value))
    ) %>%
    filter(
      !is.na(vintage_date),
      !is.na(quarter),
      is.finite(gdp_level)
    )
}

# --- 3b. Countries: OECD STES revisions (MEI editions) -------------------
# One batched request per edition covers all four countries:
#   DEU+FRA+ITA+ESP.Q.B1GQ_Q.XDC._T.<EDITION>
# The response is SDMX-CSV (needs a custom Accept header, so we drive curl);
# each edition returns the full published history of quarterly GDP volume
# levels in national currency. Edition YYYYMM maps to as-of date
# YYYY-MM-01 + 1 month (the MEI edition is published in its label month).
OECD_STES_BASE <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.STES,DSD_STES_REVISIONS@DF_STES_REVISIONS,4.0"
OECD_CSV_ACCEPT <- "application/vnd.sdmx.data+csv; charset=utf-8; version=2; labels=id"

oecd_edition_asof_date <- function(edition) {
  year <- as.integer(substr(edition, 1L, 4L))
  month <- as.integer(substr(edition, 5L, 6L))
  as.Date(make_date(year, month, 1L) + months(1))
}

# Generate the sequence of edition codes from OECD_EDITIONS_FROM to
# OECD_EDITIONS_TO (default: the current month, inclusive).
edition_sequence <- function(from = OECD_EDITIONS_FROM,
                             to = OECD_EDITIONS_TO) {
  if (is.null(to)) to <- format(Sys.Date(), "%Y%m")
  start <- as.Date(paste0(substr(from, 1L, 4L), "-", substr(from, 5L, 6L), "-01"))
  end <- as.Date(paste0(substr(to, 1L, 4L), "-", substr(to, 5L, 6L), "-01"))
  if (end < start) return(character())
  months_seq <- seq(start, end, by = "month")
  format(months_seq, "%Y%m")
}

# Download one edition (all four countries batched) with retries/backoff on
# HTTP 429/5xx, using the R curl package for header control and status
# reporting. Returns a data frame or NULL when the edition has no data.
fetch_oecd_edition <- function(edition, areas_csv = "DEU+FRA+ITA+ESP") {
  url <- paste0(
    OECD_STES_BASE, "/",
    areas_csv, ".Q.B1GQ_Q.XDC._T.", edition,
    "?detail=dataonly"
  )
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  backoff <- OECD_BACKOFF_START
  status <- NA_integer_
  for (attempt in seq_len(OECD_MAX_ATTEMPTS)) {
    response <- tryCatch(
      curl::curl_fetch_disk(
        url,
        disk = tmp,
        handle = curl::new_handle(
          acceptencoding = "gzip, deflate",
          httpheader = c("Accept" = OECD_CSV_ACCEPT),
          useragent = "recession-risk research (R)"
        )
      ),
      error = function(e) NULL
    )

    if (is.null(response)) {
      status <- 0L
    } else {
      status <- response$status_code
      if (status == 200L) {
        body <- tryCatch(
          utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE),
          error = function(e) NULL
        )
        if (is.null(body) || nrow(body) == 0L) return(NULL)
        return(body)
      }
      if (status == 404L) {
        return(NULL)  # edition not (yet) published for this measure
      }
    }

    # 429 (rate limit) or transient 5xx/network error: back off and retry.
    Sys.sleep(backoff)
    backoff <- min(round(backoff * 1.6), 420)
  }

  stop(paste0(
    "OECD download failed for edition ", edition,
    " after ", OECD_MAX_ATTEMPTS, " attempts (last HTTP status ", status, ")."
  ))
}

# Download every edition in the sequence, or load the on-disk cache when it
# already covers the requested editions. The cache stores raw rows with the
# edition tag; re-runs only fetch missing editions.
download_country_gdp_vintages <- function() {
  editions <- edition_sequence()
  cached <- NULL
  have <- character()

  if (file.exists(OECD_CACHE_FILE)) {
    cached <- utils::read.csv(OECD_CACHE_FILE, stringsAsFactors = FALSE)
    have <- unique(cached$edition)
  }

  missing_editions <- setdiff(editions, have)

  if (length(missing_editions) > 0L) {
    cat(
      "Downloading", length(missing_editions),
      "OECD MEI editions (this is rate-limited and can take a while)...\n"
    )
    fresh <- list()
    for (k in seq_along(missing_editions)) {
      ed <- missing_editions[k]
      if (k > 1L) Sys.sleep(OECD_CURL_PAUSE)
      body <- tryCatch(
        fetch_oecd_edition(ed),
        error = function(e) {
          warning(conditionMessage(e))
          NULL
        }
      )
      if (is.null(body) || nrow(body) == 0L) next

      geo_lookup <- setNames(oecd_geo_map$geo, oecd_geo_map$oecd_code)
      fresh[[ed]] <- body %>%
        transmute(
          geo = unname(geo_lookup[REF_AREA]),
          edition = ed,
          quarter = as.character(TIME_PERIOD),
          gdp_level = suppressWarnings(as.numeric(OBS_VALUE))
        ) %>%
        filter(!is.na(geo), is.finite(gdp_level))

      cat("  edition", ed, "->", nrow(fresh[[ed]]), "rows\n")
    }

    fresh_all <- bind_rows(fresh)
    if (nrow(fresh_all) > 0L) {
      combined <- bind_rows(
        if (!is.null(cached)) as_tibble(cached) else NULL,
        fresh_all
      ) %>%
        distinct()
      utils::write.csv(combined, OECD_CACHE_FILE, row.names = FALSE)
      cached <- combined
    } else if (is.null(cached)) {
      stop("No OECD GDP vintages could be downloaded.")
    }
  } else {
    cat("OECD GDP vintages loaded from cache:", length(have), "editions.\n")
    cached <- as_tibble(cached)
  }

  cached %>%
    transmute(
      geo,
      vintage_date = oecd_edition_asof_date(edition),
      quarter = parse_sdmx_quarter(quarter),
      gdp_level
    ) %>%
    filter(!is.na(quarter), is.finite(gdp_level))
}

# --- 3c. Common vintage processing ---------------------------------------
# Within each vintage, compute QoQ growth from consecutive levels and the
# technical-recession label (two consecutive negative quarters).
compute_growth_and_recession <- function(vintages) {
  vintages %>%
    group_by(geo, vintage_date) %>%
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
    ungroup() %>%
    select(-previous_quarter, -consecutive_level, -previous_growth,
           -consecutive_growth)
}

###############################################################################
# 4. PREDICTOR DOWNLOADS
###############################################################################

# --- 4a. CISS (daily, all areas) ------------------------------------------
download_ciss <- function() {
  bind_rows(
    lapply(
      seq_len(nrow(ciss_geo_map)),
      function(i) {
        raw <- download_sdmx_table(
          provider = "ECB",
          id = paste0(
            "CISS/D.", ciss_geo_map$ecb_code[i], ".Z0Z.4F.EC.SS_CIN.IDX"
          ),
          label = paste0("CISS ECB - ", ciss_geo_map$ecb_code[i]),
          start = "1998-01-01",
          gregorian_time = TRUE
        )

        raw %>%
          transmute(
            geo = ciss_geo_map$geo[i],
            time = as.Date(time_period),
            ciss_daily = suppressWarnings(as.numeric(obs_value))
          ) %>%
          filter(!is.na(time), is.finite(ciss_daily))
      }
    )
  ) %>%
    arrange(geo, time)
}

# --- 4b. ESI (monthly, publication-availability as-of) --------------------
read_esi_release_overrides <- function(path) {
  if (!file.exists(path)) {
    return(
      tibble(
        month = zoo::as.yearmon(character()),
        esi_release_date_override = as.Date(character())
      )
    )
  }

  raw <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(raw) <- tolower(gsub("[^A-Za-z0-9]+", "_", names(raw)))

  required_columns <- c("month", "release_date")
  missing_columns <- setdiff(required_columns, names(raw))

  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "The ESI release-calendar file is missing columns: ",
        paste(missing_columns, collapse = ", "),
        ". Expected columns: month, release_date."
      )
    )
  }

  as_tibble(raw) %>%
    transmute(
      month = parse_sdmx_month(month),
      esi_release_date_override = as.Date(release_date)
    ) %>%
    filter(!is.na(month), !is.na(esi_release_date_override))
}

download_esi <- function() {
  release_overrides <- read_esi_release_overrides(ESI_RELEASE_CALENDAR_FILE)

  esi_monthly <- bind_rows(
    lapply(
      seq_len(nrow(esi_geo_map)),
      function(i) {
        raw <- download_sdmx_table(
          provider = "EUROSTAT",
          id = paste0(
            "ei_bssi_m_r2/M.BS-ESI-I.SA.", esi_geo_map$eurostat_code[i]
          ),
          label = paste0("ESI Eurostat - ", esi_geo_map$eurostat_code[i]),
          gregorian_time = TRUE
        )

        raw %>%
          transmute(
            geo = esi_geo_map$geo[i],
            month = parse_sdmx_month(time_period),
            esi = suppressWarnings(as.numeric(obs_value))
          ) %>%
          filter(!is.na(month), is.finite(esi))
      }
    )
  )

  esi_monthly %>%
    group_by(geo, month) %>%
    summarise(esi = mean(esi, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      esi_release_date_default = release_date_from_fixed_day(month, ESI_RELEASE_DAY)
    ) %>%
    left_join(release_overrides, by = "month") %>%
    mutate(
      esi_release_date = coalesce(
        esi_release_date_override,
        esi_release_date_default
      ),
      release_date_source = if_else(
        !is.na(esi_release_date_override),
        "calendar override",
        paste0("day ", ESI_RELEASE_DAY, " assumption")
      )
    ) %>%
    select(geo, month, esi, esi_release_date, release_date_source) %>%
    arrange(geo, month)
}

# --- 4c. PMI (S&P Global workbook, prod-only data) -------------------------
# Month labels are like "Jan98" (2-digit year); years >= 70 are 19xx.
parse_pmi_month <- function(x) {
  x <- toupper(trimws(as.character(x)))

  month_codes <- c(
    "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
    "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
  )

  month_number <- match(substr(x, 1L, 3L), month_codes)
  two_digit_year <- suppressWarnings(as.integer(substr(x, 4L, 5L)))

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

  zoo::as.yearmon(normalized, format = "%Y-%m")
}

read_pmi_workbook <- function(path) {
  pmi_sheet_map <- tribble(
    ~sheet,    ~geo,
    "CompEMU", "EA",
    "CompGER", "DE",
    "CompFRA", "FR",
    "CompITA", "IT",
    "CompSPA", "ES"
  )

  available_sheets <- readxl::excel_sheets(path)

  bind_rows(
    lapply(
      seq_len(nrow(pmi_sheet_map)),
      function(i) {
        sheet <- pmi_sheet_map$sheet[i]
        if (!sheet %in% available_sheets) {
          stop(
            paste0(
              "PMI sheet ", sheet, " not found. Available sheets: ",
              paste(available_sheets, collapse = ", ")
            )
          )
        }

        # Structure verified in production: cell B5 names the series
        # ("S&P GLOBAL PMI: COMPOSITE - OUTPUT"); data starts at row 10,
        # column A = month labels (Jan98), column B = PMI values.
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

        parsed %>%
          filter(!is.na(month), is.finite(pmi_composite)) %>%
          mutate(geo = pmi_sheet_map$geo[i]) %>%
          group_by(geo, month) %>%
          summarise(
            pmi_composite = mean(pmi_composite, na.rm = TRUE),
            .groups = "drop"
          )
      }
    )
  ) %>%
    mutate(
      month_start = as.Date(month, frac = 0),
      pmi_release_date = make_date(
        year(month_start),
        month(month_start),
        PMI_RELEASE_DAY
      )
    ) %>%
    select(geo, month, pmi_composite, pmi_release_date) %>%
    arrange(geo, month)
}

resolve_pmi_file <- function() {
  if (file.exists(network_pmi_file)) {
    return(network_pmi_file)
  }
  if (file.exists(local_pmi_file)) {
    return(local_pmi_file)
  }
  stop(
    paste0(
      "PMI workbook not found. Checked:\n",
      "  1. ", network_pmi_file, "\n",
      "  2. ", local_pmi_file, "\n",
      "The S&P Global PMI workbook cannot be copied out of the production ",
      "environment; run this script there, or place a local copy in the ",
      "working directory for testing."
    )
  )
}

###############################################################################
# 5. AS-OF PREDICTORS
###############################################################################

build_ciss_as_of <- function(ciss_daily, as_of_date) {
  cutoff <- as_of_date - days(CISS_RELEASE_LAG_DAYS)

  ciss_daily %>%
    filter(time <= cutoff) %>%
    mutate(quarter = as.yearqtr(time)) %>%
    group_by(geo, quarter) %>%
    summarise(
      ciss = mean(ciss_daily, na.rm = TRUE),
      ciss_days_available = n(),
      .groups = "drop"
    ) %>%
    filter(is.finite(ciss))
}

build_monthly_as_of <- function(monthly, value_col, release_col, as_of_date) {
  monthly %>%
    filter(.data[[release_col]] <= as_of_date) %>%
    mutate(quarter = as.yearqtr(month)) %>%
    group_by(geo, quarter) %>%
    summarise(
      value = mean(.data[[value_col]], na.rm = TRUE),
      months_available = dplyr::n_distinct(month),
      .groups = "drop"
    ) %>%
    filter(is.finite(value))
}

build_predictors_as_of <- function(ciss_daily, esi_monthly, pmi_monthly,
                                   as_of_date) {
  ciss_q <- build_ciss_as_of(ciss_daily, as_of_date)

  esi_q <- build_monthly_as_of(esi_monthly, "esi", "esi_release_date", as_of_date)
  if (REQUIRE_COMPLETE_MONTHLY_QUARTER) {
    esi_q <- esi_q %>% filter(months_available == 3L)
  }
  esi_q <- esi_q %>% transmute(geo, quarter, esi = value)

  pmi_q <- build_monthly_as_of(
    pmi_monthly, "pmi_composite", "pmi_release_date", as_of_date
  )
  if (REQUIRE_COMPLETE_MONTHLY_QUARTER) {
    pmi_q <- pmi_q %>% filter(months_available == 3L)
  }
  pmi_q <- pmi_q %>% transmute(geo, quarter, pmi = value)

  list(ciss = ciss_q, esi = esi_q, pmi = pmi_q)
}

###############################################################################
# 6. EVENT GRID AND AS-OF GDP PANELS
###############################################################################

# Latest GDP vintage available for each area at the event date:
#   countries -> latest OECD edition with as-of date <= event date;
#   EA        -> latest Eurostat vintage date <= event date.
gdp_panel_as_of <- function(gdp_vintages_processed, as_of_date) {
  gdp_vintages_processed %>%
    group_by(geo) %>%
    filter(vintage_date <= as_of_date) %>%
    filter(vintage_date == max(vintage_date)) %>%
    ungroup() %>%
    select(geo, quarter, gdp_growth, recession)
}

# Per-area latest quarter with a usable recession label as of the event.
latest_target_quarters <- function(panel) {
  panel %>%
    filter(!is.na(recession), is.finite(gdp_growth)) %>%
    group_by(geo) %>%
    summarise(
      target_quarter = max(quarter),
      gdp_growth_vintage = gdp_growth[which.max(quarter)],
      recession_vintage = recession[which.max(quarter)],
      .groups = "drop"
    )
}

# Event dates: OECD edition as-of dates (>= EVAL_START) at which at least one
# area's latest GDP quarter is new. Built by walking the edition sequence and
# tracking each area's latest quarter in the as-of panel (+ EA from Eurostat
# vintages as-of the edition date).
build_event_grid <- function(gdp_vintages_processed) {
  edition_dates <- gdp_vintages_processed %>%
    filter(geo != "EA") %>%
    distinct(vintage_date) %>%
    arrange(vintage_date) %>%
    filter(vintage_date >= EVAL_START) %>%
    pull(vintage_date)

  prev_latest <- setNames(numeric(0), character())
  event_rows <- list()
  j <- 0L

  # Iterate by INDEX: a `for (d in dates)` loop strips the Date class.
  for (k in seq_along(edition_dates)) {
    d <- edition_dates[k]
    panel <- gdp_panel_as_of(gdp_vintages_processed, d)
    targets <- latest_target_quarters(panel)
    latest_key <- setNames(
      as.numeric(targets$target_quarter), targets$geo
    )

    prev_vals <- prev_latest[targets$geo]
    prev_vals[is.na(prev_vals)] <- -Inf
    new_geo <- targets$geo[latest_key > prev_vals]

    if (length(new_geo) > 0L) {
      j <- j + 1L
      event_rows[[j]] <- tibble(event_date = d) %>%
        bind_cols(targets %>% filter(geo %in% new_geo))
    }

    prev_latest <- latest_key
  }

  bind_rows(event_rows)
}

###############################################################################
# 7. ONE PSEUDO-REAL-TIME RUN (both approaches, hierarchical model)
###############################################################################

# Gap design: for each area, training rows satisfy
# quarter <= target_quarter(area) - TRAINING_GAP_YEARS.
training_cutoff_quarter <- function(target_quarter,
                                    gap_years = TRAINING_GAP_YEARS) {
  target_quarter - gap_years
}

fit_one_event <- function(gdp_vintages_processed, predictors, event_date,
                          approach) {
  panel <- gdp_panel_as_of(gdp_vintages_processed, event_date)
  targets <- latest_target_quarters(panel)

  sentiment_col <- if (approach == "ESI") "esi" else "pmi"
  sentiment_q <- if (approach == "ESI") predictors$esi else predictors$pmi

  model_data <- panel %>%
    inner_join(predictors$ciss, by = c("geo", "quarter")) %>%
    inner_join(sentiment_q, by = c("geo", "quarter")) %>%
    filter(!is.na(recession), is.finite(gdp_growth)) %>%
    arrange(geo, quarter)

  # Per-area gap cutoff: training quarter <= area's target - 5 years.
  cutoffs <- targets %>%
    transmute(geo, cutoff_quarter = training_cutoff_quarter(target_quarter))

  training_data <- model_data %>%
    inner_join(cutoffs, by = "geo") %>%
    filter(quarter <= cutoff_quarter)

  target_data <- model_data %>%
    semi_join(targets, by = c("geo", "quarter" = "target_quarter"))

  if (nrow(target_data) == 0L) {
    stop(paste0(
      "No target rows for ", approach, " at event ", format(event_date), "."
    ))
  }

  if (nrow(training_data) < MIN_TRAINING_OBS) {
    stop(paste0(
      "Only ", nrow(training_data), " training rows (< ", MIN_TRAINING_OBS,
      ") for ", approach, " at event ", format(event_date), "."
    ))
  }

  recession_events <- sum(training_data$recession == 1L)
  non_recession_events <- sum(training_data$recession == 0L)

  if (recession_events < MIN_RECESSION_EVENTS ||
      non_recession_events < MIN_RECESSION_EVENTS) {
    stop(paste0(
      "Insufficient outcome variation for ", approach, " at event ",
      format(event_date), ": ", recession_events, " recession rows, ",
      non_recession_events, " non-recession rows."
    ))
  }

  ciss_scale <- safe_scale_parameters(training_data$ciss, "CISS")
  sentiment_scale <- safe_scale_parameters(
    training_data[[sentiment_col]],
    toupper(sentiment_col)
  )

  scale_row <- function(df) {
    df %>%
      mutate(
        ciss_scaled = (ciss - ciss_scale$mean) / ciss_scale$sd,
        sentiment_scaled =
          (.data[[sentiment_col]] - sentiment_scale$mean) / sentiment_scale$sd
      )
  }

  training_scaled <- scale_row(training_data)
  target_scaled <- scale_row(target_data)

  model_formula <- brmsformula(
    recession ~ ciss_scaled + sentiment_scaled +
      (1 + ciss_scaled + sentiment_scaled | geo),
    family = bernoulli(link = "logit")
  )

  model_file_stub <- file.path(
    MODEL_CACHE_DIR,
    paste0(
      "esi_vs_pmi_", tolower(approach), "_",
      format(event_date, "%Y%m%d")
    )
  )

  brm_arguments <- list(
    formula = model_formula,
    data = training_scaled,
    prior = c(
      prior(normal(0, 5), class = "Intercept"),
      prior(normal(0, 2), class = "b"),
      prior(normal(0, 2), class = "sd"),
      prior(lkj(2), class = "cor")
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

  draws <- posterior_epred(fitted_model, newdata = target_scaled)
  if (length(dim(draws)) == 3L) draws <- draws[, , 1L, drop = TRUE]
  prob_by_row <- colMeans(draws)
  lower_by_row <- apply(draws, 2L, quantile, probs = 0.025, names = FALSE)
  upper_by_row <- apply(draws, 2L, quantile, probs = 0.975, names = FALSE)

  target_scaled %>%
    transmute(
      approach = approach,
      event_date = as.Date(event_date),
      target_quarter = quarter,
      geo,
      gdp_growth_vintage = gdp_growth,
      recession_vintage = recession,
      ciss_as_of = ciss,
      sentiment_as_of = .data[[sentiment_col]],
      training_observations = nrow(training_scaled),
      training_recessions = recession_events,
      prob_recession = prob_by_row,
      lower_95 = lower_by_row,
      upper_95 = upper_by_row,
      status = "ok",
      error_message = NA_character_
    )
}

###############################################################################
# 8. RUN THE BACKTEST
###############################################################################

cat("\n=== Downloading GDP vintage archives ===\n")

ea_vintages_raw <- download_ea_gdp_vintages()
cat("Eurostat EA vintages:", n_distinct(ea_vintages_raw$vintage_date), "dates\n")

country_vintages_raw <- download_country_gdp_vintages()

gdp_vintages_raw <- bind_rows(
  ea_vintages_raw,
  country_vintages_raw
)

gdp_vintages <- compute_growth_and_recession(gdp_vintages_raw)

if (nrow(gdp_vintages) == 0) {
  stop("No usable GDP-vintage observations were downloaded.")
}

cat("\nGDP vintage coverage by geo:\n")
print(
  gdp_vintages %>%
    group_by(geo) %>%
    summarise(
      first_vintage = min(vintage_date),
      last_vintage = max(vintage_date),
      vintage_dates = n_distinct(vintage_date),
      first_quarter = min(quarter),
      last_quarter = max(quarter),
      .groups = "drop"
    )
)

event_grid <- build_event_grid(gdp_vintages)

cat("\nPseudo-real-time events (as-of dates with a new GDP quarter):\n")
print(
  event_grid %>%
    group_by(event_date) %>%
    summarise(new_targets = paste(geo, collapse = ","), .groups = "drop") %>%
    summarise(n_events = n(), first = min(event_date), last = max(event_date))
)
cat("Distinct event dates:", n_distinct(event_grid$event_date), "\n")

cat("\n=== Downloading predictors ===\n")

ciss_daily <- download_ciss()
esi_monthly <- download_esi()
pmi_file <- resolve_pmi_file()
pmi_monthly <- read_pmi_workbook(pmi_file)

cat("\nPredictor coverage (first/last observation):\n")
print(
  bind_rows(
    ciss_daily %>% transmute(series = "ciss", geo, dt = as.character(time)),
    esi_monthly %>% transmute(series = "esi", geo, dt = as.character(month)),
    pmi_monthly %>% transmute(series = "pmi", geo, dt = as.character(month))
  ) %>%
    group_by(series, geo) %>%
    summarise(first = min(dt), last = max(dt), n = n(), .groups = "drop")
)

event_dates <- sort(unique(event_grid$event_date))
cat("\nPseudo-real-time model runs (event dates):", length(event_dates), "\n")

run_one_event <- function(event_date) {
  predictors <- build_predictors_as_of(
    ciss_daily, esi_monthly, pmi_monthly, event_date
  )

  lapply(c("ESI", "PMI"), function(approach) {
    tryCatch(
      fit_one_event(gdp_vintages, predictors, event_date, approach),
      error = function(e) {
        event_targets <- latest_target_quarters(
          gdp_panel_as_of(gdp_vintages, event_date)
        )
        event_targets %>%
          transmute(
            approach = approach,
            event_date = as.Date(event_date),
            target_quarter,
            geo,
            gdp_growth_vintage,
            recession_vintage,
            ciss_as_of = NA_real_,
            sentiment_as_of = NA_real_,
            training_observations = NA_integer_,
            training_recessions = NA_integer_,
            prob_recession = NA_real_,
            lower_95 = NA_real_,
            upper_95 = NA_real_,
            status = "failed",
            error_message = conditionMessage(e)
          )
      }
    )
  }) %>%
    bind_rows()
}

backtest_all <- bind_rows(
  lapply(event_dates, function(d) {
    cat("  [", format(d), "] fitting ESI + PMI hierarchical models\n", sep = "")
    tryCatch(
      run_one_event(d),
      error = function(e) {
        warning(paste0("Event ", format(d), " failed: ", conditionMessage(e)))
        NULL
      }
    )
  })
)

# One nowcast per (approach, geo, quarter): keep the FIRST event at which the
# quarter was the area's latest (its first nowcast opportunity).
backtest_results <- backtest_all %>%
  filter(status == "ok", is.finite(prob_recession)) %>%
  group_by(approach, geo, target_quarter) %>%
  arrange(event_date, .by_group = TRUE) %>%
  slice_head(n = 1L) %>%
  ungroup()

if (nrow(backtest_results) == 0) {
  write.csv(
    backtest_all %>% mutate(target_quarter = format(target_quarter, "%Y-Q%q")),
    file.path(OUTPUT_DIR, "pseudo_realtime_all_runs_including_failures.csv"),
    row.names = FALSE, na = ""
  )
  stop("All pseudo-real-time runs failed; inspect the failures CSV.")
}

cat(
  "\nSuccessful nowcasts per approach:\n",
  paste(
    capture.output(print(count(backtest_results, approach, geo))),
    collapse = "\n"
  ),
  "\n"
)

###############################################################################
# 9. COMPARISON METRICS (per area and aggregate)
###############################################################################

metrics_table <- bind_rows(
  lapply(sort(unique(backtest_results$approach)), function(ap) {
    df <- backtest_results %>% filter(approach == ap)
    bind_rows(
      lapply(sort(unique(df$geo)), function(g) {
        score_probabilities(df %>% filter(geo == g), "recession_vintage", g)
      }),
      score_probabilities(df, "recession_vintage", "Pooled (all areas)")
    ) %>%
      mutate(approach = ap, .after = scope)
  })
)

cat("\n=== Real-time scores by approach and area ===\n")
print(metrics_table)

###############################################################################
# 10. DIEBOLD-MARIANO TESTS (per area and pooled)
###############################################################################

# Align the two approaches on (geo, target_quarter): loss differentials must
# compare the SAME events.
paired <- backtest_results %>%
  select(approach, geo, target_quarter, recession_vintage, prob_recession) %>%
  pivot_wider(
    names_from = approach,
    values_from = prob_recession
  ) %>%
  filter(is.finite(ESI), is.finite(PMI)) %>%
  arrange(geo, target_quarter)

if (nrow(paired) == 0) {
  stop("No common (geo, quarter) events between the ESI and PMI approaches.")
}

cat("\nPaired evaluation events:", nrow(paired), "\n")

dm_tests <- bind_rows(
  lapply(sort(unique(paired$geo)), function(g) {
    sub <- paired %>% filter(geo == g) %>% arrange(target_quarter)
    bind_rows(
      diebold_mariano_test(
        brier_losses(sub$recession_vintage, sub$ESI),
        brier_losses(sub$recession_vintage, sub$PMI),
        lag.order = 1L
      ),
      diebold_mariano_test(
        log_losses(sub$recession_vintage, sub$ESI),
        log_losses(sub$recession_vintage, sub$PMI),
        lag.order = 1L
      )
    ) %>%
      mutate(scope = g, .before = 1)
  }),
  diebold_mariano_test(
    brier_losses(paired$recession_vintage, paired$ESI),
    brier_losses(paired$recession_vintage, paired$PMI),
    lag.order = 1L
  ) %>% mutate(scope = "Pooled (all areas)", .before = 1),
  diebold_mariano_test(
    log_losses(paired$recession_vintage, paired$ESI),
    log_losses(paired$recession_vintage, paired$PMI),
    lag.order = 1L
  ) %>% mutate(scope = "Pooled (all areas)", .before = 1)
) %>%
  mutate(
    loss = rep(c("squared (Brier)", "log"), length.out = n()),
    loss1_label = "ESI",
    loss2_label = "PMI",
    better = case_when(
      is.na(p_value) ~ NA_character_,
      p_value < 0.10 & mean_loss_diff > 0 ~ "PMI better (10% level)",
      p_value < 0.10 & mean_loss_diff < 0 ~ "ESI better (10% level)",
      TRUE ~ "No significant difference"
    )
  )

cat("\n=== Diebold-Mariano tests (HAC lag 1, Harvey-corrected) ===\n")
print(dm_tests, n = Inf)

###############################################################################
# 11. EXPORT
###############################################################################

write.csv(
  backtest_all %>%
    mutate(target_quarter = format(target_quarter, "%Y-Q%q")),
  file.path(OUTPUT_DIR, "pseudo_realtime_all_runs_including_failures.csv"),
  row.names = FALSE, na = ""
)

write.csv(
  backtest_results %>%
    mutate(target_quarter = format(target_quarter, "%Y-Q%q")) %>%
    select(-status, -error_message) %>%
    arrange(approach, geo, target_quarter),
  file.path(OUTPUT_DIR, "pseudo_realtime_nowcasts.csv"),
  row.names = FALSE, na = ""
)

write.csv(
  metrics_table,
  file.path(OUTPUT_DIR, "comparison_metrics_by_area.csv"),
  row.names = FALSE, na = ""
)

write.csv(
  dm_tests,
  file.path(OUTPUT_DIR, "diebold_mariano_tests.csv"),
  row.names = FALSE, na = ""
)

write.csv(
  paired %>%
    mutate(target_quarter = format(target_quarter, "%Y-Q%q")) %>%
    select(geo, target_quarter, recession_vintage, ESI, PMI),
  file.path(OUTPUT_DIR, "paired_probabilities.csv"),
  row.names = FALSE, na = ""
)

write.csv(
  event_grid %>% mutate(target_quarter = format(target_quarter, "%Y-Q%q")),
  file.path(OUTPUT_DIR, "event_grid.csv"),
  row.names = FALSE, na = ""
)

saveRDS(
  list(
    backtest_results = backtest_results,
    metrics_table = metrics_table,
    dm_tests = dm_tests,
    paired = paired,
    event_grid = event_grid
  ),
  file.path(OUTPUT_DIR, "pseudo_realtime_ESI_vs_PMI_results.rds")
)

capture.output(sessionInfo(), file = file.path(OUTPUT_DIR, "session_info.txt"))

###############################################################################
# 12. VISUALIZATION
###############################################################################

plot_data <- backtest_results %>%
  mutate(date = as.Date(target_quarter))

recession_rects <- plot_data %>%
  distinct(approach, geo, target_quarter, recession_vintage) %>%
  filter(recession_vintage == 1L) %>%
  transmute(
    geo,
    xmin = as.Date(target_quarter),
    xmax = as.Date(target_quarter + 0.25) - 1,
    ymin = 0, ymax = 1
  )

approach_colours <- c(ESI = "#0072B2", PMI = "#D55E00")

area_plot <- function(g) {
  ggplot(
    plot_data %>% filter(geo == g),
    aes(x = date, y = prob_recession, colour = approach)
  ) +
    geom_rect(
      data = recession_rects %>% filter(geo == g),
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = "grey80", alpha = 0.55
    ) +
    geom_ribbon(
      aes(ymin = lower_95, ymax = upper_95, fill = approach),
      alpha = 0.12, colour = NA
    ) +
    geom_line(linewidth = 0.85) +
    scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
    scale_colour_manual(values = approach_colours) +
    scale_fill_manual(values = approach_colours) +
    labs(title = g, x = NULL, y = "Recession probability") +
    theme_minimal() +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
}

p_ea <- area_plot("EA")
p_de <- area_plot("DE")
p_fr <- area_plot("FR")
p_it <- area_plot("IT")
p_es <- area_plot("ES")

country_grid <- (p_de | p_fr) / (p_it | p_es) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

final_plot <- p_ea / country_grid + plot_layout(heights = c(1, 1.4))

ggsave(
  filename = file.path(OUTPUT_DIR, "esi_vs_pmi_realtime_probabilities.png"),
  plot = final_plot,
  width = 13, height = 10, dpi = 300
)

cat("\nCompleted. Outputs saved in:\n  ", OUTPUT_DIR, "\n", sep = "")
