# Shared I/O for the S&P Global composite-PMI workbook (production-only data).
#
# Sourced by new_code/update_dashboard_data.R; the implementation is the one
# validated in production by recessione_CISS_PMI_composito.R and unit-tested
# against a synthetic workbook in new_code/tests/ (exact layout: cell B5
# names the series, data starts at row 10, column A = month labels "Jan98",
# column B = PMI values). The licensed workbook must never leave the
# production environment; only derived model outputs may be committed.

# Month labels like "Jan98" (2-digit year); years >= 70 are 19xx.
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

read_pmi_workbook <- function(path, pmi_release_day = 26L) {
  pmi_sheet_map <- tibble::tribble(
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
        pmi_release_day
      )
    ) %>%
    select(geo, month, pmi_composite, pmi_release_date) %>%
    arrange(geo, month)
}

resolve_pmi_file <- function(network_pmi_file, local_pmi_file) {
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
