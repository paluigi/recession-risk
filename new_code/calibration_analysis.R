# Post-hoc calibration analysis for the pseudo-real-time ESI vs PMI backtest.
#
# Reads new_code/pseudo_realtime_ESI_vs_PMI_output/paired_probabilities.csv
# (produced by pseudo_realtime_ESI_vs_PMI.R — no model re-runs needed) and
# computes, per approach and per area plus pooled:
#   * a reliability table on fixed-width probability bins (0.1): mean
#     predicted probability vs observed recession frequency;
#   * the Murphy decomposition of the Brier score into reliability (miscalibration),
#     resolution (discrimination) and uncertainty (base-rate) components;
#   * a logistic recalibration fit (calibration intercept + slope on the
#     logit scale; perfect calibration = intercept 0, slope 1).
#
# Usage (after the backtest results are in the repo):
#   Rscript --vanilla new_code/calibration_analysis.R
# Optional environment overrides:
#   CALIBRATION_INPUT   path to paired_probabilities.csv
#   CALIBRATION_OUTPUT  output directory (default: the backtest output dir)

###############################################################################
# 1. SETTINGS
###############################################################################

suppressPackageStartupMessages(library(dplyr))

default_input <- file.path(
  getwd(), "pseudo_realtime_ESI_vs_PMI_output", "paired_probabilities.csv"
)
repo_root_input <- file.path(
  dirname(getwd()), "new_code", "pseudo_realtime_ESI_vs_PMI_output",
  "paired_probabilities.csv"
)

CALIBRATION_INPUT <- Sys.getenv(
  "CALIBRATION_INPUT",
  if (file.exists(default_input)) default_input else repo_root_input
)

CALIBRATION_OUTPUT <- Sys.getenv(
  "CALIBRATION_OUTPUT",
  dirname(CALIBRATION_INPUT)
)

PROB_BIN_WIDTH <- 0.1
PROB_BIN_EDGES <- seq(0, 1, by = PROB_BIN_WIDTH)

###############################################################################
# 2. PURE HELPERS (unit-tested offline in new_code/tests/)
###############################################################################

assign_prob_bin <- function(p, edges = PROB_BIN_EDGES) {
  bins <- cut(
    p,
    breaks = edges,
    include.lowest = TRUE,
    labels = sprintf("[%s,%s)", head(edges, -1), tail(edges, -1))
  )
  # The top bin [0.9,1] would print as "[0.9,1)" although it includes 1.
  levels(bins)[length(levels(bins))] <-
    sprintf("[%s,%s]", tail(edges, 2)[1], tail(edges, 1))
  bins
}

# Fixed-width reliability table: bin, n, mean predicted, observed frequency.
reliability_by_bin <- function(prob, outcome, edges = PROB_BIN_EDGES) {
  check_lengths <- length(prob) == length(outcome)
  if (!check_lengths) {
    stop("reliability_by_bin: prob and outcome lengths differ.")
  }
  keep <- is.finite(prob) & !is.na(outcome)
  tibble::tibble(
    bin = assign_prob_bin(prob[keep], edges),
    n = rep(1L, sum(keep)),
    mean_predicted = prob[keep],
    observed = outcome[keep]
  ) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(
      n = sum(n),
      mean_predicted = mean(mean_predicted),
      observed_frequency = mean(observed),
      .groups = "drop"
    )
}

# Murphy (1973) decomposition of the Brier score on the SAME fixed bins:
#   Brier = reliability - resolution + uncertainty
brier_decomposition <- function(prob, outcome, edges = PROB_BIN_EDGES) {
  keep <- is.finite(prob) & !is.na(outcome)
  p <- prob[keep]
  y <- as.numeric(outcome)[keep]
  n <- length(p)
  if (n < 2L) {
    return(tibble::tibble(
      brier = NA_real_, reliability = NA_real_, resolution = NA_real_,
      uncertainty = NA_real_, n = as.integer(n)
    ))
  }
  bins <- assign_prob_bin(p, edges)
  o_bar <- mean(y)
  parts <- dplyr::tibble(bin = bins, p = p, y = y) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(
      n_k = dplyr::n(),
      p_bar = mean(p),
      o_bar_k = mean(y),
      .groups = "drop"
    )
  reliability <- sum(parts$n_k * (parts$p_bar - parts$o_bar_k)^2) / n
  resolution <- sum(parts$n_k * (parts$o_bar_k - o_bar)^2) / n
  uncertainty <- o_bar * (1 - o_bar)
  brier <- mean((p - y)^2)
  tibble::tibble(
    brier = brier,
    reliability = reliability,
    resolution = resolution,
    uncertainty = uncertainty,
    n = as.integer(n)
  )
}

# Logistic recalibration: outcome ~ logit(prob). Perfect calibration is
# intercept 0 / slope 1; returns NA rows when the fit degenerates.
calibration_slope_intercept <- function(prob, outcome,
                                        eps = sqrt(.Machine$double.eps)) {
  keep <- is.finite(prob) & !is.na(outcome)
  p <- pmin(pmax(prob[keep], eps), 1 - eps)
  y <- as.numeric(outcome)[keep]
  logit_p <- stats::qlogis(p)
  fit <- tryCatch(
    stats::glm(y ~ logit_p, family = stats::binomial()),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    return(tibble::tibble(
      intercept = NA_real_, slope = NA_real_, n = as.integer(length(y))
    ))
  }
  coef <- stats::coef(fit)
  tibble::tibble(
    intercept = unname(coef["(Intercept)"]),
    slope = unname(coef["logit_p"]),
    n = as.integer(length(y))
  )
}

###############################################################################
# 3. EXECUTION
###############################################################################

cat("\n=== Calibration analysis ===\n")

if (!file.exists(CALIBRATION_INPUT)) {
  stop(
    "paired_probabilities.csv not found at: ", CALIBRATION_INPUT, "\n",
    "Run new_code/pseudo_realtime_ESI_vs_PMI.R first (see ",
    "new_code/PROD_RUN_ESI_VS_PMI.md) and copy the results back."
  )
}

paired <- utils::read.csv(CALIBRATION_INPUT, stringsAsFactors = FALSE)

required_cols <- c("geo", "target_quarter", "recession_vintage", "ESI", "PMI")
missing_cols <- setdiff(required_cols, names(paired))
if (length(missing_cols) > 0) {
  stop("paired_probabilities.csv missing columns: ",
       paste(missing_cols, collapse = ", "))
}

approaches <- c(ESI = "ESI", PMI = "PMI")

rows_decomp <- list()
rows_reliab <- list()
rows_slope <- list()
k <- 0L

for (approach in approaches) {
  prob_col <- paired[[approach]]
  for (g in c("pooled", sort(unique(paired$geo)))) {
    sel <- if (g == "pooled") rep(TRUE, nrow(paired)) else paired$geo == g
    k <- k + 1L
    rows_decomp[[k]] <- brier_decomposition(
      prob_col[sel], paired$recession_vintage[sel]
    ) %>%
      dplyr::mutate(approach = approach, geo = g, .before = 1)
    rows_slope[[k]] <- calibration_slope_intercept(
      prob_col[sel], paired$recession_vintage[sel]
    ) %>%
      dplyr::mutate(approach = approach, geo = g, .before = 1)
    rel <- reliability_by_bin(
      prob_col[sel], paired$recession_vintage[sel]
    )
    rows_reliab[[k]] <- rel %>%
      dplyr::mutate(approach = approach, geo = g, .before = 1)
  }
}

decomposition <- dplyr::bind_rows(rows_decomp)
slope_table <- dplyr::bind_rows(rows_slope)
reliability <- dplyr::bind_rows(rows_reliab)

dir.create(CALIBRATION_OUTPUT, recursive = TRUE, showWarnings = FALSE)

write.csv(
  decomposition,
  file.path(CALIBRATION_OUTPUT, "calibration_decomposition.csv"),
  row.names = FALSE, na = ""
)
write.csv(
  slope_table,
  file.path(CALIBRATION_OUTPUT, "calibration_slope.csv"),
  row.names = FALSE, na = ""
)
write.csv(
  reliability,
  file.path(CALIBRATION_OUTPUT, "calibration_reliability_bins.csv"),
  row.names = FALSE, na = ""
)

cat("\nBrier decomposition (Murphy):\n")
print(decomposition, n = Inf)
cat("\nCalibration intercept/slope (logit scale; 0/1 = perfect):\n")
print(slope_table, n = Inf)

# Reliability diagram (one panel per approach, pooled + areas).
if (requireNamespace("ggplot2", quietly = TRUE) &&
    requireNamespace("patchwork", quietly = TRUE) &&
    requireNamespace("scales", quietly = TRUE)) {  geos <- c("pooled", sort(setdiff(unique(paired$geo), "pooled")))
  plot_one <- function(approach) {
    panels <- lapply(geos, function(g) {
      d <- reliability %>%
        dplyr::filter(approach == !!approach, geo == !!g) %>%
        dplyr::mutate(mid = as.numeric(sub("\\[(.*),.*", "\\1", bin)) +
                        PROB_BIN_WIDTH / 2)
      ggplot2::ggplot(d, ggplot2::aes(x = mid)) +
        ggplot2::geom_segment(
          x = 0, y = 0, xend = 1, yend = 1,
          linetype = "dashed", colour = "grey55",
          inherit.aes = FALSE
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = observed_frequency, size = n),
          colour = "#004B87"
        ) +
        ggplot2::scale_x_continuous(limits = c(0, 1)) +
        ggplot2::scale_y_continuous(limits = c(0, 1),
                                    labels = scales::percent_format()) +
        ggplot2::labs(title = g, x = "Mean predicted", y = "Observed") +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none")
    })
    patchwork::wrap_plots(panels, ncol = 3) +
      patchwork::plot_annotation(
        title = paste0("Reliability diagram — ", approach, " arm")
      )
  }
  p_esi <- plot_one("ESI")
  p_pmi <- plot_one("PMI")
  combined <- p_esi / p_pmi
  ggplot2::ggsave(
    file.path(CALIBRATION_OUTPUT, "calibration_reliability.png"),
    combined, width = 12, height = 8, dpi = 150
  )
  cat("\nReliability diagram: calibration_reliability.png\n")
} else {
  cat("\nSkipping reliability diagram: ggplot2/patchwork/scales not all ",
      "installed.\n", sep = "")
}

cat("\nDone. Outputs in ", CALIBRATION_OUTPUT, "\n", sep = "")
