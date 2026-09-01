# Unit tests for new_code/pseudo_realtime_ESI_vs_PMI.R helper functions.
#
# Runs WITHOUT brms/RJSDMX/Java: only sections 1-7 definitions that do not
# require the heavy dependencies are sourced. The test harness extracts the
# function definitions from the script by sourcing it with the heavy parts
# stubbed (see tests/test_helpers_for_tests.R).
#
# Usage:  Rscript --vanilla new_code/tests/test_pseudo_realtime_helpers.R

library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(zoo)

# ---- load helpers from the main script -------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a

# Locate the main script whether run from the repo root or the tests dir.
find_script <- function() {
  candidates <- c(
    file.path(getwd(), "new_code", "pseudo_realtime_ESI_vs_PMI.R"),
    file.path(getwd(), "pseudo_realtime_ESI_vs_PMI.R"),
    file.path(getwd(), "..", "pseudo_realtime_ESI_vs_PMI.R")
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0L) stop("Cannot locate pseudo_realtime_ESI_vs_PMI.R")
  normalizePath(hit[1])
}

script_path <- find_script()

# Silently capture warnings/errors while sourcing helper defs
source_helpers <- function(path) {
  exprs <- parse(path)
  env <- new.env(parent = globalenv())
  # Stub out heavy library calls and downloads before they run.
  assign("library", function(...) invisible(NULL), envir = env)
  assign("require", function(...) invisible(NULL), envir = env)
  assign("install.packages", function(...) invisible(NULL), envir = env)
  assign("Sys.which", function(x) "/usr/bin/java", envir = env)
  assign("download_ea_gdp_vintages", function() stop("stub"), envir = env)
  assign("download_country_gdp_vintages", function() stop("stub"), envir = env)
  assign("download_ciss", function() stop("stub"), envir = env)
  assign("download_esi", function() stop("stub"), envir = env)
  assign("resolve_pmi_file", function() stop("stub"), envir = env)
  assign("read_pmi_workbook", function(...) stop("stub"), envir = env)
  assign("dir.create", function(...) invisible(TRUE), envir = env)
  assign("set.seed", function(...) invisible(NULL), envir = env)

  # Evaluate expressions one at a time; stop right before the first
  # expression that would execute the backtest (section 8 header marker).
  for (i in seq_along(exprs)) {
    deparse_first <- deparse(exprs[[i]][[1]], nlines = 1)[1]
    if (identical(deparse_first, "cat")) {
      break  # reached "cat(\"\\n=== Downloading GDP vintage archives ===\\n\")"
    }
    eval(exprs[[i]], envir = env)
  }
  env
}

# ---- tiny assertion framework ----------------------------------------------
n_pass <- 0L
n_fail <- 0L
failures <- character()

check <- function(label, expr) {
  ok <- tryCatch(
    isTRUE(all.equal(expr, TRUE, check.attributes = FALSE)),
    error = function(e) FALSE
  )
  if (ok) {
    n_pass <<- n_pass + 1L
  } else {
    n_fail <<- n_fail + 1L
    failures <<- c(failures, label)
    cat("FAIL:", label, "\n")
  }
  invisible(ok)
}

check_equal <- function(label, got, expected) {
  ok <- tryCatch(
    {
      isTRUE(all.equal(got, expected, check.attributes = FALSE)) ||
        (is.numeric(got) && is.numeric(expected) &&
           max(abs(got - expected)) < 1e-8)
    },
    error = function(e) FALSE
  )
  if (ok) {
    n_pass <<- n_pass + 1L
  } else {
    n_fail <<- n_fail + 1L
    failures <<- c(failures, label)
    cat("FAIL:", label, "\n  got:", paste(head(capture.output(str(got)), 3), collapse = " "),
        "\n  expected:", paste(head(capture.output(str(expected)), 3), collapse = " "), "\n")
  }
  invisible(ok)
}

check_error <- function(label, expr) {
  ok <- tryCatch({ expr; FALSE }, error = function(e) TRUE)
  if (ok) {
    n_pass <<- n_pass + 1L
  } else {
    n_fail <<- n_fail + 1L
    failures <<- c(failures, label)
    cat("FAIL (no error raised):", label, "\n")
  }
  invisible(ok)
}

###############################################################################
# TESTS
###############################################################################

env <- source_helpers(script_path)

# --- parse_sdmx_quarter ------------------------------------------------------
q <- env$parse_sdmx_quarter(c("2025-Q3", "2025Q3", "2025-07-01", "junk", NA))
check_equal("parse_sdmx_quarter values", as.numeric(q[1:3]), rep(as.numeric(zoo::as.yearqtr("2025 Q3")), 3))
check("parse_sdmx_quarter NA handling", is.na(q[4]) && is.na(q[5]))

# --- parse_sdmx_month --------------------------------------------------------
m <- env$parse_sdmx_month(c("2026-04", "2026-04-01", "bad", NA))
check_equal("parse_sdmx_month values", as.numeric(m[1:2]), rep(as.numeric(zoo::as.yearmon("2026-04")), 2))
check("parse_sdmx_month NA handling", is.na(m[3]) && is.na(m[4]))

# --- is_consecutive_quarter ---------------------------------------------------
check("is_consecutive_quarter yes", env$is_consecutive_quarter(zoo::as.yearqtr("2025 Q2"), zoo::as.yearqtr("2025 Q1")))
check("is_consecutive_quarter gap", !env$is_consecutive_quarter(zoo::as.yearqtr("2025 Q3"), zoo::as.yearqtr("2025 Q1")))
check("is_consecutive_quarter NA gives FALSE", !env$is_consecutive_quarter(zoo::as.yearqtr("2025 Q3"), zoo::as.yearqtr(NA)))

# --- release_date_from_fixed_day ----------------------------------------------
check_equal("release day clamp Feb", env$release_date_from_fixed_day(zoo::as.yearmon("2024-02"), 29), as.Date("2024-02-29"))
check_equal("release day normal", env$release_date_from_fixed_day(zoo::as.yearmon("2024-06"), 26), as.Date("2024-06-26"))
check_equal("release day clamp 31-month", env$release_date_from_fixed_day(zoo::as.yearmon("2024-04"), 31), as.Date("2024-04-30"))

# --- clamp_probability ---------------------------------------------------------
check_equal("clamp lower", env$clamp_probability(c(0, 0.5, 1)), c(1e-6, 0.5, 1 - 1e-6))

# --- binary_auc ----------------------------------------------------------------
check_equal("binary_auc perfect", env$binary_auc(c(0, 0, 1, 1), c(0.1, 0.2, 0.8, 0.9)), 1)
check_equal("binary_auc random", env$binary_auc(c(0, 1, 0, 1), c(0.4, 0.6, 0.6, 0.4)), 0.5)
check("binary_auc single class NA", is.na(env$binary_auc(c(1, 1), c(0.3, 0.7))))

# --- score_probabilities ---------------------------------------------------------
df <- tibble(
  prob_recession = c(0.8, 0.2, 0.6, 0.1),
  recession_vintage = c(1L, 0L, 0L, 0L)
)
sc <- env$score_probabilities(df, "recession_vintage", "t")
check_equal("brier", sc$brier_score, mean(c(0.04, 0.04, 0.36, 0.01)))
check_equal("log score", sc$log_score,
            -mean(c(log(0.8), log(0.8), log(0.4), log(0.9))))
check_equal("recession rate", sc$recession_rate, 0.25)
check_equal("n obs", sc$observations, 4L)

# --- brier_losses / log_losses ----------------------------------------------------
check_equal("brier_losses", env$brier_losses(c(1, 0), c(0.7, 0.7)), c(0.09, 0.49))
ll <- env$log_losses(c(1, 0), c(0.7, 0.7))
check_equal("log_losses", ll, c(-log(0.7), -log(0.3)))

# --- diebold_mariano_test --------------------------------------------------------
set.seed(42)
l1 <- c(0.20, 0.15, 0.30, 0.10, 0.25, 0.18)
l2 <- c(0.10, 0.14, 0.28, 0.12, 0.22, 0.19)
dm <- env$diebold_mariano_test(l1, l2, lag.order = 1L)
check("dm returns finite stat", is.finite(dm$dm_statistic))
check("dm harvey shrinks stat (h=1, n small)", abs(dm$harvey_statistic) <= abs(dm$dm_statistic) + 1e-12)
check("dm mean diff", isTRUE(all.equal(dm$mean_loss_diff, mean(l1 - l2))))
check("dm p in (0,1)", dm$p_value > 0 && dm$p_value < 1)

# identical losses -> zero diff, statistic 0/undefined variance path
dm0 <- env$diebold_mariano_test(l1, l1, lag.order = 1L)
check("dm identical losses stat 0", abs(dm0$mean_loss_diff) < 1e-12)

# Hand-computed Newey-West verification (lag 1):
d <- l1 - l2
dbar <- mean(d)
dc <- d - dbar
g0 <- mean(dc^2)
g1 <- mean(dc[2:6] * dc[1:5])
hac_manual <- (g0 + 2 * (1 - 1/2) * g1) / 6
dm_manual <- dbar / sqrt(hac_manual)
check_equal("dm statistic matches manual Newey-West",
            dm$dm_statistic, dm_manual)
check_equal("dm harvey factor sqrt((n-1)/n) for h=1",
            dm$harvey_statistic, dm_manual * sqrt((6 - 1) / 6))

# too few observations
dmn <- env$diebold_mariano_test(c(1, 2), c(2, 1))
check("dm n<3 returns NA", is.na(dmn$p_value) && dmn$n_obs == 2L)

# alternative = greater
dmg <- env$diebold_mariano_test(l1, l2, lag.order = 0L, alternative = "greater")
check("dm greater p smaller", dmg$p_value < 0.5)

# --- compute_growth_and_recession -----------------------------------------------
# Synthetic vintage: two areas, levels implying growth -0.5, -0.6 (recession),
# +0.4, +0.2 (expansion) for consecutive quarters.
six_quarters <- zoo::as.yearqtr(seq(as.Date("2024-01-01"), by = "quarter", length.out = 6))
synth_v <- tibble(
  geo = rep(c("EA", "DE"), each = 6),
  vintage_date = as.Date("2025-01-01"),
  quarter = rep(six_quarters, 2),
  gdp_level = c(100, 99.5, 98.9, 99.3, 99.5, 100,   # EA: -0.5, -0.6, +0.4, +0.2 ...
                200, 199, 198, 197.5, 198.5, 199.5) # DE
)

out <- env$compute_growth_and_recession(synth_v)
ea <- out %>% filter(geo == "EA") %>% arrange(quarter)
check_equal("growth Q2", ea$gdp_growth[2], 100 * (99.5 / 100 - 1))
check_equal("growth Q3", ea$gdp_growth[3], 100 * (98.9 / 99.5 - 1))
check("recession label Q3 (2 neg quarters)", ea$recession[3] == 1L)
check("second vintage quarter label is NA (needs prior growth)", is.na(ea$recession[2]))
check("expansion Q4", ea$recession[4] == 0L)
de <- out %>% filter(geo == "DE") %>% arrange(quarter)
check("DE recession starts Q3", de$recession[3] == 1L && de$recession[4] == 1L)

# Non-consecutive quarters must yield NA growth (data hole)
hole_v <- tibble(
  geo = "EA",
  vintage_date = as.Date("2025-01-01"),
  quarter = zoo::as.yearqtr(c("2024 Q1", "2024 Q3")),
  gdp_level = c(100, 101)
)
hole_out <- env$compute_growth_and_recession(hole_v)
check("hole NA growth", is.na(hole_out$gdp_growth[2]))

# --- build_ciss_as_of ------------------------------------------------------------
ciss_synth <- tibble(
  geo = rep("EA", 6),
  time = as.Date(c("2024-01-02", "2024-01-15", "2024-02-01", "2024-02-20",
                   "2024-03-05", "2024-04-02")),
  ciss_daily = c(0.2, 0.4, 0.6, 0.8, 1.0, 0.1)
)
cq <- env$build_ciss_as_of(ciss_synth, as.Date("2024-03-31"))
# Q1 mean of (0.2, 0.4, 0.6, 0.8, 1.0); April obs excluded by cutoff
check_equal("ciss Q1 mean", cq$ciss[cq$quarter == zoo::as.yearqtr("2024 Q1")], mean(c(0.2, 0.4, 0.6, 0.8, 1.0)))
check("ciss excludes future obs", !(zoo::as.yearqtr("2024 Q2") %in% cq$quarter))

# --- build_monthly_as_of (complete-quarter rule) ---------------------------------
esi_synth <- tibble(
  geo = "EA",
  month = zoo::as.yearmon(c("2024-01", "2024-02", "2024-03", "2024-04")),
  esi = c(100, 102, 104, 106),
  esi_release_date = as.Date(c("2024-01-29", "2024-02-29", "2024-03-29", "2024-04-29"))
)
mq <- env$build_monthly_as_of(esi_synth, "esi", "esi_release_date", as.Date("2024-03-15"))
# Only Jan+Feb published by 2024-03-15 -> Q1 incomplete -> filtered later;
# check months_available for Q1 is 2
q1 <- mq %>% filter(quarter == zoo::as.yearqtr("2024 Q1"))
check_equal("incomplete quarter months", q1$months_available, 2L)
mq2 <- env$build_monthly_as_of(esi_synth, "esi", "esi_release_date", as.Date("2024-04-30"))
q1b <- mq2 %>% filter(quarter == zoo::as.yearqtr("2024 Q1"))
check_equal("complete quarter mean", q1b$value, 102)

# --- gdp_panel_as_of / latest_target_quarters --------------------------------------
vint <- tibble(
  geo = rep("DE", 4),
  vintage_date = rep(as.Date(c("2025-01-01", "2025-02-01")), each = 2),
  quarter = rep(zoo::as.yearqtr(c("2024 Q3", "2024 Q4")), 2),
  gdp_growth = c(0.5, -0.2, 0.5, 0.1),   # Q4 revised
  recession = 0L
)
p1 <- env$gdp_panel_as_of(vint, as.Date("2025-01-15"))
check("panel uses latest vintage <= date",
      p1$gdp_growth[p1$quarter == zoo::as.yearqtr("2024 Q4")] == -0.2)
p2 <- env$gdp_panel_as_of(vint, as.Date("2025-03-01"))
check("panel updates after revision",
      p2$gdp_growth[p2$quarter == zoo::as.yearqtr("2024 Q4")] == 0.1)

# --- build_event_grid ---------------------------------------------------------------
eg_synth <- tibble(
  geo = c(rep("DE", 6), rep("EA", 3)),
  vintage_date = c(rep(as.Date(c("2025-01-01", "2025-02-01", "2025-03-01")), each = 2),
                   rep(as.Date(c("2025-02-01", "2025-02-01", "2025-03-01")))),
  quarter = zoo::as.yearqtr(c("2024 Q3", "2024 Q4",   # DE ed.1
                              "2024 Q3", "2024 Q4",   # DE ed.2 (no new quarter)
                              "2024 Q4", "2025 Q1",   # DE ed.3 (Q1-2025 new)
                              "2024 Q3", "2024 Q4",   # EA vintage at ed.2 (Q4 new)
                              "2024 Q4")),            # EA vintage at ed.3 (no new)
  gdp_growth = 0.4,
  recession = 0L
)
eg <- env$build_event_grid(eg_synth)
# 2025-01-01: DE latest Q4 new -> event
# 2025-02-01: DE still Q4; EA latest Q4 new -> event (EA only)
# 2025-03-01: DE Q1-2025 new -> event (DE)
check_equal("event grid rows", nrow(eg), 3L)
check_equal("event dates", sort(unique(eg$event_date)),
            as.Date(c("2025-01-01", "2025-02-01", "2025-03-01")))
check_equal("EA event at 02-01",
            eg %>% filter(event_date == as.Date("2025-02-01")) %>% pull(geo), "EA")
check_equal("DE event at 03-01",
            eg %>% filter(event_date == as.Date("2025-03-01")) %>% pull(geo), "DE")

# --- parse_pmi_month -----------------------------------------------------------------
pm <- env$parse_pmi_month(c("Jan98", "Dec23", "Mar15", "garbage", NA))
check_equal("pmi Jan98", as.numeric(pm[1]), as.numeric(zoo::as.yearmon("1998-01")))
check_equal("pmi Dec23", as.numeric(pm[2]), as.numeric(zoo::as.yearmon("2023-12")))
check_equal("pmi Mar15", as.numeric(pm[3]), as.numeric(zoo::as.yearmon("2015-03")))
check("pmi garbage NA", is.na(pm[4]) && is.na(pm[5]))

###############################################################################
# SUMMARY
###############################################################################

cat("\n=============================================\n")
cat("PASS:", n_pass, " FAIL:", n_fail, "\n")
if (n_fail > 0L) {
  cat("Failed tests:\n - ", paste(failures, collapse = "\n - "), "\n")
  quit(status = 1)
} else {
  cat("All tests passed.\n")
  quit(status = 0)
}
