# Hierarchical Nowcasting of Euro Area Recession Risk

This repository implements a **Hierarchical Bayesian Logit** model to nowcast recession risk in the Euro Area (EA) and its four largest member states: Germany (DE), France (FR), Italy (IT), and Spain (ES). 

The methodology is a hierarchical extension of the framework proposed by **Francesco Furno and Domenico Giannone (2024)** in their paper *"Nowcasting recession risk"*.

## Project Objective

The primary goal of this project is to assess and nowcast recession probabilities in real-time. While official business cycle dating committees (such as the CEPR for the Euro Area) announce recessions with significant lags, this framework leverages high-frequency financial and macroeconomic data to estimate the probability that each economy is currently in a recession.

By using a hierarchical structure, we pool statistical strength across the Euro Area and the individual countries. This allows the model to learn common relationships between indicators and recessions while adapting parameters to country-specific dynamics.

## Data Sources

The model uses monthly and quarterly indicators retrieved directly from official APIs:
1. **Macroeconomic Activity**: **Economic Sentiment Indicator (ESI)** from Eurostat (dataset `ei_bssi_m_r2`).
2. **Financial Conditions**: **Composite Indicator of Systemic Stress (CISS)** from the European Central Bank (ECB) Data Portal.
3. **Target Variable**: **Real GDP Growth** (Quarter-on-Quarter percentage changes) from Eurostat (dataset `namq_10_gdp`). A technical recession is defined as two consecutive quarters of negative GDP growth.

## Methodology Summary

The probability of a recession $p_{i,t}$ for region $i$ at quarter $t$ is modeled using a hierarchical logit specification:
$$ \text{logit}(p_{i,t}) = \alpha_i + \beta_{1,i} \text{CISS}_{i,t} + \beta_{2,i} \text{ESI}_{i,t} $$

Where the coefficients are modeled hierarchically around a global mean:
- Intercepts: $\alpha_i = \alpha_0 + u_{0,i}$
- CISS impact: $\beta_{1,i} = \beta_1 + u_{1,i}$
- ESI impact: $\beta_{2,i} = \beta_2 + u_{2,i}$
- Random effects: $(u_{0,i}, u_{1,i}, u_{2,i}) \sim \mathcal{N}(0, \Sigma)$

Priors are selected to be weakly informative: $\alpha_0 \sim \mathcal{N}(0, 5)$ and $\beta_k \sim \mathcal{N}(0, 2)$.

## Getting Started

### Prerequisites
The project uses R and Quarto. You should have R (>= 4.2) and Quarto installed.

The project dependencies are managed via `renv`. To install them, launch R in the project root and run:
```R
renv::restore()
```

This will install packages including `brms` (for Bayesian modeling), `RJSDMX` (data
retrieval; requires Java on PATH), `dplyr`, `ggplot2`, and `patchwork`.

### Tests
The offline test suite (unit tests on pure helpers + a synthetic-data pipeline
test, no downloads and no brms needed) runs in seconds:
```bash
Rscript --vanilla new_code/tests/run_all_tests.R
```
Add `--with-live` to include the live OECD smoke test.

### Dashboard refresh cycle
The website renders from **committed artifacts**, so refreshing the forecast
never re-fits models at render time:

1. On a host with RJSDMX/brms (e.g. production), produce the artifacts:
   ```bash
   DASHBOARD_PREDICTOR=esi Rscript --vanilla new_code/update_dashboard_data.R
   ```
   This fits the production hierarchical model with the 5-year-gap design and
   writes `dashboard/data/probabilities_esi.csv` (probabilities tagged
   in-sample / out-of-sample / nowcast), `fixed_effects_esi.csv`, `meta_esi.csv`,
   and the model summary. Use `DASHBOARD_PREDICTOR=pmi` for the PMI arm
   (production only — the S&P Global workbook cannot leave that network).
2. Commit the refreshed `dashboard/data/*.csv`.
3. Render locally: `make dashboard` (or `quarto render`).

`index.qmd` shows the last 120 months: in-sample fit (blue), out-of-sample
inference (orange), and the nowcast extension to the latest quarter whose
three months of predictor data are published (dashed orange). There are no
forward projections.

### Running the original analysis script
```bash
Rscript recession-index.R
```
downloads data via the `eurostat`/`ecb` packages, fits the model in memory and
plots it. `recession-index-rjsdmx.R` is the RJSDMX-based equivalent.

### Rendering the Website
The results and methodology are compiled into a Quarto website. To build the website:
```bash
quarto render
```
The rendered files will be placed in the `_site/` directory.

To preview the website locally with live-reloading:
```bash
quarto preview
```

## Repository Structure
- `index.qmd`: dashboard home page — renders the probability charts from the
  committed artifacts in `dashboard/data/` (no downloads, no fitting).
- `about.qmd`: Details about the project background and data sources.
- `methodology.qmd`: Technical report explaining the equations, priors, and references.
- `short-note.qmd`: research note (Economics Letters style; renders to
  `_site/short-note.pdf`) — also reads committed artifacts and includes the
  real-time evaluation section.
- `Makefile`: `make test` / `make update-data` / `make dashboard` / `make site`.
- `_quarto.yml`: Quarto website configuration.
- `recession-index.R`: Standalone R script for processing and estimation
  (`eurostat`/`ecb` package data layer).
- `recession-index-rjsdmx.R`: RJSDMX-based version of the same.
- `references.bib`: Bibliographical database containing the citation entries.
- `reference/`: Folder containing the reference paper PDF.
- `dashboard/data/`: committed model artifacts consumed by the site.
- `new_code/`: production specifications and real-time evaluation experiments.
  - `recessione_CISS_ESI.R`: production model, CISS + ESI predictors.
  - `recessione_CISS_PMI_composito.R`: production model, CISS + composite PMI
    predictors (S&P Global workbook, production environment only).
  - `pmi_io.R`: shared, unit-tested reader for the PMI workbook.
  - `update_dashboard_data.R`: dashboard data layer (fit + artifact export).
  - `robustness_real_time_vs_latest_EA_CISS_{ESI,PMI}.R`: EA-only real-time
    vs latest-vintage comparisons for each specification.
  - `pseudo_realtime_ESI_vs_PMI.R`: **pseudo-real-time backtest comparing
    the ESI and PMI specifications head-to-head** on the Euro Area and DE,
    FR, IT, ES with real GDP vintages (Eurostat `ei_na_q_vtg` for the EA,
    OECD MEI vintage editions for countries), Brier/log/AUC scores per
    area and pooled, and Diebold-Mariano tests (HAC + Harvey correction)
    of whether either specification forecasts better. Supports a smoke mode
    (`BACKTEST_MAX_EVENTS=<n>`) and resume via per-event caches. See
    `new_code/PROD_RUN_ESI_VS_PMI.md` for the production run guide and
    `new_code/data_sources_pseudo_realtime.md` for the full source
    documentation.
  - `calibration_analysis.R`: post-hoc calibration (reliability bins, Murphy
    Brier decomposition, logistic recalibration) from the backtest's
    `paired_probabilities.csv` — no model re-runs needed.
  - `tests/`: offline test suites (`run_all_tests.R`; add `--with-live` for
    the OECD live smoke test).

## Status / next steps
- [x] Production hierarchical models (ESI and PMI arms) + vintage data layer.
- [x] EA-only real-time vs latest-vintage robustness runs (results committed).
- [x] ESI-vs-PMI backtest code, tests, and production run guide — **execution
      pending on production** (`new_code/PROD_RUN_ESI_VS_PMI.md`).
- [x] Dashboard data layer + artifact-driven site.
- [ ] Ingest the backtest results, write `new_code/RESULTS.md`, finalize the
      note's ESI-vs-PMI section, and pick the dashboard predictor arm.
- [ ] Post-hoc calibration once the backtest results are committed.
