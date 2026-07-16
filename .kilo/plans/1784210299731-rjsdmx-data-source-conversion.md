# Plan: Convert `recession-index-rjsdmx.R` to RJSDMX (drop `eurostat`/`ecb`)

## Goal & Scope
Rewrite the **data-download layer** of `recession-index-rjsdmx.R` so it fetches GDP, CISS, and ESI via `RJSDMX::getTimeSeriesTable()` instead of the `eurostat` / `ecb` packages. The modeling (brms) and visualization (ggplot2/patchwork) layers are preserved as-is to avoid changing model behavior.

- **Single file changed:** `recession-index-rjsdmx.R` (currently an identical copy of `recession-index.R`).
- **Out of scope:** `recession-index.R`, `check_units.R`, `test_ecb.R`, `modello_recessione_ciss_pmi_RJSDMX.R`, README, renv.lock, qmd files.

## Confirmed SDMX keys (verified against live REST APIs — all HTTP 200)
| Series | Provider | Key (per geo, substitute `<geo>`) | Freq | EA code (→"EA") |
|---|---|---|---|---|
| GDP (real QoQ %, SCA) | EUROSTAT | `namq_10_gdp/Q.CLV_PCH_PRE.SCA.B1GQ.<geo>` | Q | **EA** (changing-composition, works) |
| CISS | ECB | `CISS/D.<geo>.Z0Z.4F.EC.SS_CIN.IDX` | D | U2 |
| ESI (SA) | EUROSTAT | `ei_bssi_m_r2/M.BS-ESI-I.SA.<geo>` | M | **EA21** (user-specified; no `EA` exists in this dataset) |

Evidence:
- **GDP unit code is `CLV_PCH_PRE`, NOT `PCH_PRE`.** The SDMX API rejects `UNIT=PCH_PRE` (`INVALID_QUERY_DIMENSION_VALUE`); `PCH_PRE` only exists on the bulk JSONSTAT API used by the `eurostat` package. `CLV_PCH_PRE` = "chain-linked volumes, % change on previous period" = real GDP QoQ growth. Verified: EA returns 0.3/0.2/0.0 for 2025-Q3/Q4/2026-Q1. (Note: `modello_*.R` uses the broken `PCH_PRE` — that path fails on SDMX.)
- **ESI has no `unit` dimension**; key order is `FREQ.INDIC.S_ADJ.GEO`. No changing-composition `EA`/`EA19` in this dataset on SDMX (both 400); `EA21` (history since 1980) and `EA20` are the EA aggregates. Per user instruction, use **EA21**.
- **GDP `EA`** works directly (no EA20/EA19/EA fallback loop needed).
- The original **monthly CISS** key `CISS/M.U2...SS_CIN.B` returns **404** on SDMX (legacy `ecb` package only). The **daily** key returns valid data. Daily CISS + `D→M→Q` aggregation is the **only** working SDMX path.

Geos to fetch: **EA, DE, FR, IT, ES** (Spain preserved; `modello_*.R` only has 4 — we must keep all 5 here).

## Key Decisions
1. **Use `RJSDMX::getTimeSeriesTable()`** (user-confirmed). It returns a data frame with `time_period` / `obs_value` columns. Reuse the normalization helper pattern proven in `modello_recessione_ciss_pmi_RJSDMX.R` (`normalize_sdmx_table`, `download_sdmx_table`, tolerant `try_*` variant, `parse_sdmx_quarter`), rewritten in English to match this file's style.
2. **CISS = daily**, aggregated `daily→monthly (mean)→quarterly (mean)` — verbatim from `modello_*.R` (`read_ciss_series` + the `ciss_daily`/`ciss_monthly`/`ciss` pipeline).
3. **ESI = monthly**, aggregated `monthly→quarterly (mean)`. Monthly date parse mirrors the original script's own logic: `ifelse(nchar(x)==7, paste0(x,"-01"), x)` → `as.Date` → `as.yearmon` → `as.yearqtr`.
4. **GDP EA code** uses `EA` directly (no fallback loop needed — `EA` works on SDMX). **ESI EA code** uses `EA21` (per user instruction; `EA`/`EA19` don't exist in this dataset). Both relabeled to "EA" internally.
5. **Preserve original modeling/viz logic**: the recession definition (`gdp_growth<0 & lag(gdp_growth,1)<0`), the full-dataset `scale()` standardization, the brms priors/chains, and the plot function stay unchanged (minimal-diff principle). The full-set `scale()` is a known mild data-leakage; left as-is and noted below, not in scope.

## Reference (proven patterns to port)
- `modello_recessione_ciss_pmi_RJSDMX.R:93-193` — SDMX table helpers + `parse_sdmx_quarter`.
- `modello_recessione_ciss_pmi_RJSDMX.R:224-293` — GDP download + EA20/EA19/EA fallback loop.
- `modello_recessione_ciss_pmi_RJSDMX.R:347-409` — CISS daily→monthly→quarterly.
- `modello_recessione_ciss_pmi_RJSDMX.R:199-210` — `getProviders()` check; `:42-50` — Java availability check.

## Implementation Steps (ordered)
1. **Header / libs** — Remove `library(eurostat)` and `library(ecb)`. Add `library(RJSDMX)`. Add `library(scales)` (used by `scales::percent_format()` in the plot) and `library(tibble)`. Keep `dplyr, tidyr, lubridate, ggplot2, patchwork, brms, zoo`.
2. **Bootstrap guards** (new, top of script): auto-install missing packages loop; `Sys.which("java")` stop with clear message (RJSDMX needs Java 8+); `RJSDMX::getProviders()` check that both `EUROSTAT` and `ECB` are present.
3. **Helper functions** (new block): port `normalize_sdmx_table`, `download_sdmx_table`, `try_download_sdmx_table`, `parse_sdmx_quarter` from `modello_*.R`, translated to English comments. Add a small `parse_sdmx_month()` (the 7-char monthly date logic).
4. **GDP download** — Replace the `get_eurostat("namq_10_gdp", ...)` block with a per-geo `read_gdp_series()` using key `namq_10_gdp/Q.PCH_PRE.SCA.B1GQ.<geo>` (EUROSTAT, `gregorianTime=FALSE`, parse via `parse_sdmx_quarter`). Run the EA20→EA19→EA tolerant loop, then `bind_rows` EA, DE, FR, IT, ES. Output columns: `geo, quarter, gdp_growth` (grouped mean per `geo, quarter`).
5. **CISS download** — Replace the `get_data(ciss_key)` block with `read_ciss_series()` using key `CISS/D.<ecb_geo>.Z0Z.4F.EC.SS_CIN.IDX` (ECB, `start="1998-01-01"`, `gregorianTime=TRUE`). Fetch U2, DE, FR, IT, ES; map U2→EA. Replicate the `daily → monthly (as.yearmon, mean) → quarterly (as.yearqtr, mean)` pipeline. Output: `geo, quarter, ciss`.
6. **ESI download** — Replace the `get_eurostat("ei_bssi_m_r2", ...)` block with `read_esi_series()` using key `ei_bssi_m_r2/M.BS-ESI-I.SA.<geo>` (EUROSTAT, `gregorianTime=TRUE`). EA via EA20→EA19→EA tolerant loop; fetch DE, FR, IT, ES. Parse monthly dates with `parse_sdmx_month()` → `as.yearmon` → group by `geo, quarter` mean. Output: `geo, quarter, esi`.
7. **Merge / recession / scaling** — Leave the existing `df` pipeline unchanged (`inner_join` gdp+ciss+esi by `geo, quarter`; group_by(geo) recession via `lag`; `drop_na()`; `scale()` ciss/esi). Only dependency: the three input tibbles must expose `geo, quarter, <value>` — guaranteed by steps 4–6.
8. **Model + plots** — No change. Keep `brmsformula`, `brm(...)`, `fitted(...)`, `plot_geo(...)`, and the EA + 2×2 country grid (EA/DE/FR/IT/ES) exactly as in the current file.
9. **Self-check output** (new, optional, cheap): after each download, `message()` the geo list + min/max quarter per series, so a failed SDMX query surfaces immediately (fail-fast per global rules) rather than as an empty `inner_join`.

## Files Touched
- `recession-index-rjsdmx.R` — only this file.

## Dependencies / renv
- `RJSDMX` is already in `renv.lock` (verified). No new installs required for the script to run.
- `eurostat`/`ecb` remain in `renv.lock` because `recession-index.R`, `check_units.R`, `test_ecb.R` still use them. **Do not** run `renv::snapshot` removal and do not drop those packages.

## Risks & Mitigations
- **EA code drift** (EA20 vs EA19 vs EA): mitigated by the tolerant EA20→EA19→EA search for both Eurostat series (GDP, ESI).
- **`getTimeSeriesTable` column-name casing**: mitigated by `normalize_sdmx_table` (lowercase + sanitize; require `time_period`/`obs_value`), proven in `modello_*.R`.
- **`gregorianTime` monthly format ambiguity** (`"2026-04"` vs `"2026-04-01"`): mitigated by `parse_sdmx_month()` handling both forms.
- **Modeling behavior change**: none — predictors, recession rule, priors, and scaling are unchanged (only the data source swaps).

## Validation
1. `grep -nE 'eurostat|ecb|get_eurostat|get_data' recession-index-rjsdmx.R` returns **no** functional usages (only, possibly, comments).
2. Run `Rscript recession-index-rjsdmx.R` (requires Java 8+ and network access):
   - No errors from the SDMX helpers; self-check messages show all 5 geos for each series with sensible date ranges.
   - `nrow(df) > 0` (merge produced observations); `train_data` non-empty.
   - brms model fits; `fitted()` returns probabilities; `final_plot` renders the EA panel + DE/FR/IT/ES grid.
3. Spot-check a few ESI/CISS/GDP values against the Eurostat/ECB databrowser for one recent quarter to confirm the values match (not just non-empty).
