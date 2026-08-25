# Data sources — pseudo-real-time ESI vs PMI comparison

Reproducible documentation of every source used by
`new_code/pseudo_realtime_ESI_vs_PMI.R`, with endpoints, keys, access
quirks, and verification evidence. All endpoints were probed live on
2026-08-25 from the development machine unless noted.

## 1. GDP vintages (target variable, real-time)

The target is the technical-recession label (two consecutive quarters of
negative QoQ GDP growth) **as first measurable in each real-time data
snapshot**. Two complementary archives are used because no single public
source covers both the Euro Area aggregate and the member states.

### 1a. Euro Area aggregate — Eurostat `ei_na_q_vtg`

| Item | Value |
|---|---|
| Provider | Eurostat, SDMX 2.1 (via `RJSDMX::getTimeSeriesTable`, provider `EUROSTAT`) |
| Dataset | `ei_na_q_vtg` — Euro indicators, vintages of quarterly national accounts |
| Key | `ei_na_q_vtg/Q..SCA.CLV05_MEUR.EA` (empty REVDATE = all revision dates) |
| Dimensions | `FREQ.REVDATE.S_ADJ.UNIT.GEO` |
| Unit | `CLV05_MEUR` — chain-linked volumes (2005), million euro (levels) |
| Geo | `EA` — **changing-composition** Euro Area aggregate (relabelled internally; never a fixed EA19/EA20 code) |
| Coverage observed | 175 vintages (e.g. 2026-07-30 back through the archive); the repo's EA-only robustness scripts use the same archive |
| Processing | QoQ growth = `100 * (level / lag(level) - 1)` computed **within each vintage**; recession label from consecutive negative growth |

**Pitfalls verified live**

- GEO is restricted to `EU` and `EA` (dimension enumeration via the
  statistics API). Querying `DE`, `FR`, `IT`, `ES`, `EA20`, `EA19` returns
  HTTP 400 `INVALID_QUERY_DIMENSION_VALUE`. **This dataset cannot serve
  countries.**
- The unit is a **level** series; growth must be derived per-vintage (the
  percent-change units of `namq_10_gdp` are not available here).
- Vintages are only stored when something changed, so consecutive vintage
  dates are irregular (roughly 20/45/65/110 days after quarter-end).

### 1b. Countries (DE, FR, IT, ES) — OECD MEI vintage archive

Eurostat does **not** disseminate country-level GDP vintages through any
API (verified: `euroind_vtg` exists as a dataset with country coverage per
its ESMS metadata, but is browser/Excel-only; `ei_na_q_vtg` is aggregates
only). The public country-vintage archive is the OECD's:

| Item | Value |
|---|---|
| Provider | OECD SDMX 2.1 REST, no key required |
| Dataflow | `OECD.SDD.STES,DSD_STES_REVISIONS@DF_STES_REVISIONS,4.0` ("Short-term economic statistics revisions") |
| Base URL | `https://sdmx.oecd.org/public/rest/data/OECD.SDD.STES,DSD_STES_REVISIONS@DF_STES_REVISIONS,4.0` |
| Key (batched, all four countries in ONE request) | `DEU+FRA+ITA+ESP.Q.B1GQ_Q.XDC._T.<EDITION>` |
| Dimensions | `REF_AREA.FREQ.MEASURE.UNIT_MEASURE.ACTIVITY.EDITION` |
| Measure | `B1GQ_Q` — GDP, volume |
| Unit | `XDC` — national currency (**levels**); `GR`/`IX`/`PS`/`PT_*` return 404 for this measure |
| Activity | `_T` — total |
| Edition | `YYYYMM` — the MEI edition month; 331 editions from `199902` to `202608` at verification time |
| Detail param | `?detail=dataonly` with `Accept: application/vnd.sdmx.data+csv; charset=utf-8; version=2; labels=id` |
| As-of date mapping | edition `YYYYMM` → `YYYY-MM-01 + 1 month` (edition is published in its label month) |
| Coverage verified | edition `202608` → data through `2026-Q2` for DEU/FRA/ITA/ESP; edition `202506` → through `2025-Q3` |

Each edition request returns the **full published history** of quarterly
GDP volume levels as of that edition (110-190 quarters per country), which
is exactly the vintage panel needed: training labels are computed within
the edition, never across editions.

**Pitfalls verified live**

- **Aggressive rate limiting.** Bursts of requests get HTTP 429 for several
  minutes. The downloader retries with exponential backoff (45 s → ×1.6,
  ceiling 420 s), keeps an 8 s pause between editions, and caches all
  editions to `pseudo_realtime_ESI_vs_PMI_output/oecd_gdp_vintages_raw.csv`
  so re-runs only fetch missing editions.
- `+`-batched multi-area keys work and are the correct way to stay within
  rate limits (1 request per edition instead of 4).
- The response is SDMX-CSV v2 **only** with the right Accept header; plain
  `read.csv(url)` yields parse warnings. The R `curl` package sets the
  header and captures the HTTP status for retry logic.
- Editions are monthly even though GDP is quarterly: multiple editions can
  share the same latest quarter. The event grid keeps only the first
  edition in which a new quarter appears (one nowcast opportunity per
  quarter).

### Alternatives probed and rejected

| Source | Result |
|---|---|
| Eurostat `euroind_vtg` (PEEI vintage DB) | Country coverage per ESMS, but **not on the API** (`ERR_NOT_FOUND_2 ... not available for dissemination`; bulk-download facility decommissioned — 404). Browser/Excel only. |
| ECB Real-Time Database (`RTD`) | SDMX-accessible, but **REF_AREA = EA (S0), US, JP only** — no member states (full enumeration: 81k rows, no DE/FR/IT/ES). |
| OECD revisions triangles / `DF_STES_REVISIONS` unit variants | `GR`, `IX`, `PS`, `PT_LF`, `PT_PCOS` return `NoRecordsFound` for `B1GQ_Q`; only `XDC` levels exist. |
| ALFRED (St. Louis Fed) | Reachable only over plain HTTP from this network; **vintage queries need a FRED API key** (none available). Latest-vintage CSV works but is of no use for real-time analysis. |
| Eurostat revision triangles page | EU/EA aggregates only (QoQ growth since 2012/2017). |

## 2. CISS (predictor, daily)

| Item | Value |
|---|---|
| Provider | ECB Data Portal, SDMX (via `RJSDMX`, provider `ECB`) |
| Key | `CISS/D.<GEO>.Z0Z.4F.EC.SS_CIN.IDX` with `<GEO>` ∈ {`U2`→EA, `DE`, `FR`, `IT`, `ES`} |
| Frequency | Daily (the monthly key `...SS_CIN.B` returns 404 on SDMX — same finding as the repo's existing scripts) |
| Coverage verified | all five areas from 1980-01-02 |
| As-of treatment | daily values cut at the event date (`CISS_RELEASE_LAG_DAYS = 0`); quarterly mean of available days |

## 3. ESI (predictor, monthly)

| Item | Value |
|---|---|
| Provider | Eurostat SDMX (via `RJSDMX`, provider `EUROSTAT`) |
| Dataset | `ei_bssi_m_r2` |
| Key | `M.BS-ESI-I.SA.<GEO>` with `<GEO>` ∈ {`EA21`→EA, `DE`, `FR`, `IT`, `ES`} |
| Geo code | **`EA21`** — the dataset has no changing-composition `EA` code on SDMX (same convention as the repo's existing scripts) |
| Coverage verified | all five areas from 1980-01 |
| As-of treatment | a reference month becomes available on day 29 of that month (clamped to month-end) by default; an optional `esi_release_calendar.csv` (`month,release_date`) overrides month by month; quarterly mean requires all 3 months (complete-quarter rule) |

**Limitation (carried over from the existing robustness scripts):**
Eurostat serves the *currently available* ESI history, not historical ESI
vintages — real-time treatment is with respect to publication availability
only; values may include later revisions.

## 4. PMI (predictor, monthly, production-only)

| Item | Value |
|---|---|
| Source | `pmi_servizi_composito.xlsx` — S&P Global Composite PMI Output |
| Location | `//home/group/main/891ac/private/ac/PMI/pmi_servizi_composito.xlsx` (production network), fallback `./pmi_servizi_composito.xlsx` |
| Sheets | `CompEMU`→EA, `CompGER`→DE, `CompFRA`→FR, `CompITA`→IT, `CompSPA`→ES |
| Layout | cell B5 = series name (`S&P GLOBAL PMI: COMPOSITE - OUTPUT`, validated); data from row 10: column A month labels `Jan98` (2-digit year, `>=70` ⇒ 19xx), column B PMI value |
| As-of treatment | available on day 26 of the reference month (flash-release timing assumption); complete-quarter rule as for ESI |

**Data-handling constraint:** the workbook is licensed S&P Global data and
**cannot be taken outside the production environment**. For local tests, a
synthetic workbook with the identical layout is generated
(`new_code/tests/smoke_data_layer.R`); on production the real path resolves
automatically. S&P PMI values are not revised after publication.

## 5. Event grid and as-of alignment

- Evaluation events are the OECD edition as-of dates from
  `EVAL_START = 2015-01-01` at which at least one area's latest usable GDP
  quarter is new (its **first nowcast opportunity**).
- Countries use the latest OECD edition with as-of date ≤ event date; the
  Euro Area uses the latest Eurostat `ei_na_q_vtg` vintage ≤ event date.
- Predictors are built strictly as of the event date (publication rules
  above); the hierarchical model is estimated once per approach per event
  on the pooled 5-area panel with a **5-year gap**: for each area, training
  rows satisfy `quarter <= target_quarter(area) − 5 years`, replicating the
  production scripts' `cutoff_date <- max_date - 5` split literally.

## 6. Evaluation

- One nowcast per (approach, area, quarter): Brier score, log score, AUC
  per area and pooled, against the **real-time** vintage label.
- **Diebold-Mariano** tests of equal predictive ability on paired Brier and
  log-loss differentials, per area and pooled: Newey-West HAC variance
  (lag 1) with the Harvey-Leybourne-Newbold (1997) small-sample correction
  (`h = 1`).

## 7. Reproducibility

- The OECD vintage archive is cached to
  `pseudo_realtime_ESI_vs_PMI_output/oecd_gdp_vintages_raw.csv`; deleting
  the file re-downloads everything (rate limits permitting).
- `brms` fits are cached by event date + approach in
  `pseudo_realtime_ESI_vs_PMI_output/brms_cache/` (`file_refit =
  "on_change"`), so interrupted runs resume.
- Seeds fixed (`set.seed(123)`, brms `seed = 123`).
