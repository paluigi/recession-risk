# Production run guide — pseudo-real-time ESI vs PMI backtest

This is the step-by-step guide for running
`new_code/pseudo_realtime_ESI_vs_PMI.R` on the production environment,
where the licensed S&P Global PMI workbook lives. The workbook cannot be
copied out of production, so the head-to-head ESI-vs-PMI comparison must
run there. Everything else (Eurostat/ECB/OECD data) is public.

The script fits the hierarchical Bayesian logit **once per event date per
approach** (ESI and PMI), so expect a long run: from a few hours to a
couple of days depending on the box. All progress is cached, so an
interrupted run simply resumes where it stopped.

---

## 0. Prerequisites

| Requirement | Detail |
|---|---|
| R | ≥ 4.2 with `RJSDMX`, `brms`, `rstan`, `dplyr`, `tidyr`, `tibble`, `lubridate`, `ggplot2`, `patchwork`, `zoo`, `scales`, `readxl`. The box that produced the committed `real_time_vs_latest_*` results (R 4.5.3, RHEL 8.10, MKL) satisfies this; the script also auto-installs any missing package at startup. |
| Java | `java` on PATH (RJSDMX requirement; Java 16+ is handled by the script itself via the `--add-opens` option). |
| Network | Outbound HTTPS to the Eurostat, ECB and OECD SDMX APIs. The OECD endpoint rate-limits aggressively; the script retries with backoff and caches the download. |
| PMI workbook | Readable at `//home/group/main/891ac/private/ac/PMI/pmi_servizi_composito.xlsx` — the same path used by `recessione_CISS_PMI_composito.R`. A local copy at `new_code/pmi_servizi_composito.xlsx` is honoured as fallback (for testing only; do not commit it). |
| Disk | A few GB: the OECD vintage cache, the brms fit cache (`brms_cache/`, one file per fit) and the output tables. |

## 1. Get the code

```bash
git clone https://github.com/paluigi/recession-risk.git
cd recession-risk
git checkout <run-branch>     # the branch carrying the latest script version
```

If the repo is already cloned: `git fetch && git checkout <run-branch> && git pull`.

> Do **not** run `renv::restore()`: the lockfile pins R 4.4.1 while
> production runs 4.5.3, and the box already has the required packages in
> its site/user library. Run everything with `--vanilla`, which skips the
> renv bootstrap entirely.

## 2. Pre-flight (minutes)

From the **repo root**:

```bash
Rscript --vanilla new_code/tests/run_all_tests.R
```

Expected: `ALL SUITES PASSED` (unit + offline pipeline; ~50 + 20 checks,
fully offline). Optional, if you want to hit the live OECD endpoint once
before the real run:

```bash
Rscript --vanilla new_code/tests/run_all_tests.R --with-live
```

If a suite fails here, stop and fix the environment — do not start the
full run.

## 3. Smoke run (~15–30 min)

**Run from inside `new_code/`** — the output directory is created
relative to the working directory, and running from `new_code/` keeps the
results next to the existing committed result sets:

```bash
cd new_code
BACKTEST_MAX_EVENTS=6 Rscript --vanilla pseudo_realtime_ESI_vs_PMI.R
```

This fits only the first 6 event dates and exercises every stage:
RJSDMX downloads (Eurostat EA vintages, CISS, ESI), the OECD vintage
archive, the PMI workbook read (including the B5 series-name check), the
hierarchical fits, and all export writers.

Success looks like:

* `SMOKE MODE: BACKTEST_MAX_EVENTS=6 ...` in the log;
* a plot window/artifact at the end (or the `esi_vs_pmi_realtime_probabilities.png`
  file in `pseudo_realtime_ESI_vs_PMI_output/`);
* `pseudo_realtime_ESI_vs_PMI_output/event_runs_smoke/` containing six
  `event_YYYYMMDD.csv` files;
* `pseudo_realtime_ESI_vs_PMI_output/progress_manifest.csv` with six
  `mode=smoke` rows.

**Leave the smoke output directory in place.** The OECD download cache and
the brms fit cache it created are reused by the full run; smoke result
tables live in `event_runs_smoke/` and can never mix with the full run
(`event_runs/`).

## 4. Full run

Still from `new_code/`, in a `tmux`/`screen` session or with `nohup`:

```bash
cd new_code
nohup Rscript --vanilla pseudo_realtime_ESI_vs_PMI.R > backtest.log 2>&1 &
tail -f backtest.log
```

* Each event prints one line: `  [ 2015-02-01] fitting ESI + PMI hierarchical models`.
* **Interrupted? Just re-run the same command.** Completed events are
  reloaded from `event_runs/`, the OECD cache skips the bulk download, and
  the brms cache skips completed fits.
* `progress_manifest.csv` tracks one row per event (`source=fit|cache`,
  `status=ok|partial|error`). Events whose predictors could not be
  assembled leave no cache file and are retried on the next run; per-model
  fit failures are recorded as `status="failed"` rows in the results, not
  fatal.
* If you ever change script settings or input data, delete
  `event_runs/` (and, to be safe, `brms_cache/`) before re-running — the
  caches would otherwise return stale results.

## 5. Copy the results back

Copy the whole directory
`new_code/pseudo_realtime_ESI_vs_PMI_output/` from prod into the same
path in your local clone, then commit (or just hand the directory over —
the ingestion step sorts out what to keep).

Exact file list:

**Required (the analysis):**

| File | Content |
|---|---|
| `comparison_metrics_by_area.csv` | Brier / log score / AUC per area and pooled, both approaches |
| `diebold_mariano_tests.csv` | DM/HLN tests of ESI vs PMI predictive ability |
| `pseudo_realtime_nowcasts.csv` | One nowcast per (approach, area, quarter) at its first real-time opportunity |
| `paired_probabilities.csv` | Paired ESI/PMI probabilities per evaluation point |
| `event_grid.csv` | The evaluation event grid |
| `pseudo_realtime_all_runs_including_failures.csv` | Every run incl. failed fits |
| `session_info.txt` | Full R/session description of the run |
| `esi_vs_pmi_realtime_probabilities.png` | Head-to-head probability plot |

**Recommended:**

| File | Why |
|---|---|
| `pseudo_realtime_ESI_vs_PMI_results.rds` | Full run object — enables post-hoc analyses (e.g. calibration curves) without re-running |
| `oecd_gdp_vintages_raw.csv` | The OECD vintage cache — avoids re-hitting the rate-limited endpoint in any future rerun |
| `progress_manifest.csv` | Run log (useful if the run needed retries) |

**Optional (default: do **not** commit):**

| File | Why |
|---|---|
| `brms_cache/` | Large; only needed to resume/extend the run on the same box |
| `event_runs/`, `event_runs_smoke/` | Per-event intermediate tables already folded into the final CSVs |

Licensing note: all files are model outputs (probabilities, scores,
vintage-derived growth rates) and contain **no S&P Global series values**;
committing them is consistent with the already-committed
`real_time_vs_latest_*` result sets. The PMI workbook itself must stay in
production.

## 6. What happens after the results land

1. The output directory is committed under `new_code/pseudo_realtime_ESI_vs_PMI_output/`.
2. `new_code/RESULTS.md` interprets the metrics and DM tests → verdict on
   which predictor (ESI or PMI) is better suited going forward.
3. The research note gets its real-time section; the dashboard data layer
   is wired to the winning predictor (ESI until proven otherwise).
