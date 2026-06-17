# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

This is a research pipeline estimating utility models for a social norm meta-analysis, using Dictator Game (DG) data. The pipeline runs in three stages: Stata estimation → R post-processing → R tables and plots.

## Running the analysis

**Step 1 — Stata estimation** (fix the `cd` on line 101 first if needed):
```stata
do "Utility_comparison.do"
```
Outputs clean CSVs to `Output/Data/` and LaTeX tables to `Output/Tables/`.

**Step 2 — Process results** (compute IVW and RE averages):
```r
Rscript "Ancillary/_run_convert.R"
```

**Step 3a — Summary table**:
```r
source("Ancillary/make_tables.R")
```
Writes `Output/Tables/table_estimates.tex` — the main results table with coefficients and 95% CIs.

**Step 3b — Coefficient plots**:
```r
Rscript "Ancillary/_run_plots.R"
```
Writes FE and RE coefficient plots + I² heterogeneity chart to `Output/Figures/`.

## Known path issue

`Utility_comparison.do` has two hardcoded `cd` paths that may differ between machines:
- Section 1 (line 11): `C:\Users\andrea\OneDrive\Documents\github\...`
- Section 2 (line 101): `C:\Users\andrea\OneDrive\Documents\GitHub\...`

The R scripts use `setwd("~/GitHub/...")` which does not resolve on this machine. The wrapper scripts `_run_convert.R` and `_run_plots.R` intercept `setwd` and redirect to the correct path — always use these wrappers instead of sourcing the scripts directly.

When loading a new data file, update the filename in both `import delimited` calls in the do file and the two `read.csv` calls in `convert_stata_output.R`.

## Data flow

```
Data/new_data_utility*.csv
        │
        ▼
Utility_comparison.do  (Stata)
        │
        ├── Output/Data/results_DG.csv       — one row per treatment: all S/N/DA/FU coefficients, SEs, AICs, nobs
        ├── Output/Data/results_NU_DG.csv    — same for base-NU and full-NU models (incl. baseNU_AIC)
        └── Output/Tables/overall_models.tex  — pooled esttab tables
        │
        ▼
Ancillary/convert_stata_output.R  (R)
        │
        ├── Output/Data/model_DG.csv          — per-treatment + IVW Average row
        ├── Output/Data/model_NU_DG.csv
        ├── Output/Data/AIC_DG.csv
        ├── Output/Data/95CI_model_DG.csv
        ├── Output/Data/model_RE_DG.csv       — per-treatment + DL Random-Effects Average row
        ├── Output/Data/model_RE_NU_DG.csv
        ├── Output/Data/tau2_DG.csv           — τ² and I² per coefficient (main models)
        └── Output/Data/tau2_NU_DG.csv        — τ² and I² per coefficient (NU models)
        │
        ┌──────────────────┴──────────────────┐
        ▼                                     ▼
Ancillary/make_tables.R            Ancillary/plot_coefficients.R
Output/Tables/table_estimates.tex  Output/Figures/*.pdf
```

## Architecture

### How Stata exports results

Each model family uses `postfile`/`post`/`postclose` to write one row per treatment directly to a `.dta`, then exports to CSV via `export delimited`. This avoids fragile log-file parsing. The main data is saved to a Stata `tempfile` around each export so it is available for the next section:

```stata
postclose results
tempfile maindata
save `maindata'
use "Utility estimation\Output\Data\results_DG.dta", clear
export delimited "Utility estimation\Output\Data\results_DG.csv", replace
use `maindata', clear
```

### Models estimated (all via `clogit`, grouped by subject `id`)

| Code | Name | Variables | Constraint |
|------|------|-----------|-----------|
| S | Selfish | `payoff` | none |
| N | Social Norm | `payoff mean_app` | none |
| DA | Difference Averse | `payoff rho sigma` | payoff=1 |
| FU | Full | `payoff rho sigma mean_app` | payoff=1 |
| baseNU | Norm Strength (base) | `payoff mean_app sd_app rho sigma` | payoff=1 |
| NU | Norm Strength (full) | `payoff c.mean_app##c.sd_app rho sigma` | payoff=1 |

`rho` and `sigma` follow Charness-Rabin (2002): `r = payoff > endowment/2`, `rho = endowment*r - 2*payoff*r`, etc.

### Summary table (`make_tables.R`)

Reads the "Average" row from `model_DG.csv` and `model_NU_DG.csv` (IVW-averaged coefficients across treatments) and produces `table_estimates.tex` matching the paper layout: one row per model, coefficients on line 1, `[95% CI]` on line 2, blank cells where a parameter is not in that model.

### R averaging method

`convert_stata_output.R` computes two averages:
- **Fixed-effects (IVW)**: `Σ(coeff/se²) / Σ(1/se²)`. Constrained-to-zero coefficients (`sigmaDA`, `rhoFU`, `sigmaFU`) are set to `NA` before averaging.
- **Random-effects (DerSimonian-Laird)**: adds between-study variance τ² to each study's sampling variance before weighting. Also outputs I² (% of total variance due to true heterogeneity) per coefficient.

### Data structure

Each row in `new_data_utility*.csv` is a subject–scenario observation:
- `paper_id`, `treatment_id` — study identifiers
- `id` — subject identifier (clogit group variable)
- `a` — binary choice indicator (1 = chosen action)
- `scenarios` / `endowment` / `payoff` — action amounts
- `mean_app`, `sd_app` — mean and SD of appropriateness ratings
- `game_type` — analysis filters to "DG"

### Paper-specific data adjustments

- `2017Gac013`: endowment reset to 4, scenarios > 4 dropped
- `2017Del037`: scenarios rescaled (`scenarios * endowment`), payoff recalculated
