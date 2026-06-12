# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

This is a research pipeline estimating utility models for a social norm meta-analysis, using Dictator Game (DG) data. The pipeline runs in two stages: Stata estimation → R post-processing and plotting.

## Running the analysis

**Step 1 — Stata estimation** (run interactively in Stata or via `stata -b do`):
```
stata -b do "Utility_comparison.do"
```
Outputs log files to `Output/Logs/` and LaTeX tables to `Output/Tables/`.

**Step 2 — Parse Stata logs and compute averages** (R):
```r
source("Ancillary/convert_stata_output.R")
```
Reads the `.log` files, computes inverse-variance weighted average coefficients, and writes CSVs to `Output/Logs/`.

**Step 3 — Plot coefficients** (R):
```r
source("Ancillary/plot_coefficients.R")
```
Reads the CSVs from Step 2 and saves PDF figures to `Output/Figures/`.

## Known path issues

`Utility_comparison.do` contains two hardcoded `cd` paths that differ between machines:
- Section 1 (descriptive stats): `C:\Users\andrea\OneDrive\Documents\github\...`
- Section 2 (estimation): `C:\Users\a.guido\Documents\GitHub\...`

Update both `cd` lines to match the local path before running.

Similarly, `convert_stata_output.R` uses `setwd("C:/Users/a.guido/...")` — update as needed.

## Key parameter to update

In `Ancillary/convert_stata_output.R`, line 2:
```r
t = 14  # must equal the number of treatments in the data
```
Change `t` whenever the number of treatment IDs in `Data/new_data_utility*.csv` changes. It controls `n_max` when parsing log files.

When loading a new data file, also update the filename in both `Utility_comparison.do` (two `import delimited` calls) and `convert_stata_output.R` (two `read.csv` calls referencing `new_data_utility*.csv`).

## Architecture

### Data flow
```
Data/new_data_utility*.csv
        │
        ▼
Utility_comparison.do  (Stata)
        │
        ├── Output/Logs/stata_*.log          (raw coefficient/AIC/SE output)
        ├── Output/Logs/uncertain_stata_*.log (norm uncertainty models)
        └── Output/Tables/*.tex              (esttab regression tables)
        │
        ▼
Ancillary/convert_stata_output.R  (R)
        │
        ├── Output/Logs/MODEL_DG.csv
        ├── Output/Logs/AIC_DG.csv
        ├── Output/Logs/95CI_MODEL_DG.csv
        ├── Output/Logs/uncertainty_MODEL_DG.csv
        └── Output/Logs/uncertainty_AIC_DG.csv
        │
        ▼
Ancillary/plot_coefficients.R  (R)
        └── Output/Figures/*.pdf
```

### Models estimated (all via `clogit`, grouped by subject `id`)

| Code | Name | Variables |
|------|------|-----------|
| S | Selfish | `payoff` |
| N | Social Norm | `payoff mean_app` |
| DA | Difference Averse | `payoff rho sigma` (Charness-Rabin 2002, constraint: payoff=1) |
| FU | Full | `payoff rho sigma mean_app` |
| NU | Norm Uncertainty | `payoff mean_app sd_app [rho sigma]`, with and without `mean_app × sd_app` interaction |

`rho` and `sigma` are constructed from `payoff` and `endowment` following Charness-Rabin (2002): `r = payoff > endowment/2`, `rho = endowment*r - 2*payoff*r`, etc.

### Data structure

Each row in `new_data_utility*.csv` is a subject–scenario observation. Key columns:
- `paper_id`, `treatment_id` — study identifiers
- `id` — subject identifier (used as `clogit` group)
- `a` — binary choice indicator (1 = chosen)
- `scenarios` — action/donation amount
- `endowment` — total endowment
- `payoff` — sender's payoff
- `mean_app`, `sd_app` — mean and SD of appropriateness ratings for that scenario
- `game_type` — "DG", "ToG", "Donation Game" (analysis currently filters to "DG")

### Paper-specific data adjustments

Two papers require pre-processing before constructing `coop = scenarios/endowment`:
- `2017Gac013`: endowment reset to 4, scenarios > 4 dropped
- `2017Del037`: scenarios rescaled (`scenarios * endowment`), then rounded
