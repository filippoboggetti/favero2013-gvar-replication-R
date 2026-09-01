# Replication of Favero (2013) in R — euro-area government bond spreads in a GVAR

R replication of

> Carlo A. Favero (2013), "Modelling and forecasting government bond spreads in the euro area: A GVAR model",
> *Journal of Econometrics* 177(2), 343–356.

written by Filippo Felice Boggetti for a project with Prof. Carlo Favero (HLC Program, Bocconi University,
January–March 2024). The code rebuilds the dataset from the paper's replication workbook, reproduces the
exploratory figures, estimates the traditional spread model (Table 1) and the GVAR (Table 2) with and without
panel restrictions, and reproduces the paper's Figures 5, 6 and 8 and the out-of-sample forecasting exercise.

## Pipeline

`Main.R` installs/loads the required packages (`R_scripts/preparePackages.R`) and then sources, in order:

| Step | Script | What it does |
|---|---|---|
| 1 | `Exploratory_Analysis.R` | Figures 1–2: co-movement of spreads vs. real variables |
| 2 | `Dataset.R` | Stacked dataset from the replication workbook (trade weights, ratings, CDS, swaps, exchange rates, US corporate spread, EIB yields, deficit and debt) |
| 3 | `d_Yields_1990_2011.R` | Weekly (5-day) yields 1990–2011 |
| 4 | `Monthly_Dataset.R` | Monthly stacked dataset used in the estimation |
| 5 | `Efficient_Global_variables.R` | GVAR "global" (trade-weighted) variables |
| 6 | `Graph_5.R` | Figure 5 |
| 7 | `Table_1.R` | Traditional model, Table 1 (no panel restrictions / panel restrictions), rendered to `.tex` and PDF |
| 8 | `Table_2.R` | GVAR, Table 2 (no panel restrictions / panel restrictions), rendered to `.tex` and PDF |
| 9 | `Figure_6.R` | Figures 6.1–6.3 |
| 10 | `GVAR_forecasting.R` | Out-of-sample forecasts with the GVAR |
| 11 | `Fig8_a.R` | Figure 8 (upper panel) |

`Clean.R`, `Clean1.R`, `ConvertColumn.R`, `ConvertQ.R`, `safe_merge.R` are small helpers. `Tables/` holds the
LaTeX sources of Tables 1–2 produced by the last run.

Two scripts are kept but **not sourced** by `Main.R`: `Forecats_Traditional_model.R` (out-of-sample forecasts
of the traditional model, iterated dynamically without bootstrap) and `GIRF.R` (generalised impulse responses
to a 200 bp Greek-spread shock with bootstrapped bands, re-estimating the system by `systemfit` at every
replication — correct but slow). For the project those two pieces, and the confidence bands of the paper's
Figures 7.1–7.4, were produced in EViews instead; the EViews code is not part of this repository.

## Data

The scripts read the paper's replication workbook `Database_cf1.xls` (sheets `d_trade_weights`, `d_Ratings`,
`d_CDS_Premia_10Y_&_5Y`, `d_Benchmark_10Y_RY_&_Swaps`, `d_Exchange_rates`, `d_US_corporate_AAA_BAA`,
`d_EIB_10Y`, deficit/GDP and debt/GDP). It is Prof. Favero's material and is **not redistributed here**:
obtain it from the paper's replication files and place it in the repository root before running.

## Requirements and how to run

R 4.x. The packages are listed at the top of `Main.R` and installed automatically if missing (`readxl`,
`dplyr`, `ggplot2`, `zoo`, `reshape2`, `tidyverse`, `xts`, `openxlsx`, `lubridate`, `patchwork`, `gridExtra`,
`systemfit`, `broom`, `xtable`, `kableExtra`, `plm`, `MASS`, …). Rendering Tables 1–2 to PDF needs a LaTeX
installation (the table scripts fall back to installing TinyTeX).

`Main.R` sets the working directory to its own location with `rstudioapi`, so the intended way to run it is
to open it in **RStudio** and run it top to bottom; from a plain R session replace that line with
`setwd("<path to this folder>")`. Outputs are written to `Tables/`, `Plots/`, `backup_excel/` (Excel sanity
checks of every intermediate dataset) and `Database.RDS/`.

## Status

Written and last used in RStudio in Spring 2024 (R 4.3). **Re-run on 2026-09-01** from a clean copy with
`Rscript Main.R` (R 4.3.2, macOS, `setwd` pointed at the folder): completes in about 15 s with exit status 0,
regenerating Figures 1–6 and 8a, Tables 1–2 (`.tex` and PDF) and all intermediate datasets. Only
package-deprecation warnings (dplyr/ggplot2/xts) are printed.

The `Tables/*.tex` files in this repository are the output of that 2026 run. They are **not** numerically identical
to the versions produced in April 2024 (the project's archived tables came from an earlier vintage of the scripts
and/or older package versions; coefficient estimates differ in the second–third decimal and in places by more), so
treat the tables as what *this* code produces, not as the exact numbers of the project write-up.

## License

MIT (see `LICENSE`) for the code in this repository. The paper and its data are © the author/publisher.
