# Do Renewable Portfolio Standards Lower Power-Sector Carbon Intensity?

A state-level policy evaluation of renewable portfolio standards (RPS) from a group project for
ECON 474 at the University of Illinois Urbana-Champaign (Fall 2026).

**Team:** Group project with 3 classmates. I (Salvatore Sgro) built the dataset (`build_panel.py`) and wrote the
analysis (`rps_analysis.R`).

## Question
When a state enacts an RPS, does the CO2 emitted per MWh of electricity generated in that state fall?

## Data
A state-year panel, 1990-2024, built by `build_panel.py` from:
- **EIA:** annual electricity generation by state and fuel, and power-sector CO2 emissions
- **Lawrence Berkeley National Laboratory:** RPS enactment years and targets
- **NOAA:** state heating degree days, cooling degree days and precipitation

The analysis sample is 46 states (27 that adopted an RPS in 13 different years, and 19 that never did)
over 1,610 state-years.

## Method
Staggered difference-in-differences with the Callaway & Sant'Anna (2021) estimator (R package
`did`), using never-RPS states as controls, plus event studies by years since enactment.

Robustness checks: timing by first compliance year instead of enactment, conditioning on 1990 coal
share, not-yet-treated controls, adding back Vermont and DC, dropping hydro-heavy states, and
two-way fixed effects for comparison. Also policy intensity (RPS target size), mechanisms (shifts in
coal, gas and renewable generation shares) and effects by adoption cohort.

## Main results
- Carbon intensity fell about **10%** after RPS enactment (overall ATT of about -0.11 log points), and
  about 13% on average 9-15 years after enactment.
- Standard errors are analytic and clustered by state. The main estimate is -10.5% (p = 0.047), with a
  wide 95% confidence interval of -19.8% to -0.1%, because there are only 46 state clusters.
- Across the alternative specifications, estimates ranged from -4.5% to -15.4%. The two-way fixed
  effects versions and the version that adds back Vermont and DC are not statistically significant.
- Pre-trends: a joint test does not reject parallel pre-trends.

**Caveats:** emissions are measured where electricity is generated, so imports and out-of-state
renewable credits are not captured; and the estimate is the effect of an RPS together with any
policies adopted at the same time.

## How to run
1. Rebuild the data (optional): `python build_panel.py` (needs pandas, numpy and the files in `raw/`).
2. Open `rps_analysis.R` in RStudio, set the working directory to this folder, and click Source.
   Packages install automatically the first time. Graphs appear in the Plots pane and tables print
   in the Console, ending with a summary of every key number.

## Files
- `build_panel.py`: builds `rps_panel_1990_2024.csv` and `.xlsx` (the xlsx also has a codebook and source list)
- `rps_analysis.R`: the full analysis
- `raw/`: the original EIA, LBNL and NOAA downloads
