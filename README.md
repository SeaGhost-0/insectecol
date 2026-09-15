# insectecol

<!-- badges: start -->
[![R-CMD-check](https://github.com/SeaGhost-0/insectecol/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SeaGhost-0/insectecol/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-informational.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Introduction

**insectecol** (Insect Ecology Data Analysis Toolkit) is a collection of
analytical tools for insect ecology research. It currently ships two
modules:

- **Age-stage, two-sex life table** - main function
  `lifeTable_analyze()` for data already loaded in R, batch function
  `lifeTable_calculate()` for csv files on disk. Validates raw csv
  data, computes the cohort size (N), mean fecundity (F), age-stage
  survival rates (s_xj), age-specific survival (l_x), age-specific
  fecundities (F_xj, m_x), life expectancy (e_x) and the derived
  population parameters (net reproductive rate R0, intrinsic and finite
  rates of increase r and lambda, mean generation time T), draws the
  age-stage survival curves and exports all tabular results and plots
  to Excel in a single run. Fast batch processing of multi-group
  datasets is supported.
- **Dose-response bioassay** - main function `lc50_analyze()` for data
  already loaded in R, one-step batch functions `save_lc50_auto()`
  (Excel tables) and `save_lc50_plot_auto()` (figures) for csv files
  on disk. Estimates lethal concentrations by the traditional and the
  weighted (improved) linear regression methods and by probit analysis,
  with Abbott correction, 95% confidence intervals and chi-square
  goodness-of-fit tests. The lethal proportion can be set freely (25%,
  50%, 70%, 90%, ...), so any LC value such as the LC25, LC70 or LC90
  can be computed - not only the LC50. Regression plots and tables are
  exported to Excel.

### Which main function should I use?

Each module has a **main function** for analysing data that are already
loaded in R (a data frame or plain vectors) and a **one-step batch
function** for processing csv files on disk:

| Module | Main function (data in R) | One-step batch (csv on disk) |
|---|---|---|
| Life table | `lifeTable_analyze()` | `lifeTable_calculate()` |
| Bioassay | `lc50_analyze()` | `save_lc50_auto()` (tables), `save_lc50_plot_auto()` (figures) |

The main functions assemble the data, compute everything and optionally
build the plots, but never write to disk - export is handled separately by
`save_results()`, `save_lc50()` and `save_lc50_plot()`, so the results stay
fully customisable inside R.

For full control, every module can also be driven step by step
(`read_life_table()` -> `lifeTable_calculate_all()` -> `plot_sxj()` ->
`save_results()`, and `read_lc50()` -> `lc50_calculate()` -> `plot_lc50()`
-> `save_lc50()` / `save_lc50_plot()`); see the function reference below.

Planned extensions include more insect ecology indicators, such as the
median lethal temperature/time (LT50) and thermal constants (effective
accumulated temperature).

## Installation

```r
# from CRAN (once accepted)
install.packages("insectecol")

# development version from GitHub
# install.packages("devtools")
devtools::install_github("SeaGhost-0/insectecol")
```

## Quick start: life table

```r
library(insectecol)

# example data shipped with the package
f <- system.file("extdata", "Example.csv", package = "insectecol")
d <- read.csv(f)

# analyse straight from the columns of the loaded data frame
out <- lifeTable_analyze(
  stages = d[2:8],           # one column per immature stage
  adult_days = d$Adult,      # adult survival days
  sex = d$gender,            # "F" / "M" / "N" (died before adult)
  oviposition = d[, 11:17],  # daily oviposition of the females
  file_name = "Example"
)

out$results$N        # cohort size
out$results$R0       # net reproductive rate
out$results$lambda   # finite rate of increase

# survival analysis only: skip the reproduction-related parameters
out2 <- lifeTable_analyze(
  stages = d[2:8],
  adult_days = d$Adult,
  sex = d$gender,
  fecundity = FALSE          # no oviposition data required
)

# with the age-stage survival curve (a ggplot object)
out3 <- lifeTable_analyze(
  stages = d[2:8], adult_days = d$Adult, sex = d$gender,
  oviposition = d[, 11:17], plot = TRUE
)
print(out3$plot)
```

To batch-process csv files on disk instead (each csv gets its own Excel
workbook with all results and the survival curve; an additional
`all.xlsx` summarises every file):

```r
lifeTable_calculate("path/to/lifetable_data")
```

## Quick start: bioassay (LC)

```r
library(insectecol)

# three parallel vectors - no csv file involved
conc   <- c(0, 1.5, 3, 6, 12, 24)
tested <- c(120, 60, 60, 60, 60, 60)
dead   <- c(7, 9, 18, 32, 48, 57)

out <- lc50_analyze(
  concentration = conc,
  tested = tested,
  dead = dead,
  name = "trial1",
  method = "all",            # traditional + improved + probit in one call
  lc = 0.5                   # LC50; any proportion works (e.g. 0.9 = LC90)
)

out$results$summary_df       # estimate, 95% CI, slope, chi-square, ...

# ... or straight from the example csv shipped with the package
f <- system.file("extdata", "bioassay.csv", package = "insectecol")
out_csv <- lc50_analyze(read_lc50(f), method = "all")
out_csv$results$summary_df

# with the regression plot (a named list of ggplot objects)
out2 <- lc50_analyze(
  concentration = conc, tested = tested, dead = dead,
  name = "trial1", method = "probit", plot = TRUE
)
print(out2$plot$trial1)
```

To batch-process csv files on disk instead (one xlsx / one tiff per csv,
written next to the raw data; non-default settings are appended to the
file names, e.g. `LB_48_LC90_probit.xlsx`):

```r
save_lc50_auto("path/to/bioassay_data", method = "probit")
save_lc50_plot_auto("path/to/bioassay_data", method = "probit")
```

## Example data

Two example csv files ship with the package in `inst/extdata/`; the
examples in this README and in the help pages are built on them:

```r
system.file("extdata", "Example.csv", package = "insectecol")   # life table
system.file("extdata", "bioassay.csv", package = "insectecol")  # bioassay
```

- `Example.csv` - life table data in the csv template: one row per
  individual with the `ID`, the days spent in each of the seven
  immature stages (egg, four larval instars, prepupa, pupa), the adult
  survival days, the sex (`F`/`M`/`N`) and the daily oviposition of
  the females.
- `bioassay.csv` - bioassay data: one row per concentration group with
  the columns `Concentration` (0 = control group for the Abbott
  correction), `Tested` and `Dead`.

The file layouts are described in detail under
[Data formats](#data-formats).

## Data formats

### Life table csv

One row per individual. If the sex column is at position `n`:

| Column | Content |
|---|---|
| 1 | individual ID (header `ID`) |
| 2 ... n-2 | days spent in each immature stage (egg, instars, prepupa, pupa) |
| n-1 | adult survival days |
| n | sex: `F`, `M` or `N` (died before the adult stage); header `gender` |
| n+1 ... | daily oviposition of the females (one column per day) |

The first line must contain the stage names as headers. The sex column is
located automatically, and the file encoding is detected automatically
(UTF-8 and GBK are both supported).

### Bioassay csv

One row per concentration group (replicates = repeated concentration
values):

| Column | Content |
|---|---|
| `Concentration` | the concentration (0 = control group, used for the Abbott correction) |
| `Tested` | number of insects tested |
| `Dead` | number of dead insects |

Headers are matched loosely, so a header like
`Concentration (mg/L)` is recognised as well. UTF-8 (with BOM) and GBK
encodings are supported.

## Function reference

### Life table module

| Function | Purpose |
|---|---|
| `lifeTable_analyze()` | **main function** - analyse data in R (build + compute + optional plot) |
| `build_life_table()` | build a `life_table` object from user-supplied columns |
| `read_life_table()` | read and validate a life table csv file |
| `lifeTable_calculate()` | batch: analyse every csv in a folder and export to Excel |
| `lifeTable_calculate_all()` | all parameters of one `life_table` object |
| `calc_N()`, `calc_F()`, `calc_sxj()`, `calc_lx()`, `calc_fxj()`, `calc_mx()`, `calc_ex()`, `calc_R0()`, `calc_r()`, `calc_lambda()`, `calc_T()` | individual indicators |
| `plot_sxj()` | age-stage survival rate curves |
| `save_results()` | export one analysis to Excel |
| `check_life_table()`, `get_stage_names()`, `default_stage_names()` | helpers |

### Bioassay module

| Function | Purpose |
|---|---|
| `lc50_analyze()` | **main function** - analyse data in R (build + compute + optional plots) |
| `read_lc50()` | read bioassay csv file(s) |
| `lc50_calculate()` | compute the LC values; several methods (or `"all"`) in one call |
| `plot_lc50()` | regression plots |
| `save_lc50()` | export results to Excel |
| `save_lc50_plot()` | export figures |
| `save_lc50_auto()` | one-step batch: csv file(s) -> Excel workbook(s) |
| `save_lc50_plot_auto()` | one-step batch: csv file(s) -> tiff figure(s) |
| `check_path_type()` | path helper (folder / csv file) |

## Updates

### 1.0.1 (CRAN submission round)

**New features**

- New main functions `lifeTable_analyze()` and `build_life_table()` for
  the life table module: analyse data already loaded in R - pass the stage
  columns, adult days, sex and oviposition columns of any data frame; no
  package-conform csv file required. Nothing is written to disk.
- New main function `lc50_analyze()` for the bioassay module: accepts a
  data frame, a named list of data frames or three parallel vectors
  (concentration, tested, dead) and computes the LC values with the
  selected method(s).
- New one-step batch exporters `save_lc50_auto()` and
  `save_lc50_plot_auto()`: read every csv in a folder, compute the LC
  values and write the xlsx/tiff results next to the raw data, with
  self-documenting file names (e.g. `LB_48_LC90_probit.xlsx`).
- `lc50_calculate()` now accepts several methods (or `method = "all"`) in
  a single call and reports the status of every method for every file.
- Redesigned LC regression plots: replicate rows pooled with Wilson score
  intervals, pointwise confidence band of the fitted curve, dashed LC
  reference lines with automatic tick and label placement, and a `shape`
  argument switching between the log10 (sigmoid) and the linear
  concentration axis. The LC reference label now also shows the 95%
  confidence interval of the estimate on a second line, e.g.
  `LC50 = 1.23 mg/L` over `(0.98-1.55)`, and optionally the
  chi-square goodness-of-fit result on a third line (`lc_ci = FALSE`
  / `lc_p = FALSE` omit the lines; `lc_lab_gap` (left of the reference
  line) / `lc_lab_gap_right` (right of it) / `lc_lab_dy`
  fine-tune the label position, `lc_lab_lh` its line spacing, given in
  multiples of the font size). Every line
  of the label is drawn on its own so that all of them share one vertical
  axis (grid would otherwise justify each line of a multi-line string by
  its own width), and the label is kept inside the panel. The block is
  placed in the diagonal quadrant around the crossing that the rising
  fitted curve never enters - above it when the label sits left of the
  vertical reference line, below it when it sits right - anchored by the
  edge facing the crossing, so adding or dropping a line (or changing
  `lc_lab_lh`) grows the block away from the crossing instead of onto
  the dashed line or the curve. The LC
  estimate is additionally marked by a circle where it lies on the fitted
  curve, i.e. where the two dashed reference lines meet.
- Customisable titles, axis titles and legend labels for the age-stage
  survival curves (`plot_sxj()` and `lifeTable_analyze()`).

**CRAN fixes**

- Quoted the software names in DESCRIPTION (`'csv'`, `'Excel'`).
- Unwrapped the example code.
- Replaced `cat()` with `message()` for all progress output.
- showtext is now enabled at export time and also works for the pdf
  device (vector devices are converted at 72 pt/in); the previous session
  settings are restored afterwards, so exports look identical in every R
  session.
- Bumped the version to 1.0.1.

**Internal changes**

- `lc50_traditional()`, `lc50_improved()` and `lc50_probit()` are no
  longer exported; select them via the `method` argument of
  `lc50_calculate()` / `lc50_analyze()`.
- Font fallback chain for publication-quality figures: system Times New
  Roman -> bundled Liberation Serif -> `"sans"` (see the font note below).
- The oviposition consistency check is skipped gracefully when the data
  end at the sex column (no oviposition columns).
- Declared `R (>= 3.5)` and `LazyData` in DESCRIPTION.
- Added a GitHub Actions R-CMD-check workflow (macOS / Windows / Linux:
  release, devel and oldrel).
- Updated the CITATION file.
- Added example data for the planned thermal-constants module
  (`inst/extdata/gdd_example.csv`).
- Normalised the source indentation to two spaces and rewrote the
  README (main-function guide, example data section, quick starts on
  the shipped csv files).

### 1.0.0

- Initial release: the age-stage, two-sex life table module and the
  dose-response bioassay (LC) module.

## Note on bundled fonts

This package bundles the Liberation Serif font (SIL Open Font License 1.1)
in `inst/fonts/` for publication-quality figures. The full license text is
shipped as `inst/fonts/OFL.txt`. All other components of the package are
licensed under MIT.

## License

MIT (see [LICENSE](LICENSE)). The bundled Liberation Serif font is
licensed under the SIL Open Font License 1.1.

## Citation

```r
citation("insectecol")
```

If you use the life table module in a publication, please also cite the
method papers behind the age-stage, two-sex theory (Chi & Liu 1985; Chi
1988 - see the references of `read_life_table()`), and for probit
analysis Finney (1971) together with Abbott (1925) for the correction of
natural mortality.
