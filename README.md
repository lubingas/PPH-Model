# Misoprostol for postpartum haemorrhage in Uganda: cost-effectiveness model

The decision model, written in R, and an interactive Shiny app for:

> Lubinga SJ, Atukunda EC, Wasswa-Ssalongo G, Babigumira JB. Potential cost-effectiveness of prenatal distribution of misoprostol for prevention of postpartum hemorrhage in Uganda. *PLOS ONE*. 2015;10(11):e0142550. https://doi.org/10.1371/journal.pone.0142550

The model compares prenatal distribution of misoprostol with no misoprostol, by wealth quintile, from governmental and societal perspectives, for three outcomes: PPH cases, PPH deaths and DALYs.

**Live app:** https://theejonslo.shinyapps.io/pph-misoprostol/

## What you can do in the app

- **Parameters:** browse every model input with its source, edit values with checks and a preview of the probabilistic draws, see which inputs you have changed, reset them, and download the current inputs as CSV.
- **Base case:** outcomes averted and incremental costs by wealth quintile, with ICERs for each perspective.
- **One-way sensitivity analysis:** tornado diagrams showing which parameters move each result most.
- **Probabilistic sensitivity analysis:** cost-effectiveness plane and acceptability curves from Monte Carlo draws.

Every chart has a menu for downloading it as PNG or SVG, or its data as CSV.

## Run it locally

In R (4.3 or later):

```r
install.packages(c("MCMCpack", "data.table", "magrittr", "tidyverse", "readxl",
                   "truncnorm", "gt", "pbapply", "highcharter", "shiny", "shinyjs",
                   "progressr", "waiter", "reactable", "bs4Dash"))
shiny::runApp()
```

Run `shiny::runApp()` from this folder, or open `PPH-Model.Rproj` in RStudio and click **Run App**.

## Files

| File | Contents |
|------|----------|
| `global.R` | Packages, chart styling, model constants; reads the input spreadsheet |
| `sub_routines.R` | The decision model, sensitivity analyses and chart functions |
| `server.R`, `ui.R` | The Shiny app |
| `pph_data_input.xlsx` | Model inputs: base-case values, ranges, distributions and sources |
| `analysis/meta-analysis.R` | Random-effects meta-analyses of PPH incidence and of the relative risk of PPH with misoprostol versus placebo, from the trial data in `analysis/` (needs the `metafor` package) |
| `analysis/PPHTree.json` | The structure of the decision tree |

The 2022 version of the model and app, before this update, is tagged [`v2022`](../../tree/v2022).

## Licence

The code is released under the MIT License (see `LICENSE`). The published article, and the decision-tree figure the app shows from it, are © the authors under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
