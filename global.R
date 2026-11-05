# PPH Model — Global setup
# Loaded once by Shiny on startup. Loads all packages, sources model
# functions, and defines shared constants used by server.R and sub_routines.R.

# ── Packages ──────────────────────────────────────────────────────────────────
# Loaded with explicit library() calls so that shinyapps.io (rsconnect) can
# detect and install them when the app is deployed. To run locally, install
# them once with install.packages().
#
# MCMCpack, data.table, and magrittr are loaded FIRST so that when tidyverse
# loads afterwards, dplyr::select / dplyr::filter correctly mask MASS::select
# (MASS is a dependency of MCMCpack and would otherwise shadow dplyr functions).

library(MCMCpack)     # rdirichlet() — loaded early so MASS loads before dplyr
library(data.table)   # as.data.table(), setDF(), rbindlist()
library(magrittr)     # set_colnames()
library(tidyverse)    # dplyr, purrr, tibble, tidyr, ... (masks MASS::select)
library(readxl)       # read_excel()
library(truncnorm)    # rtruncnorm()
library(gt)           # gt tables (TabulateOutcomes)
library(pbapply)      # pblapply() / pbsapply() progress bars
library(highcharter)  # highchart plots
library(shiny)        # Shiny framework
library(shinyjs)      # useShinyjs(), reset()
library(progressr)    # progressor()
library(waiter)       # use_waiter()
library(reactable)    # renderReactable(), reactableOutput()
library(bs4Dash)      # dashboardPage() and bs4Dash widgets

pboptions(type = "txt")

# ── Chart styling ─────────────────────────────────────────────────────────────
# One colour per meaning, used by every chart so a colour always means the same
# thing: blue = governmental perspective, orange = societal perspective,
# green = health outcome averted, greys = parameter at its low / high value.
# These pairs were checked for colour-vision deficiency and contrast.
chart_cols <- list(
  gov     = "#2a78d6",
  soc     = "#eb6834",
  outcome = "#1baf7a",
  low     = "#a9a79e",
  high    = "#52514e",
  ink     = "#0b0b0b",
  ink2    = "#52514e",
  muted   = "#898781",
  grid    = "#e1e0d9",
  axis    = "#c3c2b7",
  band    = "#f3f2ee"
)

# Shared Highcharts theme: recessive grid and axes, system font, no credits
# watermark, and a download menu (PNG / SVG / CSV) on every chart.
options(highcharter.theme = hc_theme(
  colors = c(chart_cols$gov, chart_cols$soc),
  chart = list(
    backgroundColor = "#ffffff",
    style = list(fontFamily = 'system-ui, -apple-system, "Segoe UI", Roboto, sans-serif')
  ),
  title    = list(align = "left", style = list(color = chart_cols$ink, fontSize = "15px", fontWeight = "600")),
  subtitle = list(align = "left", style = list(color = chart_cols$ink2, fontSize = "12px")),
  xAxis = list(
    lineColor = chart_cols$axis, tickColor = chart_cols$axis, gridLineColor = chart_cols$grid,
    labels = list(style = list(color = chart_cols$ink2, fontSize = "12px")),
    title  = list(style = list(color = chart_cols$ink2, fontWeight = "500"))
  ),
  yAxis = list(
    lineColor = chart_cols$axis, gridLineColor = chart_cols$grid, gridLineWidth = 1,
    labels = list(style = list(color = chart_cols$ink2, fontSize = "12px")),
    title  = list(style = list(color = chart_cols$ink2, fontWeight = "500"))
  ),
  legend  = list(itemStyle = list(color = chart_cols$ink2, fontWeight = "500")),
  tooltip = list(backgroundColor = "#ffffff", borderColor = "rgba(11,11,11,0.15)",
                 borderRadius = 6, shadow = TRUE, style = list(color = chart_cols$ink)),
  plotOptions = list(
    series = list(turboThreshold = 0),        # allow > 1,000 PSA draws per series
    column = list(borderWidth = 0, borderRadius = 3, groupPadding = 0.12, pointPadding = 0.04),
    bar    = list(borderWidth = 0, borderRadius = 3)
  ),
  credits   = list(enabled = FALSE),
  exporting = list(enabled = TRUE)
))

# highcharter's own defaults switch the download menu and credits off; turn the
# menu on (PNG / SVG / CSV) for every chart.
local({
  hc_defaults <- getOption("highcharter.chart")
  hc_defaults$exporting <- list(enabled = TRUE)
  options(highcharter.chart = hc_defaults)
  # Highcharts separates thousands with a space by default; use commas ($1,000).
  hc_lang <- getOption("highcharter.lang")
  hc_lang$thousandsSep <- ","
  options(highcharter.lang = hc_lang)
})

# ── Model functions ───────────────────────────────────────────────────────────
source("sub_routines.R")
source("equations.R")

# ── Constants ─────────────────────────────────────────────────────────────────
interventions <- c("Misoprostol", "No Misoprostol")

# Load parameter spreadsheet
data <- read_excel("pph_data_input.xlsx", sheet = 1)

# Readable labels for parameters in the tornado diagrams (variable -> Description)
param_labels <- setNames(trimws(data$Description), data$variable)[!is.na(data$variable)]

# General model constants (not varied in sensitivity analyses)
# (life expectancy and the discount rate are read from the spreadsheet:
#  lifeexpectancy and cDiscountCosts)
procDaysYr <- 264
procHrsDay <- 8
age        <- c(18, 27, 42)

# Misoprostol programme cost parameters
nWomen      <- 10000
nHW         <- 200
daysTraining <- 5
cTrain      <- 10      # USD/day
nurseFTE    <- 20
nNurse      <- 50
cPackage    <- 0.5     # proportion of misoprostol unit cost

# Named lists used by path functions
TxTrjList <- list("Hospital", "HC", "Friend", "TBA", "Unassisted")
names(TxTrjList) <- c("Hospital", "HC", "Friend", "TBA", "Unassisted")

QuintileList <- list("Lowest", "Second", "Middle", "Fourth", "Highest")
names(QuintileList) <- c("Lowest", "Second", "Middle", "Fourth", "Highest")

# Outcomes iterated over in sensitivity analyses
outcomes <- list("Incidence", "Mortality", "DALYs")
names(outcomes) <- c("Incidence", "Mortality", "DALYs")
