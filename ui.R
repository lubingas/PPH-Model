#
# PPH Model — UI
#

dashboardPage(
  dark = NULL,

  # ── Header ─────────────────────────────────────────────────────────────────
  header = dashboardHeader(title = "PPH CEA — Misoprostol Uganda"),

  # ── Sidebar ─────────────────────────────────────────────────────────────────
  sidebar = dashboardSidebar(
    skin = "light", elevation = 2, status = "primary",
    minified = TRUE, expandOnHover = TRUE,
    sidebarMenu(id = "nav",
      menuItem("Home",               tabName = "hometab",
               icon = icon("home",         lib = "font-awesome")),
      menuItem("Parameters",         tabName = "datatab",
               icon = icon("table",        lib = "font-awesome")),
      menuItem("Base Case",          tabName = "basetab",
               icon = icon("chart-bar",    lib = "font-awesome")),
      menuItem("Sensitivity Analyses",
               icon = icon("cogs", lib = "font-awesome"),
               startExpanded = TRUE,
        menuSubItem("One-way (OWSA)", tabName = "owsatab",
                    icon = icon("sliders-h",  lib = "font-awesome")),
        menuSubItem("Probabilistic",  tabName = "psatab",
                    icon = icon("line-chart", lib = "font-awesome"))
      )
    ),
    div(
      style = paste("padding:10px 15px 6px; font-size:.78em; line-height:1.5;",
                    "opacity:.7; border-top:1px solid rgba(255,255,255,.15);",
                    "margin-top:8px;"),
      "Cost-effectiveness of prenatal misoprostol",
      tags$br(), "for PPH prevention — Uganda",
      tags$br(), tags$em("Lubinga et al. (2015) PLoS ONE")
    )
  ),

  # (no controlbar — SA settings live inside their tabs)

  # ── Body ────────────────────────────────────────────────────────────────────
  body = dashboardBody(
    useShinyjs(),
    use_waiter(),

    # ── Custom CSS ─────────────────────────────────────────────────────────────
    tags$head(tags$style(HTML("

      /* ── Metric cards ───────────────────────────────────────── */
      .metric-card {
        border-top: 4px solid var(--mc, #1B6CA8);
        border-radius: 8px; padding: 20px 16px;
        text-align: center; margin-bottom: 20px;
        transition: box-shadow .2s;
      }
      .metric-card:hover { box-shadow: 0 4px 18px rgba(0,0,0,.15); }
      .mc-val {
        font-size: 2em; font-weight: 700;
        color: var(--mc, #1B6CA8); line-height: 1.1; margin-bottom: 6px;
      }
      .mc-lbl { font-size: .82em; color: #6c757d; line-height: 1.4; }
      .dark-mode .mc-lbl    { color: #adb5bd; }
      .dark-mode .metric-card { background: rgba(255,255,255,.06); }

      /* ── Key-finding tiles ──────────────────────────────────── */
      .kf-tile { border-radius: 8px; padding: 14px 12px; text-align: center; margin-bottom: 12px; }
      .kf-val  { font-size: 1.5em; font-weight: 700; margin-bottom: 4px; }
      .kf-lbl  { font-size: .82em; margin: 0; }
      .kf-sub  { font-size: .75em; opacity: .7; }

      /* ── Step circles ───────────────────────────────────────── */
      .step-circle {
        width: 50px; height: 50px; border-radius: 50%;
        display: flex; align-items: center; justify-content: center;
        font-size: 1.4em; font-weight: 700; color: #fff; margin: 0 auto 12px;
      }
      .step-lbl { font-size: .88em; color: #555; }
      .dark-mode .step-lbl { color: #adb5bd; }

      /* ── Inline SA settings ─────────────────────────────────── */
      .sa-settings .card-body { padding: .75rem 1rem; }

      /* ── Info notes ─────────────────────────────────────────── */
      .info-note {
        font-size: .86em; color: #555;
        background: rgba(27,106,168,.07);
        border-left: 3px solid #1B6CA8;
        border-radius: 4px; padding: 8px 12px; margin-bottom: 16px;
      }
      .dark-mode .info-note { background: rgba(100,181,246,.1); color: #90caf9; }

      /* ── Parameters page ────────────────────────────────────── */
      .param-toolbar { display: flex; flex-wrap: wrap; align-items: flex-end;
                       justify-content: space-between; gap: 12px; margin-bottom: 4px; }
      .param-filter .form-group { margin-bottom: 8px; }
      .param-actions { display: flex; flex-wrap: wrap; align-items: center; gap: 8px; margin-bottom: 8px; }
      .edit-summary { font-size: .88em; color: #52514e; margin-right: 6px; }
      .edit-summary.edited { color: #8a4b00; font-weight: 600; }
      .param-code { font-family: SFMono-Regular, Menlo, Consolas, monospace;
                    font-size: .78em; color: #898781; margin-top: 2px; }
      .row-edited { background: rgba(250,178,25,.10); box-shadow: inset 3px 0 0 #eda100; }
      .chip-edited { display: inline-block; font-size: .72em; font-weight: 600; letter-spacing: .02em;
                     color: #6b4300; background: #fde7b0; border-radius: 10px; padding: 1px 8px; }
      .rt-tr-group .rt-tr:not(.rt-tr-group-header) { cursor: pointer; }
      .edited-note { border-left-color: #eda100; background: rgba(250,178,25,.10); }

      /* ── Parameter edit dialog ──────────────────────────────── */
      .pe-title { line-height: 1.3; }
      .pe-sub { font-size: .72em; font-weight: 400; color: #898781; margin-top: 4px; }
      .pe-sub code { color: #52514e; background: #f3f2ee; border-radius: 3px; padding: 1px 5px; }
      .pe-section { margin-bottom: 14px; }
      .pe-head { font-size: .78em; font-weight: 700; text-transform: uppercase; letter-spacing: .06em;
                 color: #52514e; border-bottom: 1px solid #e1e0d9; padding-bottom: 4px; margin-bottom: 10px; }
      .pe-note { font-size: .86em; color: #52514e; margin-bottom: 8px; }
      .pe-derived { font-size: .86em; color: #52514e; padding-top: 34px; }
      .pe-errors { font-size: .88em; color: #a12b2b; background: rgba(208,59,59,.08);
                   border-left: 3px solid #d03b3b; border-radius: 4px; padding: 8px 12px; margin-bottom: 12px; }
      .pe-source { font-size: .82em; color: #52514e; }
      .dark-mode .param-code, .dark-mode .pe-sub { color: #adb5bd; }

    "))),

    tabItems(

      # ════════════════════════════════ HOME ═══════════════════════════════════
      tabItem(tabName = "hometab",

        # Publication banner
        fluidRow(column(12,
          bs4Card(width = NULL, status = "primary", solidHeader = TRUE,
            title = tags$span(icon("file"), " Publication"),
            collapsible = FALSE,
            fluidRow(
              column(8,
                tags$p(class = "mb-1",
                  tags$strong("Lubinga SJ, Atukunda EC, Wasswa-Ssalongo G, Babigumira JB (2015).")),
                tags$p(class = "mb-2",
                  tags$em("Potential Cost-Effectiveness of Prenatal Distribution of Misoprostol",
                          " for Prevention of Postpartum Hemorrhage in Uganda."),
                  tags$strong(" PLoS ONE"), " 10(11): e0142550."),
                tags$a(class = "btn btn-sm btn-outline-light",
                  href = "https://doi.org/10.1371/journal.pone.0142550", target = "_blank",
                  icon("external-link"), " doi:10.1371/journal.pone.0142550")
              ),
              column(4,
                div(class = "alert alert-warning mb-0 py-2 px-3", style = "font-size:.85em;",
                  icon("exclamation-triangle"),
                  tags$strong(" Parameters reflect 2012 Uganda data."),
                  " Update the Parameters tab before applying to other settings or time periods."
                )
              )
            )
          )
        )),

        # Background | Model structure
        fluidRow(
          column(6,
            bs4Card(width = NULL, solidHeader = TRUE, status = "secondary",
              title = tags$span(icon("heartbeat"), " Background"),
              collapsible = TRUE,
              tags$p("Postpartum hemorrhage (PPH) accounts for ~15% of maternal deaths",
                     " in sub-Saharan Africa. In Uganda, nearly half of all births occur",
                     " outside health facilities, limiting access to oxytocin — the",
                     " first-line uterotonic."),
              tags$p("Misoprostol (600 mcg oral) needs no refrigeration, no injection,",
                     " and can be self-administered. Distributing it prenatally could",
                     " substantially increase uterotonic coverage across all delivery settings."),
              tags$p("This model evaluates whether such a programme represents good",
                     " value for money from both governmental (direct medical costs)",
                     " and modified societal perspectives.")
            )
          ),
          column(6,
            bs4Card(width = NULL, solidHeader = TRUE, status = "secondary",
              title = tags$span(icon("sitemap"), " Model structure"),
              collapsible = TRUE,
              tags$ol(class = "ps-3",
                tags$li(tags$strong("Wealth quintile"),
                        " — determines delivery-pathway probabilities (UDHS 2011)"),
                tags$li(tags$strong("Delivery setting"),
                        " — hospital, health centre, friend/relative, TBA, or unassisted"),
                tags$li(tags$strong("Uterotonic access"),
                        " — oxytocin, misoprostol (if distributed), or none;",
                        " misoprostol misuse (uterine rupture, stillbirth) is modelled"),
                tags$li(tags$strong("PPH occurrence"),
                        " — probability varies by setting and uterotonic received"),
                tags$li(tags$strong("Emergency obstetric care"),
                        " — access probability varies by delivery setting"),
                tags$li(tags$strong("Outcome"), " — survive or die from PPH")
              ),
              div(class = "info-note mt-2",
                icon("info-circle"),
                " Outcomes: PPH incidence, mortality, DALYs, and ICERs",
                " (governmental and societal perspectives)."
              )
            )
          )
        ),

        # Decision tree
        fluidRow(column(12,
          bs4Card(width = NULL, solidHeader = TRUE, status = "secondary",
            title = tags$span(icon("share-alt"), " Decision tree (Fig. 1, Lubinga et al. 2015)"),
            collapsible = TRUE,
            tags$p(class = "info-note",
              icon("copyright"),
              " Reproduced under ",
              tags$a("CC BY 4.0", href = "https://creativecommons.org/licenses/by/4.0/",
                     target = "_blank"),
              ". Requires internet access to display."
            ),
            div(style = "text-align:center;",
              tags$img(
                src = paste0("https://journals.plos.org/plosone/article/figure/image",
                             "?size=large&id=10.1371/journal.pone.0142550.g001"),
                alt = "Decision tree: delivery pathway trajectory and outcomes",
                style = "max-width:100%; height:auto; border-radius:4px;",
                onerror = paste0("this.style.display='none';",
                                 "document.getElementById('tree-fb').style.display='block';")
              ),
              div(id = "tree-fb", style = "display:none;",
                div(class = "alert alert-secondary",
                  icon("exclamation-circle"), " Decision tree figure requires internet access. ",
                  tags$a("View on PLOS ONE",
                         href = paste0("https://journals.plos.org/plosone/article/figure/image",
                                       "?size=large&id=10.1371/journal.pone.0142550.g001"),
                         target = "_blank"))
              )
            )
          )
        )),

        # Key findings
        fluidRow(column(12,
          bs4Card(width = NULL, solidHeader = TRUE, status = "success",
            title = tags$span(icon("chart-bar"), " Key findings — Lubinga et al. (2015)"),
            collapsible = TRUE,
            tags$p(class = "info-note",
              "Published base-case results. Run the app to reproduce or update with revised parameters."),
            fluidRow(
              column(4,
                div(class = "kf-tile", style = "background:#d4edda;",
                  div(class = "kf-val", style = "color:#155724;", "\u22121.0%"),
                  tags$p(class = "kf-lbl", style = "color:#155724;", "Reduction in PPH incidence"),
                  tags$p(class = "kf-sub", style = "color:#155724;", "95% CrI: 0.55%, 1.95%"))),
              column(4,
                div(class = "kf-tile", style = "background:#d4edda;",
                  div(class = "kf-val", style = "color:#155724;", "\u22120.08%"),
                  tags$p(class = "kf-lbl", style = "color:#155724;", "Reduction in mortality"),
                  tags$p(class = "kf-sub", style = "color:#155724;", "95% CrI: 0.04%, 0.13%"))),
              column(4,
                div(class = "kf-tile", style = "background:#d4edda;",
                  div(class = "kf-val", style = "color:#155724;", "0.02 DALYs"),
                  tags$p(class = "kf-lbl", style = "color:#155724;", "Averted per woman"),
                  tags$p(class = "kf-sub", style = "color:#155724;", "95% CrI: 0.01, 0.03")))
            ),
            tags$br(),
            fluidRow(
              column(6,
                div(class = "kf-tile", style = "background:#cce5ff;",
                  div(class = "kf-val", style = "color:#004085;", "US$191 / DALY"),
                  tags$p(class = "kf-lbl", style = "color:#004085;",
                         "ICER — Governmental perspective"),
                  tags$p(class = "kf-sub", style = "color:#004085;", "95% CrI: $82, $443"))),
              column(6,
                div(class = "kf-tile", style = "background:#cce5ff;",
                  div(class = "kf-val", style = "color:#004085;", "US$73 / DALY"),
                  tags$p(class = "kf-lbl", style = "color:#004085;",
                         "ICER — Societal perspective"),
                  tags$p(class = "kf-sub", style = "color:#004085;",
                         "95% CrI: \u2212$86, $256")))
            ),
            tags$br(),
            div(class = "alert alert-success mb-0",
              icon("check-circle"),
              " At US$1,641/DALY (3\u00d7 Uganda GDP), misoprostol distribution was",
              tags$strong(" cost-effective in 100% of PSA iterations"), " from both perspectives.")
          )
        )),

        # How to use
        fluidRow(column(12,
          bs4Card(width = NULL, solidHeader = FALSE, status = "info",
            title = tags$span(icon("question-circle"), " How to use this app"),
            collapsible = TRUE, collapsed = TRUE,
            fluidRow(
              column(3, div(style = "text-align:center; padding:10px;",
                div(class = "step-circle", style = "background:#1B6CA8;", "1"),
                tags$strong("Review parameters"),
                tags$p(class = "step-lbl",
                  "Go to ", tags$strong("Parameters"), ". Use the category",
                  " selector to jump to any group. Click",
                  tags$strong("Edit"), " to change values."))),
              column(3, div(style = "text-align:center; padding:10px;",
                div(class = "step-circle", style = "background:#2e7d32;", "2"),
                tags$strong("View base case"),
                tags$p(class = "step-lbl",
                  "The ", tags$strong("Base Case"), " tab updates automatically.",
                  " Switch between DALYs, Mortality and Incidence tabs;",
                  " the headline metrics update to match."))),
              column(3, div(style = "text-align:center; padding:10px;",
                div(class = "step-circle", style = "background:#f57c00;", "3"),
                tags$strong("Run OWSA"),
                tags$p(class = "step-lbl",
                  "Go to ", tags$strong("One-way (OWSA)"),". Set the number of",
                  " variables, click ", tags$strong("Run OWSA"),
                  ". Tornado diagrams rank parameters by impact."))),
              column(3, div(style = "text-align:center; padding:10px;",
                div(class = "step-circle", style = "background:#00838f;", "4"),
                tags$strong("Run PSA"),
                tags$p(class = "step-lbl",
                  "Go to ", tags$strong("Probabilistic"),". Set simulations",
                  " and max WTP, click ", tags$strong("Run PSA"),
                  ". Scatter plots and a CEAC are generated.")))
            )
          )
        ))
      ),

      # ════════════════════════════════ DATA ═══════════════════════════════════
      tabItem(tabName = "datatab",
        fluidRow(column(12,
          bs4Card(width = NULL, title = "Model inputs", collapsible = FALSE, solidHeader = FALSE,
            div(class = "param-toolbar",
              div(class = "param-filter",
                selectInput("data_group", label = "Show", width = "320px",
                  choices = list(
                    "All parameters" = c("All parameters, by section" = "ALL"),
                    "Delivery pathways" = c(
                      "Wealth quintiles"              = "Wealth quintiles",
                      "Health facility delivery"      = "HF delivery probability",
                      "Hospital delivery"             = "Hospital delivery probability",
                      "Unassisted delivery"           = "Unassisted delivery",
                      "Friend or relative at delivery"= "Friend/relative delivery",
                      "TBA at delivery"               = "TBA delivery"),
                    "Uterotonic access" = c(
                      "Oxytocin coverage"                   = "Oxytocin coverage",
                      "Misoprostol coverage (programme)"    = "Misoprostol coverage (programme)",
                      "Misoprostol coverage (no programme)" = "Misoprostol coverage (control)"),
                    "Other" = c(
                      "General parameters"       = "General parameters",
                      "Emergency obstetric care" = "Emergency obstetric care",
                      "Health resource use"      = "Health resource use",
                      "Age distribution"         = "Age distribution")
                  ))),
              div(class = "param-actions",
                uiOutput("editSummary", inline = TRUE),
                actionButton("reset_all", "Reset all", icon = icon("rotate-left"),
                             status = "secondary", outline = TRUE, size = "sm"),
                downloadButton("download_inputs", "Download inputs (CSV)",
                               class = "btn-outline-primary btn-sm"))
            ),
            div(class = "info-note",
              icon("info-circle"),
              " Click any parameter to edit it. Saved changes flow into the base case and both",
              " sensitivity analyses straight away. Edited inputs are marked and can be reset",
              " individually or all at once.")
          )
        )),
        fluidRow(column(12,
          bs4Card(width = NULL, collapsible = FALSE, headerBorder = FALSE,
            reactableOutput("dataTable"))
        ))
      ),

      # ══════════════════════════════ BASE CASE ════════════════════════════════
      tabItem(tabName = "basetab",
        uiOutput("editedNote"),

        # Dynamic headline metrics
        uiOutput("summaryBoxes"),

        # Outcome charts — DALYs first
        fluidRow(column(12,
          tabBox(width = NULL, id = "outcomes",
            title = "", status = "secondary",
             type = "tabs",

            tabPanel(title = "DALYs",
              div(class = "info-note",
                icon("info-circle"),
                " Left: DALYs averted per 1,000 women by wealth quintile.",
                " Right: incremental cost per woman by quintile and perspective.",
                " Hover over a column for the values with and without misoprostol."),
              fluidRow(
                column(6, highchartOutput("DALYsPlot1",     height = "420px")),
                column(6, highchartOutput("DALYsPlot2",     height = "420px"))
              )
            ),
            tabPanel(title = "Mortality",
              div(class = "info-note",
                icon("info-circle"),
                " Left: PPH deaths averted per 100,000 women by wealth quintile.",
                " Right: incremental cost per woman by quintile and perspective.",
                " Hover over a column for the values with and without misoprostol."),
              fluidRow(
                column(6, highchartOutput("MortalityPlot1", height = "420px")),
                column(6, highchartOutput("MortalityPlot2", height = "420px"))
              )
            ),
            tabPanel(title = "Incidence",
              div(class = "info-note",
                icon("info-circle"),
                " Left: PPH cases averted per 1,000 women by wealth quintile.",
                " Right: incremental cost per woman by quintile and perspective.",
                " Hover over a column for the values with and without misoprostol."),
              fluidRow(
                column(6, highchartOutput("IncidencePlot1", height = "420px")),
                column(6, highchartOutput("IncidencePlot2", height = "420px"))
              )
            )
          )
        ))
      ),

      # ════════════════════════════════ OWSA ═══════════════════════════════════
      tabItem(tabName = "owsatab",

        # Inline settings
        fluidRow(column(12,
          bs4Card(width = NULL, class = "sa-settings", solidHeader = FALSE,
            collapsible = TRUE, collapsed = FALSE,
            title = tags$span(icon("sliders-h"), " OWSA settings"),
            fluidRow(
              column(6,
                sliderInput("max_vars", "Variables shown in tornado diagrams:",
                            min = 5, max = 50, value = 15, width = "100%")),
              column(3, br(),
                actionButton("rundsa", "Run OWSA", icon = icon("play"),
                             class = "btn-primary", width = "100%"))
            ),
            div(class = "info-note mt-2",
              icon("info-circle"),
              " Each parameter is varied from its low to high value while all",
              " others are held at base-case values. Wider bars = greater influence on outcome.")
          )
        )),

        # Tornado plots
        fluidRow(column(12,
          tabBox(width = NULL, id = "owsa_out", title = "", status = "secondary",
             type = "tabs",

            tabPanel(title = "DALYs",
              tabsetPanel(id = "dalysDSA", vertical = TRUE,
                tabPanel("DALYs averted",
                  highchartOutput("dsaDALYsPlots1",     height = "520px")),
                tabPanel("Incremental governmental costs",
                  highchartOutput("dsaDALYsPlots3",     height = "520px")),
                tabPanel("Incremental societal costs",
                  highchartOutput("dsaDALYsPlots2",     height = "520px")),
                tabPanel("ICER (governmental)",
                  highchartOutput("dsaDALYsPlots5",     height = "520px")),
                tabPanel("ICER (societal)",
                  highchartOutput("dsaDALYsPlots4",     height = "520px"))
              )
            ),
            tabPanel(title = "Mortality",
              tabsetPanel(id = "mortDSA", vertical = TRUE,
                tabPanel("Deaths averted",
                  highchartOutput("dsaMortalityPlots1", height = "520px")),
                tabPanel("Incremental governmental costs",
                  highchartOutput("dsaMortalityPlots3", height = "520px")),
                tabPanel("Incremental societal costs",
                  highchartOutput("dsaMortalityPlots2", height = "520px")),
                tabPanel("ICER (governmental)",
                  highchartOutput("dsaMortalityPlots5", height = "520px")),
                tabPanel("ICER (societal)",
                  highchartOutput("dsaMortalityPlots4", height = "520px"))
              )
            ),
            tabPanel(title = "Incidence",
              tabsetPanel(id = "incidDSA", vertical = TRUE,
                tabPanel("Cases prevented",
                  highchartOutput("dsaIncidencePlots1", height = "520px")),
                tabPanel("Incremental governmental costs",
                  highchartOutput("dsaIncidencePlots3", height = "520px")),
                tabPanel("Incremental societal costs",
                  highchartOutput("dsaIncidencePlots2", height = "520px")),
                tabPanel("ICER (governmental)",
                  highchartOutput("dsaIncidencePlots5", height = "520px")),
                tabPanel("ICER (societal)",
                  highchartOutput("dsaIncidencePlots4", height = "520px"))
              )
            )
          )
        ))
      ),

      # ════════════════════════════════ PSA ════════════════════════════════════
      tabItem(tabName = "psatab",

        # Inline settings
        fluidRow(column(12,
          bs4Card(width = NULL, class = "sa-settings", solidHeader = FALSE,
            collapsible = TRUE, collapsed = FALSE,
            title = tags$span(icon("chart-line"), " PSA settings"),
            fluidRow(
              column(4,
                sliderInput("nsims",  "Number of simulations:",
                            min = 10, max = 5000, value = 20, width = "100%")),
              column(4,
                sliderInput("maxwtp", "Maximum WTP (US$ per DALY averted):",
                            min = 500, max = 5000, value = 2000, width = "100%")),
              column(2, br(),
                actionButton("runpsa", "Run PSA", icon = icon("play"),
                             class = "btn-primary", width = "100%"))
            ),
            div(class = "info-note mt-2",
              icon("info-circle"),
              " All parameters drawn simultaneously from their probability",
              " distributions. Scatter plot shows incremental cost-outcome pairs;",
              " CEAC shows probability of cost-effectiveness at each WTP threshold.")
          )
        )),

        # PSA plots — full-width stacked for each outcome
        fluidRow(column(12,
          tabBox(width = NULL, id = "psa_out", title = "", status = "secondary",
             type = "tabs",

            tabPanel(title = "DALYs",
              fluidRow(column(12,
                bs4Card(width = NULL, title = "Cost-effectiveness scatter plot",
                        highchartOutput("psaDALYsScatterPlot", height = "400px")))),
              fluidRow(column(12,
                bs4Card(width = NULL, title = "Cost-effectiveness acceptability curve (CEAC)",
                        highchartOutput("psaDALYsCEACPlot",    height = "400px"))))
            ),
            tabPanel(title = "Mortality",
              fluidRow(column(12,
                bs4Card(width = NULL, title = "Cost-effectiveness scatter plot",
                        highchartOutput("psaMortalityScatterPlot", height = "400px")))),
              fluidRow(column(12,
                bs4Card(width = NULL, title = "Cost-effectiveness acceptability curve (CEAC)",
                        highchartOutput("psaMortalityCEACPlot",    height = "400px"))))
            ),
            tabPanel(title = "Incidence",
              fluidRow(column(12,
                bs4Card(width = NULL, title = "Cost-effectiveness scatter plot",
                        highchartOutput("psaIncidenceScatterPlot", height = "400px")))),
              fluidRow(column(12,
                bs4Card(width = NULL, title = "Cost-effectiveness acceptability curve (CEAC)",
                        highchartOutput("psaIncidenceCEACPlot",    height = "400px"))))
            )
          )
        ))
      )

    ) # tabItems
  ), # body

  footer = dashboardFooter(
    left  = tags$span(
      "Lubinga et al. (2015) | PLoS ONE | ",
      tags$a("doi:10.1371/journal.pone.0142550",
             href = "https://doi.org/10.1371/journal.pone.0142550",
             target = "_blank")),
    right = "PPH Cost-Effectiveness Model v2"
  )
)
