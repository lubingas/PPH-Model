#
# PPH Model — Server
#

shinyServer(function(input, output, session) {

  # ── Data setup ─────────────────────────────────────────────────────────────
  # Add row_id (stable key for each row) and group (the section a parameter
  # belongs to, taken from the section-header rows in the spreadsheet).

  data_raw <- local({
    d <- data.frame(row_id = seq_len(nrow(data)), as.data.frame(data), stringsAsFactors = FALSE)

    # Mapping of full section-header text → short group label
    grp_map <- c(
      "Proportion of women in each wealth quintile from DHS data" = "Wealth quintiles",
      "Probability of a health facility delivery"                  = "HF delivery probability",
      "Probability of delivery in hospital (if deliver in HF)"    = "Hospital delivery probability",
      "Probability of (non-HF delivery and) unassisted delivery"  = "Unassisted delivery",
      "Assisted by friend/relative to deliver"                    = "Friend/relative delivery",
      "Assisted by TBA at delivery"                               = "TBA delivery",
      "Achievable oxytocin coverage rates"                        = "Oxytocin coverage",
      "Coverage rates for misoprostol under misoprostol policy"   = "Misoprostol coverage (programme)",
      "Coverage rates for misoprostol under Oxytocin only policy" = "Misoprostol coverage (control)",
      "Access to EmOC (Probability of referral for EmOC)"         = "Emergency obstetric care",
      "Health resource use variables"                             = "Health resource use",
      "Age distribution of pregnancies"                           = "Age distribution"
    )

    current <- "General parameters"
    grps <- character(nrow(d))
    for (i in seq_len(nrow(d))) {
      if (is.na(d$variable[i])) {
        desc <- trimws(as.character(d$Description[i]))
        hit  <- grp_map[desc]
        if (!is.na(hit)) current <- hit
      }
      grps[i] <- current
    }
    d$group <- grps
    d
  })

  # The published inputs never change; rv$df holds the values the model uses.
  rv <- reactiveValues(df = data_raw, edit_id = NULL)

  edit_cols <- c("basecase", "low", "high", "sem", "alpha", "beta")
  same <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) <= 1e-12 * pmax(1, abs(b)))

  # TRUE for each row whose values differ from the published inputs
  edited_rows <- reactive({
    !Reduce(`&`, lapply(edit_cols, function(cl) same(rv$df[[cl]], data_raw[[cl]])))
  })

  # ── Parameters page: summary, reset, download ──────────────────────────────

  output$editSummary <- renderUI({
    n <- sum(edited_rows())
    if (n == 0) span(class = "edit-summary", icon("circle-check"), " Using the published inputs")
    else span(class = "edit-summary edited", icon("pen"),
              sprintf(" %d input%s changed from the published values", n, if (n == 1) "" else "s"))
  })

  observe(shinyjs::toggleState("reset_all", condition = any(edited_rows())))

  observeEvent(input$reset_all, {
    rv$df <- data_raw
    showNotification("All inputs reset to the published values.", type = "message")
  })

  output$download_inputs <- downloadHandler(
    filename = function() paste0("pph-model-inputs-", Sys.Date(), ".csv"),
    content  = function(file) {
      d <- rv$df[!is.na(rv$df$variable), ]
      out <- data.frame(group = d$group, variable = d$variable, description = d$Description,
                        basecase = d$basecase, low = d$low, high = d$high, se = d$sem,
                        distribution = d$dist, alpha = d$alpha, beta = d$beta,
                        source = d$Reference, edited = edited_rows()[!is.na(rv$df$variable)])
      write.csv(out, file, row.names = FALSE, na = "")
    })

  # ── Parameters page: table ─────────────────────────────────────────────────

  output$dataTable <- renderReactable({
    keep <- !is.na(rv$df$variable)
    d <- rv$df[keep, ]
    d$edited <- edited_rows()[keep]
    d$dist_lbl <- dist_label(d$dist, d$alpha, d$beta, d$sem)
    grp <- if (is.null(input$data_group)) "ALL" else input$data_group
    if (grp != "ALL") d <- d[d$group == grp, ]

    d <- d[, c("row_id", "group", "Description", "variable", "basecase", "low", "high",
               "dist_lbl", "Reference", "edited")]
    num_col <- function(name) colDef(name = name, align = "right", minWidth = 80,
                                     cell = function(value) fmt_val(value),
                                     style = list(fontVariantNumeric = "tabular-nums"))

    reactable(
      d,
      groupBy  = if (grp == "ALL") "group" else NULL,
      defaultExpanded = TRUE,
      searchable  = TRUE,
      highlight   = TRUE,
      compact     = TRUE,
      pagination  = FALSE,
      height      = 640,
      language    = reactableLang(searchPlaceholder = "Search parameters, codes or sources"),
      rowClass    = function(index) if (isTRUE(d$edited[index])) "row-edited" else NULL,
      columns = list(
        row_id      = colDef(show = FALSE),
        variable    = colDef(show = FALSE),
        group       = colDef(name = "Section", minWidth = 160, show = grp == "ALL"),
        Description = colDef(name = "Parameter", minWidth = 300,
          cell = function(value, index) div(div(value), div(class = "param-code", d$variable[index]))),
        basecase    = num_col("Base case"),
        low         = num_col("Low"),
        high        = num_col("High"),
        dist_lbl    = colDef(name = "Probabilistic analysis", minWidth = 170,
                             style = list(color = "#52514e")),
        Reference   = colDef(name = "Source", minWidth = 200,
                             cell = function(value) if (is.na(value)) "—" else value,
                             style = list(color = "#52514e", fontSize = ".85em")),
        edited      = colDef(name = "", width = 78, sortable = FALSE,
                             cell = function(value) if (isTRUE(value)) span(class = "chip-edited", "Edited") else "")
      ),
      # Click any parameter row to edit it (group header rows are ignored)
      onClick = JS("function(rowInfo) {
        if (!rowInfo) return;
        if (rowInfo.aggregated) { rowInfo.toggleRowExpanded(); return; }
        if (!window.Shiny) return;
        Shiny.setInputValue('edit_row', rowInfo.original.row_id, { priority: 'event' });
      }")
    )
  })

  # ── Parameters page: edit dialog ───────────────────────────────────────────

  observeEvent(input$edit_row, {
    pos <- which(rv$df$row_id == input$edit_row)
    if (!length(pos) || is.na(rv$df$variable[pos])) return()
    rv$edit_id <- input$edit_row
    showModal(param_editor_modal(rv$df[pos, ], edited = edited_rows()[pos]))
  })

  edit_row <- reactive({ req(rv$edit_id); rv$df[rv$df$row_id == rv$edit_id, ] })

  proposed <- reactive({
    r <- edit_row(); u <- dist_uses(r$dist)
    list(dist = r$dist, base = input$pe_base,
         low  = if (u$range) input$pe_low  else NA,
         high = if (u$range) input$pe_high else NA,
         se   = if (u$se)    input$pe_se   else r$sem)
  })

  edit_errors <- reactive({
    p <- proposed(); req(!is.null(p$base))
    check_param(p$dist, p$base, p$low, p$high, p$se)
  })

  output$pe_errors <- renderUI({
    e <- edit_errors()
    if (length(e)) div(class = "pe-errors", icon("triangle-exclamation"), " ",
                       paste(e, collapse = " "))
  })

  observe(shinyjs::toggleState("pe_save", condition = length(edit_errors()) == 0))

  output$pe_derived <- renderUI({
    p <- proposed()
    if (p$dist != "beta" || length(edit_errors())) return(NULL)
    ab <- beta_mom(p$base, p$se)
    div(class = "pe-derived", sprintf("Gives α = %s and β = %s", fmt_val(ab[["alpha"]]), fmt_val(ab[["beta"]])))
  })

  output$pe_preview <- renderHighchart({
    p <- proposed()
    validate(need(length(edit_errors()) == 0, "Fix the values above to see the draws."))
    param_preview(p$dist, p$base, p$low, p$high, p$se)
  })

  observeEvent(input$pe_save, {
    req(length(edit_errors()) == 0)
    p <- proposed(); pos <- which(rv$df$row_id == rv$edit_id)
    rv$df$basecase[pos] <- p$base
    if (dist_uses(p$dist)$range) { rv$df$low[pos] <- p$low; rv$df$high[pos] <- p$high }
    if (dist_uses(p$dist)$se)    rv$df$sem[pos] <- p$se
    if (p$dist == "beta") {
      ab <- beta_mom(p$base, p$se)
      rv$df$alpha[pos] <- ab[["alpha"]]; rv$df$beta[pos] <- ab[["beta"]]
    }
    removeModal()
    showNotification(sprintf("Saved %s. Results update automatically.", rv$df$variable[pos]), type = "message")
  })

  observeEvent(input$pe_reset, {
    pos <- which(rv$df$row_id == rv$edit_id)
    rv$df[pos, edit_cols] <- data_raw[pos, edit_cols]
    removeModal()
    showNotification(sprintf("%s reset to the published value.", rv$df$variable[pos]), type = "message")
  })

  # Note on the results pages when they use edited inputs
  output$editedNote <- renderUI({
    n <- sum(edited_rows())
    if (n == 0) return(NULL)
    div(class = "info-note edited-note", icon("pen"),
        sprintf(" These results use %d edited input%s. ", n, if (n == 1) "" else "s"),
        actionLink("goto_params", "Review or reset them on the Parameters page."))
  })
  observeEvent(input$goto_params, updateTabItems(session, "nav", "datatab"))

  # ── Base-case model ─────────────────────────────────────────────────────────

  ModelRun <- reactive({
    baseInputs <- makeBaseCaseInputs(rv$df)
    withProgressShiny(message = "Running base-case model …", value = 0, {
      RunModel(baseparms      = baseInputs$baseinputs,
               basetransitions = baseInputs$basetransitions,
               makePlots       = TRUE)
    })
  })

  # ── Summary metric cards (update when outcome tab changes) ─────────────────

  output$summaryBoxes <- renderUI({
    req(ModelRun())

    sel <- if (is.null(input$outcomes)) "DALYs" else input$outcomes

    tab <- switch(sel,
      DALYs     = ModelRun()$DALYS,
      Mortality = ModelRun()$Mortality,
      Incidence = ModelRun()$Incidence,
      ModelRun()$DALYS)

    col_names <- names(tab)
    icer_gov_col <- grep("iCERGov", col_names, value = TRUE)[1]
    icer_soc_col <- grep("iCERSoc", col_names, value = TRUE)[1]

    delta    <- tab["Average", "deltaOutcome"]
    icer_gov <- if (!is.na(icer_gov_col)) tab["Average", icer_gov_col] else NA_real_
    icer_soc <- if (!is.na(icer_soc_col)) tab["Average", icer_soc_col] else NA_real_

    delta_lbl <- switch(sel,
      DALYs     = "DALYs averted per woman",
      Mortality = "Deaths averted per woman",
      Incidence = "PPH cases averted per woman",
      "Outcome averted per woman")

    icer_unit <- switch(sel,
      DALYs     = "per DALY averted",
      Mortality = "per death averted",
      Incidence = "per PPH case averted",
      "per unit")

    fmt_delta <- function(x) {
      if (is.na(x) || !is.finite(x)) return("\u2014")
      if (sel == "DALYs") formatC(round(x, 4), format = "f", digits = 4)
      else                sprintf("%.4f%%", x * 100)
    }
    fmt_icer <- function(x) {
      if (is.na(x) || !is.finite(x)) return("\u2014")
      paste0("US$", formatC(round(x), format = "d", big.mark = ","))
    }

    mk <- function(val, lbl, colour) {
      div(class = "metric-card", style = paste0("--mc:", colour),
        div(class = "mc-val", val),
        div(class = "mc-lbl", lbl))
    }

    fluidRow(
      column(4, mk(fmt_delta(delta),    delta_lbl,                              "#2e7d32")),
      column(4, mk(fmt_icer(icer_gov),  paste("ICER — Governmental", icer_unit), chart_cols$gov)),
      column(4, mk(fmt_icer(icer_soc),  paste("ICER — Societal",     icer_unit), chart_cols$soc))
    )
  })

  # ── Base-case plots ─────────────────────────────────────────────────────────

  output$IncidencePlot1 <- renderHighchart({
    outcomesPlot(outcome = "Incidence", whichTab = ModelRun()$Incidence)[[1]] })
  output$IncidencePlot2 <- renderHighchart({
    outcomesPlot(outcome = "Incidence", whichTab = ModelRun()$Incidence)[[2]] })
  output$MortalityPlot1 <- renderHighchart({
    outcomesPlot(outcome = "Mortality", whichTab = ModelRun()$Mortality)[[1]] })
  output$MortalityPlot2 <- renderHighchart({
    outcomesPlot(outcome = "Mortality", whichTab = ModelRun()$Mortality)[[2]] })
  output$DALYsPlot1 <- renderHighchart({
    outcomesPlot(outcome = "DALYs", whichTab = ModelRun()$DALYS)[[1]] })
  output$DALYsPlot2 <- renderHighchart({
    outcomesPlot(outcome = "DALYs", whichTab = ModelRun()$DALYS)[[2]] })

  # ── OWSA ────────────────────────────────────────────────────────────────────

  DSARun <- reactive({
    dsaInputs <- makeOWSAInputs(rv$df)
    withProgressShiny(message = "Running OWSA …", detail = "Starting …", value = 0, {
      owsa(model            = ModelRun(),
           low_base         = dsaInputs$usa.low,
           low_transitions  = dsaInputs$dir_lowinputs,
           high_base        = dsaInputs$usa.high,
           high_transitions = dsaInputs$dir_highinputs,
           max_vars         = input$max_vars)
    })
  }) |> bindEvent(input$rundsa)

  for (outcome in c("Incidence", "Mortality", "DALYs")) {
    for (i in 1:5) {
      local({
        o <- outcome; j <- i
        id <- paste0("dsa", o, "Plots", j)
        output[[id]] <- renderHighchart({ DSARun()$Plots[[o]][[j]] })
      })
    }
  }

  # ── PSA ─────────────────────────────────────────────────────────────────────

  PSARun <- reactive({
    psaInputs <- makeSims(nsims = input$nsims, data = rv$df)
    withProgressShiny(message = "Running PSA …", detail = "Starting …", value = 0, {
      RunPSA(model   = ModelRun(),
             psalist = psaInputs,
             wtp     = input$maxwtp,
             by      = input$maxwtp / 100)   # 101 WTP points, so the curve keeps its shape at any range
    })
  }) |> bindEvent(input$runpsa)

  output$psaIncidenceScatterPlot <- renderHighchart({ PSARun()$Plots$scatterPlots$Incidence })
  output$psaMortalityScatterPlot <- renderHighchart({ PSARun()$Plots$scatterPlots$Mortality })
  output$psaDALYsScatterPlot     <- renderHighchart({ PSARun()$Plots$scatterPlots$DALYs })
  output$psaIncidenceCEACPlot    <- renderHighchart({ PSARun()$Plots$ceacPlots$Incidence })
  output$psaMortalityCEACPlot    <- renderHighchart({ PSARun()$Plots$ceacPlots$Mortality })
  output$psaDALYsCEACPlot        <- renderHighchart({ PSARun()$Plots$ceacPlots$DALYs })

  # ── Threshold analysis: odds ratio of a health facility delivery ────────────

  ThresholdRun <- reactive({
    req(isTruthy(input$thr_maxor), input$thr_maxor > 1)
    baseInputs <- makeBaseCaseInputs(rv$df)
    # 200 odds ratios, evenly spaced on a log scale from 0.01 to the largest, plus the base case (1)
    ors <- sort(unique(c(exp(seq(log(0.01), log(input$thr_maxor), length.out = 200)), 1)))
    withProgressShiny(message = "Running threshold analysis …", value = 0, {
      list(tab    = runORThreshold(baseInputs$baseinputs, baseInputs$basetransitions, ors),
           # where the ICERs level off as the odds ratio grows (every woman delivers in a facility)
           limit  = runORThreshold(baseInputs$baseinputs, baseInputs$basetransitions, 1e6),
           inputs = baseInputs)
    })
  }) |> bindEvent(input$runthr)

  # the WTP line and threshold are for DALYs only
  thrWTP <- reactive({
    if (input$thr_outcome == "DALYs" && isTruthy(input$thr_wtp)) input$thr_wtp else NULL
  })

  output$thrPlot <- renderHighchart({
    run <- ThresholdRun()
    ThresholdPlot(tab   = run$tab[run$tab$outcome == input$thr_outcome, ],
                  named = input$thr_outcome,
                  wtp   = thrWTP(),
                  log_x = isTRUE(input$thr_log))
  })

  # where each ICER crosses zero (and the WTP), found by rerunning the model between grid points
  output$thrSummary <- renderUI({
    run <- ThresholdRun()
    o   <- input$thr_outcome
    tab <- run$tab[run$tab$outcome == o, ]
    lim <- run$limit[run$limit$outcome == o, ]
    wtp <- thrWTP()

    unit  <- switch(o, DALYs = "DALY averted", Mortality = "death averted", Incidence = "PPH case averted")
    money <- function(x) paste0(if (x < 0) "−$" else "$", format(round(abs(x)), big.mark = ","))
    orFmt <- function(x) formatC(x, format = "f", digits = 2)
    maxOR <- format(signif(max(tab$OR), 3))

    sentence <- function(p, label) {
      icer  <- tab[[p]]
      cross <- function(v) findORCrossing(run$inputs$baseinputs, run$inputs$basetransitions, tab, o, p, v)

      x0 <- cross(0)
      saving <- if (!is.na(x0)) {
        paste0("saves money when the odds ratio is ", if (icer[1] < 0) "below " else "above ", orFmt(x0))
      } else if (all(icer < 0)) {
        "saves money at every odds ratio run"
      } else {
        paste0("doesn't save money at any odds ratio from 0.01 to ", maxOR)
      }

      overWTP <- if (is.null(wtp)) "" else {
        xw <- cross(wtp)
        if (!is.na(xw)) {
          paste0(", and costs more than ", money(wtp), " per ", unit, " when the odds ratio is ",
                 if (tail(icer, 1) > wtp) "above " else "below ", orFmt(xw))
        } else if (all(icer < wtp)) {
          paste0(", and costs less than ", money(wtp), " per ", unit, " at every odds ratio up to ", maxOR)
        } else {
          paste0(", and costs more than ", money(wtp), " per ", unit, " at every odds ratio run")
        }
      }

      tags$li(tags$strong(label, .noWS = "after"),
              paste0(": the programme ", saving, overWTP, ". As the odds ratio grows, the cost per ",
                     unit, " levels off at ", money(lim[[p]]), "."))
    }

    tags$ul(class = "mt-3",
            sentence("iCERGov", "Governmental perspective"),
            sentence("iCERSoc", "Societal perspective"))
  })

  # ── Tables (used by TabulateOutcomes) ──────────────────────────────────────

  output$IncidenceTable <- render_gt({
    TabulateOutcomes(data = ModelRun()$Incidence, decimals = 4, scale = 1) })
  output$MortalityTable <- render_gt({
    TabulateOutcomes(data = ModelRun()$Mortality, decimals = 4, scale = 1) })
  output$DALYsTable <- render_gt({
    TabulateOutcomes(data = ModelRun()$DALYS,     decimals = 4, scale = 1) })

})
