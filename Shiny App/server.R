#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(shiny)

# place all files in the same working directory and change to this directory 
# setwd("../../PPH Model/Shiny App/")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  ### first deal with data file
  dF <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()}
    read_excel(path = "pph_data_input.xlsx", sheet = 1, col_names = TRUE)
  })
  
  ### now to render it
  output$dataTable <- DT::renderDataTable({
    # use existing input data, thats preloaded
    if(is.null(dF())) {return(data)}
    dF()
  })
  
  ### RUN BASE CASE ANALYSIS
  ModelRun <- reactive({

    # replace the values from the spreadsheet with these before running model
    withProgress(message = 'Running basecase model', value = 0, {
      
    model <- RunModel(baseparms = baseinputs, 
                      basetransitions = basetransitions,
                      makePlots = TRUE)
    })
    
  }) |>
    bindEvent(input$runmodel)
  
  ### RUN ONE WAY SENSITIVITY ANALYSIS
  DSARun <- reactive({
    
    # # Create a Progress object
    # progress <- shiny::Progress$new()
    # 
    # progress$set(message = "Running OWSA", value = 0)
    # 
    # # Make sure it closes when we exit this reactive, even if there's an error
    # on.exit(progress$close())
    
    # run OWSA model
    withProgressShiny(message = "OWSA in progress",
                      detail = "Starting ...",
                      value = 0, {
                        dsa <- owsa(model = ModelRun(),
                                    low_base = usa.low, low_transitions = dir_lowinputs,
                                    high_base = usa.high, high_transitions = dir_highinputs,
                                    max_vars = input$max_vars)
                      })
    
  }) |>
    bindEvent(input$rundsa)
  
  ### RUN PROBABILISTIC SENSITIVITY ANALYSES
  PSARun <- reactive({
    
    # # Create a Progress object
    # progress <- shiny::Progress$new()
    # 
    # progress$set(message = "Running PSA", value = 0)
    # 
    # # Make sure it closes when we exit this reactive, even if there's an error
    # on.exit(progress$close())

    ## PSA
    withProgressShiny(message = "PSA in progress",
                      detail = "Starting ...",
                      value = 0, {
                        psa <- RunPSA(model = ModelRun(), nsims = input$nsims, wtp = input$maxwtp, by = 200)
                      })
    
  }) %>%
    bindEvent(input$runpsa)
  
  
  
  ### GENERATE TABLES FROM BASE CASE ANALYSIS
  output$IncidenceTable <- gt::render_gt({
    TabulateOutcomes(data = ModelRun()$Incidence, decimals = 4, scale = 1)
    
  })

  output$MortalityTable <- gt::render_gt({
    TabulateOutcomes(data = ModelRun()$Mortality, decimals = 4, scale = 1)
    
  })

  output$DALYsTable <- gt::render_gt({
    TabulateOutcomes(data = ModelRun()$DALYS, decimals = 4, scale = 1)
    
  })
  
  
  ### GENERATE PLOTS FROM BASE CASE ANALYSIS
  output$IncidencePlots <- renderUI({
    outcomesPlot(outcome = "Incidence", whichTab = ModelRun()$Incidence)

   })

   output$MortalityPlots <- renderUI({
     outcomesPlot(outcome = "Mortality", whichTab = ModelRun()$Mortality)

   })

   output$DALYsPlots <- renderUI({
     outcomesPlot(outcome = "DALYs", whichTab = ModelRun()$DALYS)
  
   })
   
   ### GENERATE PLOTS FROM OWSA
   output$dsaIncidencePlots <- renderUI({
     hw_grid(DSARun()$Plots$Incidence)
     
   })
   
   output$dsaMortalityPlots <- renderUI({
     hw_grid(DSARun()$Plots$Mortality)
     
   })
   
   output$dsaDALYsPlots <- renderUI({
     hw_grid(DSARun()$Plots$DALYs)
     
   })

   
   ### GENERATE PLOTS FROM PSA
   output$psaIncidencePlots <- renderUI({
     hw_grid(list(PSARun()$Plots$scatterPlots$Incidence, PSARun()$Plots$ceacPlots$Incidence))
     
   })
   
   output$psaMortalityPlots <- renderUI({
     hw_grid(list(PSARun()$Plots$scatterPlots$Mortality, PSARun()$Plots$ceacPlots$Mortality))
     
   })
   
   output$psaDALYsPlots <- renderUI({
     hw_grid(list(PSARun()$Plots$scatterPlots$DALYs, PSARun()$Plots$ceacPlots$DALYs))
     
   })
   
      

})
  

