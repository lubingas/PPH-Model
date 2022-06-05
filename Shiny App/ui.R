#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(shiny)
#library(shinythemes)
#library(plotly)

# # Define UI 
shinyUI(tagList(useShinyjs(),  # Include shinyjs
                
                navbarPage(title = "PPH Health Economic Model", position = "static-top", 
                           theme = bs_theme(version = 5, bootswatch = "cerulean", 
                                            base_font = "Lato",
                                            code_font = font_google("JetBrains Mono")),
                           tabPanel("About",
                                    tabsetPanel(
                                      tabPanel("Description", br(), br(),
                                               "Potential Cost-Effectiveness of Prenatal Distribution of Misoprostol for Prevention of Postpartum Hemorrhage in Uganda",
                                               br(), br(),
                                               "In settings where home birth rates are high, prenatal distribution of misoprostol could 
                                 increase access to uterotonics during the third stage of labor to prevent postpartum hemorrhage (PPH). 
                                 This decision analytic model is designed to utilize data from the demographic health surveys to track the likely delivery pathway of 
                                 a woman. Depending on her social economic status, a women may deliver either in a health facility or at home, 
                                 with or without assistance of a friend or a traditional birth attendant. She may or may not experience post-partum hemorrhage.
                                 The model applied differential uterotonic coverage probabilities depending on the setting of delivery that impact the likelihood of PPH.
                                 The model then tracks outcomes of potential misuse of misoprostol, PPH, access to emergency obstetric care (EmOC) and death.",
                                               br(), br(),
                                               "The model then estimates costs and outcomes under a prenatal distribution policy versus a no misoprostol policy, assuming intervention 
                                 would increase the coverage rates of uterotonics."),
                                      tabPanel("Data",
                                               sidebarPanel(
                                                 fileInput(inputId = "file", label = "Upload data file"),
                                                 h6(helpText("Upload file with data to run model; please ensure data structure conforms to previous file. I will attempt to process the data. If data are not ready for model, it will not run"))
                                               ),
                                               mainPanel(
                                                 DT::dataTableOutput(outputId = "dataTable")
                                               )
                                      )
                                    )
                           ),
                           tabPanel("Basecase",
                                    sidebarLayout(
                                      sidebarPanel("User defined parameters",
                                                   div(id = "inputparms",
                                                       tabsetPanel(
                                                         tabPanel("Wealth quintiles",
                                                                  helpText(h6("Enter number of women in each wealth quintile from DHS survey")),
                                                                  
                                                         ),
                                                         tabPanel("Age distribution",
                                                                  helpText(h6("Enter age distribution of women")),
                                                                  
                                                         ),
                                                         tabPanel("Misoprostol access",
                                                                  helpText("Misoprostol coverage probabilities"),
                                                                  
                                                         ),
                                                         tabPanel("Misoprostol cost",
                                                                  helpText("Cost of misoprostol delivery program"),
                                                                  
                                                         )
                                                       )
                                                   ),
                                                   actionButton("runmodel", label = "Run Model"),
                                                   actionButton("resetinputs", label = "Reset Inputs"),
                                                   hr()
                                      ),
                                      mainPanel("Results/Output",
                                                #verbatimTextOutput("contents"),
                                                tabsetPanel(
                                                  tabPanel("Incidence",
                                                           br(),
                                                           gt::gt_output(outputId = "IncidenceTable"),
                                                           htmlOutput(outputId = "IncidencePlots")
                                                  ),
                                                  tabPanel("Mortality",
                                                           br(),
                                                           gt::gt_output(outputId = "MortalityTable"),
                                                           htmlOutput(outputId = "MortalityPlots")
                                                  ),
                                                  tabPanel("DALYs",
                                                           br(),
                                                           gt::gt_output(outputId = "DALYsTable"),
                                                           htmlOutput(outputId = "DALYsPlots")
                                                  )
                                                )
                                      ) 
                                    )
                           ),
                           tabPanel("Sensitivity analyses",
                                    tabsetPanel(
                                      tabPanel("Deterministic",
                                               sidebarLayout(
                                                 sidebarPanel("Settings",
                                                              sliderInput("max_vars", label = "How may variables in Tornado Plots?", min = 10, max = 25, value = 15),
                                                              actionButton("rundsa", label = "Run DSA")
                                                 ),
                                                 mainPanel("Results",
                                                           #verbatimTextOutput("dsaProgress"),
                                                           tabsetPanel(
                                                             tabPanel("Incidence",
                                                                      htmlOutput(outputId = "dsaIncidencePlots")
                                                             ),
                                                             tabPanel("Mortality",
                                                                      htmlOutput(outputId = "dsaMortalityPlots")
                                                             ),
                                                             tabPanel("DALYs",
                                                                      htmlOutput(outputId = "dsaDALYsPlots")
                                                             )
                                                           )
                                                 )
                                               )
                                      ),
                                      tabPanel("Probabilitic",
                                               sidebarLayout(
                                                 sidebarPanel("Settings",
                                                              sliderInput("nsims", label = "How many simulations", min = 10, max = 5000, value = 20),
                                                              sliderInput("maxwtp", label = "Maximum willingness-to-pay", min = 1000, max = 5000, value = 2000),
                                                              actionButton("runpsa", label = "Run PSA")
                                                 ),
                                                 mainPanel("Results",
                                                           #verbatimTextOutput("psaProgress"),
                                                           tabsetPanel(
                                                             tabPanel("Incidence",
                                                                      htmlOutput(outputId = "psaIncidencePlots")
                                                             ),
                                                             tabPanel("Mortality",
                                                                      htmlOutput(outputId = "psaMortalityPlots")
                                                             ),
                                                             tabPanel("DALYs",
                                                                      htmlOutput(outputId = "psaDALYsPlots")
                                                             )
                                                           ))
                                               )
                                      )
                                    )
                           )
                )
                
)
)
