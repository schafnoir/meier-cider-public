####  Meier Cider Public version 1.0 ####

### Define UI for Meier Cider Shiny application
navbarPage(title = "Meier Cider", selected = "Summary",
           ###  Turn on shinyjs
           shinyjs::useShinyjs(),
           
           
           
           ###  Summary Tab ###############################################################
           tabPanel("Summary",
                    sidebarLayout(
                      sidebarPanel(width = 3,
                                   # Meier Cider logo
                                   div(img(src = "meierCiderLogo_small.png"), style="text-align: center;"),
                                   fluidRow(p()),
                                   
                                   # yearSelect drop-down
                                   uiOutput(outputId = "summaryYearSelect"),
                                   
                                   # actionButton to display summary table for selected year
                                   actionButton(inputId = "summaryButton", "Display Data", class = "btn-primary"),
                                   
                                   # summary process data for selected year
                                   fluidRow(p("")),
                                   fluidRow(p("")),
                                   conditionalPanel(
                                     condition = "input.summaryButton != 0",
                                     
                                     h4("Process Data"),
                                     tableOutput(outputId = "summaryProcessTable"),
                                     br(),
                                     # Apple collection photo
                                     div(img(src = "appleCollection.jpg", width="100%"), style="text-align: center;")
                                     
                                   )
                                   
                      ), #  End sidebarPanel
                      
                      mainPanel(width = 9,
                                # Render summaryMainPanel output from server.R
                                uiOutput(outputId = "summaryMainPanel")
                      ) # End mainPanel
                    ) # End sidebarLayout
             
           ), #  End Summary tabPanel
           
           
           
           ###  Fermentation Tab ##########################################################
           tabPanel("Fermentation",
                    sidebarLayout(
                      sidebarPanel(width = 3,
                        # Meier Cider logo
                        div(img(src = "meierCiderLogo_small.png"), style="text-align: center;"),
                        fluidRow(p()),

                        # yearSelect drop-down
                        uiOutput(outputId = "fermentYearSelect"),
                        
                        # fermentBatchSelect drop-down
                        uiOutput(outputId = "fermentBatchSelect"),
                        
                        # actionButton to create plots and display fermentation table
                        actionButton(inputId = "plotButton", "Display Data", class = "btn-primary"),
                        
                        # summary table output
                        fluidRow(p("")),
                        fluidRow(p("")),
                        conditionalPanel(
                          condition = "input.plotButton != 0",
                          h4("Batch Summary Data"),
                          tableOutput(outputId = "summaryTable")
                        )
                        
                        
                      ), #  End sidebarPanel
                      
                      mainPanel(width = 9,
                        # mainPanel output from server.R
                        uiOutput(outputId = "fermentMainPanel")
                      
                      ) # End mainPanel
                    ) # End sidebarLayout
           
           ), #  End Fermentation tabPanel
           
           
           
           ###  Sulfite Calculator Tab ####################################################
           tabPanel("Sulfites",
                    sidebarLayout(
                      sidebarPanel(width = 3,
                        # Meier Cider logo
                        div(img(src = "meierCiderLogo_small.png"), style="text-align: center;"),
                        fluidRow(p()),
                        
                        ##  Must pH and Sulfite concentration
                        wellPanel(
                          span(h4("Must pH and Sulfite Concentration"), style="color:#F15A29"),
                          
                          # pH input
                          sliderInput(inputId = "pHChoice", label = "Must pH:", min = 3, max = 3.8, value = 3.4, step = 0.01),
                          
                          # SO2 output
                          fluidRow(
                            column(width=6, span(h5("SO2 (ppm):"), style="color:#F15A29")),
                            column(width=5, verbatimTextOutput("so2Output"))
                            ),
                          
                          # KMS output (g L-1)
                          fluidRow(
                            column(width=6, span(h5("KMS (g L-1):"), style="color:#F15A29")),
                            column(width=5, verbatimTextOutput("kmsOutput"))
                            ),
                          helpText("No SO2 needed if pH < 3"),
                          helpText("Add acid if pH > 3.8")
                          
                          ), #  End wellPanel
                        
                        
                        ##  Sulfite addition calculator
                        wellPanel(
                          span(h4("Sulfite Addition Calculator"), style="color:#F15A29"),
                          
                          ##  Volume inputs
                          # Quantity
                          numericInput(inputId = "quantity", label = "Cider Volume:", min = 1, max = 1000, step = 0.1, value = 5),
                          
                          # Units
                          radioButtons(inputId = "units", label = "Volume Units:", choiceNames=c('Gallons', 'Liters'),
                                     choiceValues=c('gallons', 'liters'), inline = TRUE),
                          
                          
                          ##  SO2 input
                          numericInput(inputId = "targetSO2", label = "Target SO2 (ppm):", min = 1, max = 200, step = 1, value = 50),
                          
                          
                          ##  Button to calculate output
                          actionButton(inputId = "so2Button", "Calculate", class = "btn-primary"),
                          fluidRow(p()),
                          
                          ##  Calculated SO2 outputs
                          #   KMS grams
                          fluidRow(
                            column(width=6, span(h5("KMS needed (g):"), style="color:#F15A29")),
                            column(width=5, verbatimTextOutput("kmsGrams"))
                          ),
                          
                          #   Campden tablet number
                          fluidRow(
                            column(width=6, span(h5("Campden Num:"), style="color:#F15A29")),
                            column(width=5, verbatimTextOutput("campdenNum"))
                          )
                          
                        ) # End wellPanel
                      ), # End sidebarPanel
                      
                      mainPanel(width = 9,
                        wellPanel(
                        fluidRow(
                          column(width=5, div(h4("The Sulfite and pH Curve"), style="color:#F15A29")),
                          column(width=7, div(em("Data from Craft Cider Making, see References"), style="text-align: right; color:#808080"))
                          )
                        ),
                        
                        fluidRow(
                          column(width=1),
                          column(width=10, plotlyOutput("pHPlot")),
                          column(width = 1)
                                 )
                      ) # End mainPanel
                      
                    ) # End sidebarLayout
             
           ), # End Sulfites tabPanel
           
           
           
           ###  Bottling Tab ##############################################################
           tabPanel("Bottling",
                    sidebarLayout(
                      sidebarPanel(width = 3,
                        # Meier Cider logo
                        div(img(src = "meierCiderLogo_small.png"), style="text-align: center;"),
                        fluidRow(p()),
                        
                        ##  Fermentable Sugar and Carbonation
                        wellPanel(
                          span(h4("Fermentable Sugar and Potential Carbonation"), style="color:#F15A29"),
                          
                          # Fermentable Sugar input
                          sliderInput(inputId = "sugarChoice", label = "Fermentable Sugar (g L-1):", 
                                      min = 0, max = 25, value = 8.5, step = 0.1),
                          
                          # Dissolved CO2 output
                          fluidRow(
                            column(width=7, span(h5("Dissolved CO2 (g L-1):"), style="color:#F15A29")),
                            column(width=5, verbatimTextOutput("dissolvedCO2"))
                          ), # End Dissolved CO2 fluidRow
                          
                          # SG Drop output
                          fluidRow(
                            column(width=7, span(h5("SG Drop:"), style="color:#F15A29")),
                            column(width=5, verbatimTextOutput("sgDrop"))
                          ), # End SG Drop fluidRow
                          
                          # DAP output
                          fluidRow(
                            column(width=7, span(h5("DAP (ppm):"), style="color:#F15A29")),
                            column(width=5, verbatimTextOutput("dap"))
                          ), # End DAP fluidRow
                          
                          # Carb Style output
                          fluidRow(
                            column(width=7, span(h5("Style:"), style="color:#F15A29")),
                            column(width=5, verbatimTextOutput("carbStyle"))
                          )
                        ), #  End first wellPanel
                          
                          
                        ##  Bottling calculator
                        wellPanel(
                            span(h4("Bottling Calculator"), style="color:#F15A29"),
                            
                            #  Cider and Bottle Volume inputs
                            numericInput(inputId = "sgTargetDrop", label = "Target SG Drop (points):",
                                         min = 0, max = 0.05, step = 0.001, value = 0.004),
                            
                            fluidRow(
                              column(width = 6,
                                     numericInput(inputId = "ciderVolume", label = "Cider Volume:", min = 1, 
                                                  max = 1000, step = 0.1, value = 5)),
                              column(width = 6,
                                     numericInput(inputId = "bottleVolume", label = "Bottle Volume:", min = 1,
                                                  max = 1500, step = 1, value = 750))
                            ),
                            
                            # Units inputs
                            fluidRow(
                              column(width = 6,
                                     radioButtons(inputId = "ciderVolUnits", label = "Cider Units:",
                                                  choiceNames=c('Gal', 'Liter'), choiceValues=c('gallons', 'liters'), inline = TRUE)),
                              column(width = 6,
                                     radioButtons(inputId = "bottleVolUnits", label = "Bottle Units:",
                                         choiceNames = c('mL', 'oz'), choiceValues = c('mL', 'oz'), inline = TRUE))
                            ),
                            
                            
                            ##  Button to calculate output
                            actionButton(inputId = "carbButton", "Calculate", class = "btn-primary"),
                            fluidRow(p()),
                            
                            ##  Calculated priming outputs
                            #   Sugar grams
                            fluidRow(
                              column(width=7, span(h5("Sugar needed (g):"), style="color:#F15A29")),
                              column(width=5, verbatimTextOutput("sugarGrams"))
                            ),
                            
                            #   DAP grams
                            fluidRow(
                              column(width=7, span(h5("DAP needed (g):"), style="color:#F15A29")),
                              column(width=5, verbatimTextOutput("dapGrams"))
                            ),
                            
                            #   Bottle number
                            fluidRow(
                              column(width=7, span(h5("Bottles needed:"), style="color:#F15A29")),
                              column(width=5, verbatimTextOutput("bottleNumber"))
                            )
                            
                          ) # End second wellPanel
                          
                        ), # End sidebarPanel
                      
                      mainPanel(width = 9,
                        wellPanel(
                          fluidRow(
                            column(width=5, div(h4("Cider Prise de Mousse"), style="color:#F15A29")),
                            column(width=7, div(em("Table from The New Cider Maker's Handbook, see References"), 
                                                style="text-align: right; color:#808080"))
                          )
                        ),
                        
                        # Render NCMH carbonation table
                        fluidRow(
                          column(width=1),
                          column(width=10, tableOutput("carbTableNCMH")),
                          column(width = 1)
                        ),
                        
                        # Render carbonation plotly object
                        fluidRow(
                          column(width=1),
                          column(width=10, plotlyOutput("carbPlot")),
                          column(width = 1)
                        )
                      ) # End mainPanel
                    ) # End sidebarLayout
           ), # End Bottling tabPanel
           
           
           
           ###  Trees Map Tab #############################################################
           tabPanel("Trees Map",
                    
                    ### Display rendered UI
                    uiOutput(outputId = "treesUI")
                    
                    
           ), #  End Trees Map tabPanel
           
           
           
           ###  References Tab ############################################################
           tabPanel("References",
             sidebarLayout(
               sidebarPanel(width = 3,
                 # Meier Cider logo
                 div(img(src = "meierCiderLogo_small.png"), style="text-align: center;")
               ), # End sidebarPanel
               
               mainPanel(width = 9,
                 wellPanel(div(h4("Reference Material"), style="color:#F15A29")),
                 br(),
                 fluidRow(
                   column(width = 1),
                   column(width = 8, 
                          div(class = "hangingindent", style = "padding-left:25px; text-indent:-25px;",
                          p("Jolicoeur, Claude (2013),", em("The New Cider Maker's Handbook, A Comprehensive Guide for Craft Producers."), "White River Junction, Vermont: Chelsea Green Publishing, 337 pages."),
                          br(),
                          p("Lea, Andrew (2015),", em("Craft Cider Making, Third Edition."), "Wiltshire, United Kingdom: The Crowood Press Ltd, 128 pages.")
                   ) # End div
                          )
                 )
               ) # End mainPanel
             ) # End sidebarLayout
           ), # End References tabPanel
           
           
  theme = shinytheme("united")
) # End navbarPage