# saved as luc_qc.R

tabItem(tabName = "luc_qc", align = "center",
        
        shinyjs::useShinyjs(),
        
        shiny::fluidRow(style="width:85%",
                        
                        # Put content here
                        shiny::column(width=8, offset=2,
                                      box(width=12, title = "Select Plate for Quality Check", solidHeader = TRUE,collapsible = FALSE, status="primary",
                                          
                                          shiny::tags$h4("Select the Plate for QC"),
                                          uiOutput("luc_qc_select_plate")
                                          
                                          
                                      )
                                      )
                        
                        
        ),
        
        # Plots
        
        fluidRow(style="width:85%",
                 box(width=12, title = "Overview FLUC", solidHeader = TRUE, collapsible = TRUE, status="primary",
                     plotOutput("luc_qc_fluc_overview", width = "100%")
                     ),
                 box(width=12, title = "Overview RLUC", solidHeader = TRUE, collapsible = TRUE, status="primary",
                     plotOutput("luc_qc_rluc_overview", width = "100%")
                 ),
                 box(width=12, title = "Overview FLUC normalized by RLUC", solidHeader = TRUE, collapsible = TRUE, status="primary",
                     plotOutput("luc_qc_divided_overview", width = "100%")
                 )
                 
                 
                 
                 )
        
)

