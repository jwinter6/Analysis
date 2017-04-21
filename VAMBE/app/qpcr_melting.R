# saved as qpcr_melting.R

tabItem(tabName = "qpcr_melting", align = "center",
        
        shinyjs::useShinyjs(),
        
        shiny::fluidRow(style="width:85%",
                        
                        # Put content here
                        shiny::column(width=8, offset=2,
                                      box(title="Select Target Gene", width=12, solidHeader = TRUE, status="primary",
                                          uiOutput("qpcr_meltcurve_target")
                                      )
                        ),
                        
                        
                        shiny::column(width=12,
                                      
                                      box(width = 12, title = "Melting Curves",solidHeader = TRUE, status="primary",
                                             
                                             plotOutput("qpcr_qc_meltcurve", height = "800px")
                                             
                                      )
                        ),
                        
                        
                        shiny::column(width=12,
                                      
                                      box(width = 12, title = "Fluorescence Curves",solidHeader = TRUE, status="primary",
                                          
                                          plotOutput("qpcr_qc_fluorescencecurve", height = "800px")
                                          
                                      )
                        )
                        
        )
        
)

