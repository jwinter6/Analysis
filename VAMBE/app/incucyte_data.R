# saved as incucyte_data.R

tabItem(tabName = "incucyte_data", align = "center",
        
        shinyjs::useShinyjs(),
        
        shiny::fluidRow(style="width:85%",
                        
                        # Put content here
                        box(width=6, title = "Upload the Incuyte Export File", solidHeader = TRUE, collapsible = FALSE, status="primary",
                            
                            shiny::tags$h4("Upload File")
                            
                            
                            )
                        
        )
        
)

