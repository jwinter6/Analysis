# saved as luc_data.R

tabItem(tabName = "luc_data", align = "center",
        
        shinyjs::useShinyjs(),
        
        shiny::fluidRow(style="width:85%",
                        
                        # Put content here
                        box(width=6, title = "Upload the Excel Overview File", solidHeader = TRUE, collapsible = FALSE, status="primary",
                            
                            #shiny::tags$h4("Upload File"),
                            shiny::fileInput("luc_xlsx",label = "Upload the .xlsx overview file",multiple = FALSE,width = "90%"),
                            #actionButton("submit_luc_xlsx", "Set XLSX File", class="btn"),
                            shiny::tags$br(),
                            shiny::tags$h4("Uploaded Files"),
                            uiOutput("luc_xlsx_error"),
                            textOutput("luc_xlsx_filename"),
                            
                            
                            
                            column(width=10, offset=1,
                                   box(width=12, solidHeader = TRUE, collapsible = FALSE, status="primary", title = "Please choose the plate normalization method",
                                       uiOutput("luc_analysis_normalize_select")
                                   )
                            )
                            
                        ),
                        box(width=6, title = "Upload the Luciferase Readout Files", solidHeader = TRUE, collapsible = FALSE, status="primary",
                            
                            # shiny::tags$h4("Upload Files"),
                            shiny::helpText("Files must be named in the following way:", shiny::tags$br(), "FILENAME_Plate1_RLUC or FILENAME_Plate1_FLUC"),
                            shiny::fileInput("luc_data",label = "Upload the luciferase data files",multiple = TRUE,width = "90%"),
                            #actionButton("submit_luc_data", "Set Data Files", class="btn"),
                            shiny::tags$br(),
                            shiny::tags$h4("Uploaded Files"),
                            uiOutput("luc_data_error"),
                            uiOutput("luc_data_filename")
                        )
                        
        ),
        
        # Show input files
        shiny::fluidRow(style="width:85%",
                        box(width=12, title = "Uploaded files", solidHeader = TRUE, collapsible = FALSE, status="primary",
                            shiny::tags$h4("Plate Overview"),
                            uiOutput("luc_plate_samplesview")
                        ),
                        box(width=12, title = "Data Overview", solidHeader = TRUE, collapsible = TRUE, status="primary",
                            shiny::tags$h4("Data Overview"),
                            DT::dataTableOutput("luc_qc_datatable")
                        )
                        
                        
                        
                        )
        
)