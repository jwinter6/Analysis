# saved as qpcr_data.R

tabItem(tabName = "qpcr_data", align = "center",
        
        shinyjs::useShinyjs(),
        
        shiny::fluidRow(style="width:85%",
                        
                        # Put content here
                        box(width=6, title = "Upload the Excel qPCR Overview File", solidHeader = TRUE, collapsible = FALSE, status="primary",
                            
                            #shiny::tags$h4("Upload File"),
                            shiny::fileInput("qpcr_xlsx",label = "Upload the .xlsx qPCR overview file",multiple = FALSE,width = "90%"),
                            #actionButton("submit_luc_xlsx", "Set XLSX File", class="btn"),
                            shiny::tags$br(),
                            shiny::tags$h4("Uploaded Files"),
                            uiOutput("qpcr_xlsx_error"),
                            textOutput("qpcr_xlsx_filename")#,
                            
                            
                            # 
                            # column(width=10, offset=1,
                            #        box(width=12, solidHeader = TRUE, collapsible = FALSE, status="primary", title = "Please choose the plate normalization method",
                            #            uiOutput("luc_analysis_normalize_select")
                            #        )
                            # )
                            
                        ),
                        box(width=6, title = "Upload the qPCR Raw data file", solidHeader = TRUE, collapsible = FALSE, status="primary",
                            
                            # shiny::tags$h4("Upload Files"),
                            shiny::helpText("Please upload the raw data file from the LightCycler 480 - NO CT VALUE FILE!"),
                            shiny::helpText("Cq values will be calculated for you - and you can even choose which method to use."),
                            shiny::fileInput("qpcr_data",label = "Upload the qPCR raw data file",multiple = FALSE, width = "90%"),
                            #actionButton("submit_luc_data", "Set Data Files", class="btn"),
                            shiny::tags$br(),
                            shiny::tags$h4("Uploaded Files"),
                            uiOutput("qpcr_data_error"),
                            uiOutput("qpcr_data_filename")
                        )
                        
        ),
        # Show additional parameters
        shiny::fluidRow(style="width:85%",
                        column(width=10, offset=1,
                        box(width=12, title = "Additional Parameters", solidHeader = TRUE, collapsible = FALSE, status="primary",
                            
                            # shiny::tags$h4("Upload Files"),
                            shiny::helpText("Please adjust the qPCR analysis paramaters below."),
                            shiny::tags$br(),
                            shiny::tags$hr(width="50%"),
                            shiny::tags$h4("Select the Cq calculation method"),
                            
                            shiny::helpText("cpD2 is the default of the LightCycler and calculates a Ct value. The more advanced method is Cy0."),
                            shiny::selectInput(inputId = "qpcr_cq_method", label = "Cq calculation method", choices = c("cpD2", "cpD1", "Cy0"), selected = "cpD2",width = "30%"),
                            shiny::tags$br(),
                            shiny::tags$hr(width="50%"),
                            shiny::tags$h4("Select the minimum and maximum expected Cq values"),
                            shiny::helpText("All wells with lower or higher Cq values will be ignored."),
                            shiny::tags$br(),
                            shiny::textInput(inputId = "minCT",label = "Minimum expected Cq value",value = 5,width = "40%",placeholder = "Please select the minimum expected Cq threshold."),
                            shiny::textInput(inputId = "maxCT",label = "Maximum expected Cq value",value = 35,width = "40%",placeholder = "Please select the maximum expected Cq threshold."),
                            
                            
                            shiny::tags$br(),
                            shiny::tags$hr(width="50%"),
                            
                            shiny::tags$h4("Select the normalization method"),
                            shiny::tags$dl(
                              shiny::tags$dt("deltaCt"),
                              shiny::tags$dd("Default. Requires the selection of one or multiple reference genes and calculaten the differential Cq (deltaCt) between the reference gene(s) and each target gene.")
                            ),
                            shiny::selectInput(inputId = "qpcr_normalize_method", label = "Data Normalization", choices = c("deltaCt"), selected = "deltaCt",width = "30%"),
                            shiny::tags$br(),
                            shiny::helpText("Please select reference genes for normalization."),
                            shiny::tags$strong("required"),
                            shiny::uiOutput("qpcr_refgenes"),
                            shiny::tags$br(),
                            shiny::helpText("Please select all endogeneous control genes"),
                            shiny::tags$strong("required"),
                            shiny::uiOutput("qpcr_control"),
                            shiny::helpText("Please select all positive control genes"),
                            shiny::uiOutput("qpcr_poscontrol"),
                            shiny::helpText("Please select all negative control genes"),
                            shiny::uiOutput("qpcr_negcontrol"),
                            shiny::tags$br(),
                            
                            shiny::actionButton(inputId = "qpcr_start_calculation",class="btn btn-success",label = "Start Cq Calculation",width = "30%"),
                            shiny::actionButton(inputId = "qpcr_reset_calculation",class="btn btn-warning",label = "RESET Calculation",width = "30%")
                            
                        )
                        )
                        
                        
                        
        ),
        
        # Show input files
        shiny::fluidRow(style="width:85%",
                        box(width=12, title = "Uploaded data", solidHeader = TRUE, collapsible = FALSE, status="primary",
                            shiny::tags$h4("Plate Overview Target Genes according to XLSX file"),
                            plotOutput("qpcr_overview_genes"),
                            
                            shiny::tags$hr(width="50%"),
                            shiny::tags$h4("Plate Overview Samples according to Robot"),
                            plotOutput("qpcr_overview_samples"),
                            
                            shiny::tags$hr(width="50%"),
                            shiny::tags$h4("Plate Overview Targets according to Robot"),
                            plotOutput("qpcr_overview_target")
                            
                            
                            
                            
                        ),
                        box(width=12, title = "Data Overview", solidHeader = TRUE, collapsible = TRUE, status="primary",
                            shiny::helpText("This data is in a tidy format."),
                            DT::dataTableOutput("qpcr_xlsx_rawdata")
                        )
                        
                        
                        
        )
        
)

