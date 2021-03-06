# saved as luc_analysis.R

tabItem(tabName = "luc_analysis", align = "center",
        
        shinyjs::useShinyjs(),
        
        shiny::fluidRow(style="width:95%",
                        
                        # Put content here
                        
                        tabBox(width=12,height = "1000px",
                               title = "Raw Data Overview",
                               tabPanel( "FLUC",
                                         downloadButton("DL_luc_analysis_plot_rawdata_FLUC", label = "Download Plot"),
                                 plotOutput("luc_analysis_plot_rawdata_FLUC")
                               ),
                               tabPanel("RLUC",
                                        downloadButton("DL_luc_analysis_plot_rawdata_RLUC", label = "Download Plot"),
                                        plotOutput("luc_analysis_plot_rawdata_RLUC")
                                        ),
                               tabPanel("Ratio",
                                        downloadButton("DL_luc_analysis_plot_rawdata_RATIO", label = "Download Plot"),
                                        plotOutput("luc_analysis_plot_rawdata_RATIO")
                                        ),
                               tabPanel("Log2 Ratio",
                                        downloadButton("DL_luc_analysis_plot_rawdata_LOG2RATIO", label = "Download Plot"),
                                        plotOutput("luc_analysis_plot_rawdata_LOG2RATIO")
                                        ),
                               tabPanel( "FLUC PLATE",
                                         downloadButton("DL_luc_analysis_plot_rawdata_FLUC_plate", label = "Download Plot"),
                                         plotOutput("luc_analysis_plot_rawdata_FLUC_plate")
                               ),
                               tabPanel( "RLUC PLATE",
                                         downloadButton("DL_luc_analysis_plot_rawdata_RLUC_plate", label = "Download Plot"),
                                         plotOutput("luc_analysis_plot_rawdata_RLUC_plate")
                               ),
                               tabPanel( "Log2 Ratio PLATE",
                                         downloadButton("DL_luc_analysis_plot_rawdata_RATIO_plate", label = "Download Plot"),
                                         plotOutput("luc_analysis_plot_rawdata_RATIO_plate")
                               )
                               ),
                        
                        column(width=12,
                               column(width=6, offset=3,
                                      box(width=12, solidHeader = TRUE, collapsible = FALSE, status="primary", title = "Please select the treatments to plot",
                                          uiOutput("luc_analysis_rawdata_select")
                                      )
                                      ),
                                      

                        tabBox(width=12,
                               title = "User-Defined",
                               
                               
                               
                               tabPanel( "FLUC",
                                         downloadButton("DL_luc_analysis_plot_individual_FLUC", label = "Download Plot"),
                                         plotOutput("luc_analysis_plot_individual_FLUC")
                               ),
                               tabPanel("RLUC",
                                        downloadButton("DL_luc_analysis_plot_individual_RLUC", label = "Download Plot"),
                                        plotOutput("luc_analysis_plot_individual_RLUC")
                               ),
                               tabPanel("Ratio",
                                        downloadButton("DL_luc_analysis_plot_individual_RATIO", label = "Download Plot"),
                                        plotOutput("luc_analysis_plot_individual_RATIO")
                               ),
                               tabPanel("Log2 Ratio",
                                        downloadButton("DL_luc_analysis_plot_individual_LOG2RATIO", label = "Download Plot"),
                                        plotOutput("luc_analysis_plot_individual_LOG2RATIO")
                               )
                        )
                        ),
                        
                        
                        column(width=12,
                               column(width=6, offset=3,
                                      box(width=12, solidHeader = TRUE, collapsible = FALSE, status="primary", title = "Please select the calibrator",
                                          shiny::helpText("Select the treatment to which you want to calibrate all of the selected samples above to."),
                                          uiOutput("luc_analysis_calibrator_select")
                                      )
                               ),
                               tabBox(width=12,
                                      title = "Calibrated Sets",
                                      
                                      
                                      
                                      tabPanel( "FLUC",
                                                downloadButton("DL_luc_analysis_plot_calibrated_FLUC", label = "Download Plot"),
                                                plotOutput("luc_analysis_plot_calibrated_FLUC")
                                      ),
                                      tabPanel("RLUC",
                                               downloadButton("DL_luc_analysis_plot_calibrated_RLUC", label = "Download Plot"),
                                               plotOutput("luc_analysis_plot_calibrated_RLUC")
                                      ),
                                      tabPanel("Ratio",
                                               downloadButton("DL_luc_analysis_plot_calibrated_RATIO", label = "Download Plot"),
                                               plotOutput("luc_analysis_plot_calibrated_RATIO")
                                      ),
                                      tabPanel("Log2 Ratio",
                                               downloadButton("DL_luc_analysis_plot_calibrated_LOG2RATIO", label = "Download Plot"),
                                               plotOutput("luc_analysis_plot_calibrated_LOG2RATIO")
                                               
                                      )
                               ),
                               shiny::tags$br(),
                               column(width=12,
                                      box(title = "Calibrated Data as shown in the Plot", solidHeader = TRUE,status="primary",width = 12, collapsible = FALSE,
                                          shiny::tags$br(),
                                          dataTableOutput("luc_calibrated_datatable"),
                                          shiny::tags$br(),
                                          shiny::tags$br()
                                          )

                                      )
                               
                        )
                        
                        
        )
        
)

