# saved as qpcr_analysis.R

tabItem(tabName = "qpcr_analysis", align = "center",
        
        shinyjs::useShinyjs(),
        
        shiny::fluidRow(style="width:85%",
                        
                        # Put content here
                        shiny::column(width=12,
                                      column(width=8, offset=2,
                                             box(title = "Select the Target Genes", solidHeader = TRUE, status="primary",width=12,
                                                 uiOutput("qpcr_analysis_target"),
                                                 shiny::tags$hr(width="50%"),
                                                 uiOutput("qpcr_analysis_calibrator"),
                                                 shiny::tags$hr(width="50%"),
                                                 uiOutput("qpcr_analysis_samples")
                                                 
                                             )
                                             ),
                                     
                                      tabBox(title = "Analysis", width=12,
                                             
                                             tabPanel("Cq Values",
                                                      #plots raw Cq values
                                                      downloadButton("DL_qpcr_analysis_plot_cq", label = "Download Plot"),
                                                      plotOutput("qpcr_analysis_plot_cq")
                                             ),
                                             tabPanel("ddCq normalized",
                                                      #plots ddCT normalized Cq values
                                                      downloadButton("DL_qpcr_analysis_plot_ddcq", label = "Download Plot"),
                                                      plotOutput("qpcr_analysis_plot_ddcq")
                                             ),
                                             tabPanel("Foldchange",
                                                      #plots ddCT normalized Cq values
                                                      downloadButton("DL_qpcr_analysis_plot_calibrated", label = "Download Plot"),
                                                      plotOutput("qpcr_analysis_plot_calibrated")
                                             ),
                                             tabPanel("Log2 Foldchange",
                                                      #plots ddCT normalized Cq values
                                                      downloadButton("DL_qpcr_analysis_plot_calibrated_log2", label = "Download Plot"),
                                                      plotOutput("qpcr_analysis_plot_calibrated_log2")
                                             ),
                                             tabPanel("Log10 Foldchange",
                                                      #plots ddCT normalized Cq values
                                                      downloadButton("DL_qpcr_analysis_plot_calibrated_log10", label = "Download Plot"),
                                                      plotOutput("qpcr_analysis_plot_calibrated_log10")
                                             )
                                             
                                             
                                             
                                             ),
                                      box(title = "Tidy Data Table", solidHeader = TRUE, status = "primary",width = 12,collapsible = FALSE,
                                          
                                          shiny::helpText("You can download the analysed and calibrated data in a tidy format."),
                                          dataTableOutput("qpcr_analysis_calibrated_table")
                                          
                                          )
                                      )
                        
        )
        
)

