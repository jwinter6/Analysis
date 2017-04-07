# saved as qpcr_melting.R

tabItem(tabName = "qpcr_melting", align = "center",
        
        shinyjs::useShinyjs(),
        
        shiny::fluidRow(style="width:85%",
                        
                        # Put content here
                        shiny::column(width=8, offset=2,
                                      box(title="Select Target Gene", width=12, solidHeader = TRUE, status="primary"
                                          #uiOutput("qc_qpcr_melting_target")
                                      )
                        ),
                        
                        
                        shiny::column(width=12,
                                      
                                      tabBox(width = 12, title = "Melting Curves"
                                             
                                             # tabPanel("Raw Cq Values",
                                             #          downloadButton("DL_qpcr_qc_cqraw", label = "Download Plot"),
                                             #          plotOutput("qpcr_qc_cqraw")
                                             # ),
                                             # tabPanel("ddCq Values",
                                             #          downloadButton("DL_qpcr_qc_cqnorm", label = "Download Plot"),
                                             #          plotOutput("qpcr_qc_cqnorm")
                                             # )#,
                                             # tabPanel("Ratio",
                                             #          downloadButton("DL_luc_analysis_plot_rawdata_RATIO", label = "Download Plot"),
                                             #          plotOutput("luc_analysis_plot_rawdata_RATIO")
                                             # ),
                                             # tabPanel("Log2 Ratio",
                                             #          downloadButton("DL_luc_analysis_plot_rawdata_LOG2RATIO", label = "Download Plot"),
                                             #          plotOutput("luc_analysis_plot_rawdata_LOG2RATIO")
                                             # )
                                             
                                      )
                        )
                        
        )
        
)

