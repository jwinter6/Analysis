# saved as luc_analysis.R

tabItem(tabName = "luc_analysis", align = "center",
        
        shinyjs::useShinyjs(),
        
        shiny::fluidRow(style="width:95%",
                        
                        # Put content here
                        
                        tabBox(width=12,
                               title = "Raw Data Overview",
                               tabPanel( "FLUC",
                                         downloadButton("DL_luc_analysis_plot_rawdata_FLUC", label = "Download Plot"),
                                 plotOutput("luc_analysis_plot_rawdata_FLUC")
                               ),
                               tabPanel("RLUC",
                                        plotOutput("luc_analysis_plot_rawdata_RLUC")
                                        ),
                               tabPanel("Ratio",
                                        plotOutput("luc_analysis_plot_rawdata_RATIO")
                                        ),
                               tabPanel("Log2 Ratio",
                                        plotOutput("luc_analysis_plot_rawdata_LOG2RATIO")
                                        )
                               )
                        
        )
        
)

