# saved as qpcr_qc.R

tabItem(tabName = "qpcr_qc", align = "center",
        
        shinyjs::useShinyjs(),
        
        shiny::fluidRow(style="width:85%",
                        
                        # Put content here
                        
                        
                        # CQ OVERVIEW
                        shiny::column(width=12,

                                      box(width = 12, title = "Plate Overview",status = "primary", solidHeader = TRUE,
                                          shiny::column(width=8, offset=2,
                                                        shiny::tags$h3("Select Sample"),
                                                        uiOutput("qc_qpcr_sample")
                                                        
                                          ),
                                          column(width=6,
                                                    shiny::tags$h3(class="text-success", "Raw Cq Values"),
                                                    
                                                    downloadButton("DL_qpcr_qc_cqraw", label = "Download Plot"),
                                                    plotOutput("qpcr_qc_cqraw")
                                                    ),
                                          column(width=6,
                                                 shiny::tags$h3(class="text-success", "ddCq (DeltaDelta Cq)"),
                                                 downloadButton("DL_qpcr_qc_cqnorm", label = "Download Plot"),
                                                 plotOutput("qpcr_qc_cqnorm")
                                                 )
                                             
                                             )
                                      ),
                        # Variation
                        shiny::column(width=12,
                                      
                                          
                                          tabBox(width=12,
                                                 title = "Cq Data",
                                                 tabPanel( "Normalization",
                                                           
                                                           # plot(exprs...)
                                                           
                                                           downloadButton("DL_qpcr_qc_plot_normalization", label = "Download Plot"),
                                                            plotOutput("qpcr_qc_plot_normalization")
                                                           
                                                 ),
                                                 tabPanel("Sample Standard Deviation",
                                                          #plotCtVariation()
                                                          downloadButton("DL_qpcr_qc_plot_variation", label = "Download Plot"),
                                                          plotOutput("qpcr_qc_plot_variation")
                                                 ),
                                                 tabPanel("Target Gene Quality",
                                                          #plotCtVariation()
                                                          downloadButton("DL_qpcr_qc_plot_determined", label = "Download Plot"),
                                                          
                                                          plotOutput("qpcr_qc_plot_determined")
                                                          
                                                 ),
                                                 tabPanel("Cq Density",
                                                          #plotCtDensity()
                                                          downloadButton("DL_qpcr_qc_plot_density", label = "Download Plot"),
                                                          plotOutput("qpcr_qc_plot_density")
                                                 ),
                                                 tabPanel("Cq of Controls",
                                                          # plotctboxes
                                                          downloadButton("DL_qpcr_qc_plot_boxes", label = "Download Plot"),
                                                          plotOutput("qpcr_qc_plot_boxes")
                                                 ),
                                                 tabPanel("Sample Correlation",
                                                          # plotpairs
                                                          downloadButton("DL_qpcr_qc_plot_pairs", label = "Download Plot"),
                                                          plotOutput("qpcr_qc_plot_pairs")
                                                 ),
                                                 tabPanel("PCA",
                                                          # plotctpca
                                                          downloadButton("DL_qpcr_qc_plot_pca", label = "Download Plot"),
                                                          plotOutput("qpcr_qc_plot_pca")
                                                 ),
                                                 tabPanel("Heatmap",
                                                          # plotctheatmap
                                                          shiny::selectInput(inputId = "qpcr_qc_heatmap",label = "Please select the distance to determine clusters",choices = c("euclidean","pearson"),selected = "euclidean",multiple = FALSE,width = "30%"),
                                                          
                                                          shiny::tags$h4("Cq raw data"),
                                                          downloadButton("DL_qpcr_qc_plot_heatmap_raw", label = "Download Plot"),
                                                          plotOutput("qpcr_qc_plot_heatmap_raw",height = "1000px"),
                                                          shiny::tags$br(),
                                                          shiny::tags$h4("ddCT normalized"),
                                                          downloadButton("DL_qpcr_qc_plot_heatmap_norm", label = "Download Plot"),
                                                          plotOutput("qpcr_qc_plot_heatmap_norm",height = "1000px")
                                                          
                                                 )
                                          ),
                                          
                                      box(width=12, title = "Calculated Data Overview", solidHeader = TRUE, collapsible = TRUE, status="primary",
                                          shiny::helpText("This data is in a tidy format."),
                                          DT::dataTableOutput("qpcr_tidydata")
                                      )
                                      
                                      )
                        
        )
        
)

