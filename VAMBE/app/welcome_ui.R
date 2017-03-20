# saved as welcome_ui.R

tabItem(tabName = "welcome", align = "center",
        
        shinyjs::useShinyjs(),
        
        shiny::fluidRow(width="85%",
         
           # Put content here
           shiny::column(width=12,
                         shiny::tags$div(style="display:block, height:3%")
                         ),
           
           shiny::column(width=6, offset=3,
                         shiny::tags$h3(class="text-success", "Welcome to VAMBE"),
                         shiny::tags$h4(HTML("<strong>V</strong>isual <strong>A</strong>nalysis of <strong>M</strong>olecular <strong>B</strong>iology <strong>E</strong>xperiments"))
                         )
           
           
           )

)