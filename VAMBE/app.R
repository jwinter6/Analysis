#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#########################
# VAMBE - Visual Analysis of Molecular Biology Experiments
#########################

# Will be used for
# - analysis of Incucyte Experiments
# - analysis of Dual Luciferase Assay
# - analysis of qPCR Data

# load packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(highcharter)
library(reshape2)
library(data.table)
library(DT)
library(httr)
library(openxlsx)
library(shinyjs)
library(shinyBS)
library(qpcR)



options(shiny.reactlog=FALSE) 
options(shiny.sanitize.errors = FALSE)


## CONFIG
config <- list()
config$mainDir <- "."
config$appDir <- file.path(config$mainDir, "app")

# create userDir
startTime <- Sys.time()
config$userID <- paste0(format(startTime, format = "%y%m%d_%H%M%S_"), paste(sample(0:9, 4), collapse = ""))
config$userDir <- file.path(config$mainDir, config$userID)

dir.create(config$userDir)




# Define UI for application that draws a histogram

###############
#### header####
###############
header <- dashboardHeader( 
  
)




#################
#### sidebar ####
#################
sidebar <- dashboardSidebar(sidebarMenu(
  
  ## Welcome
  menuItem("Welcome", tabName = "welcome", icon = icon("home")),
  
  
  ## Dual Luciferase Assay
  # User is required to upload the XLSX overview file so that the app knows what has been done
  # Additionally, the user needs to upload the TXT based luciferase readout files -> must assign the XLSX worksheets to the files -> done by getting the name _XLUC_plateX !
  menuItem("Dual Luciferase Assay", tabName = "luc", icon = icon("gear"),
           collapsible = TRUE,
           menuSubItem("Upload your Data", tabName = "luc_data"),
           # Upload data:
           # - show upload for XLSX file -> DETECT if 96 or 384 well plate!
           # - show upload for TXT-based Luciferase Files (must be equal number, falf RLUC, half FLUC) -> detect if 96 or 384 well plate!
           # - after upload: extract data and show success with number of files and assigned XLSX worksheet
           #
           menuSubItem("Check Quality", tabName = "luc_qc"),
           # Plate Overview (derived from excel)
           # PLate Overview RLUC
           # Plate Overview FLUC
           # Plate Overview FLUC/RLUC
           menuSubItem("Analysis", tabName = "luc_analysis")
           #menuSubItem("Set Analysis Parameters", tabName = "settings")
  ),
  
  ## Incucyte Assay
  # User is required to upload the Incuyte Data Export File only -> Layout MUSS in Incucyte gesetzt sein!
  # All information will be retrieved from this file
  
  menuItem("Incucyte Assay", tabName = "incucyte", icon = icon("gear"),
           collapsible = TRUE,
           menuSubItem("Upload your Data", tabName = "incucyte_data"),
           menuSubItem("Check Quality", tabName = "incucyte_qc"),
           menuSubItem("Analysis", tabName = "incucyte_analysis")
           #menuSubItem("Set Analysis Parameters", tabName = "settings")
  ),

  ## qPCR
  # user needs to upload the XLSX overview file so that the app knows what has been done
  # If user used the robot, information will be derived additional from the cDNA and Mastermix plates for QC, otherwise only the 394 well overview plate will be shown
  # Analysis is done with qpcR
  
  menuItem("qPCR", tabName = "qpcr", icon = icon("gear"),
           collapsible = TRUE,
           menuSubItem("Upload your Data", tabName = "qpcr_data"),
           menuSubItem("Check Quality", tabName = "qpcr_qc"),
           menuSubItem("Analysis", tabName = "qpcr_analysis")
           #menuSubItem("Set Analysis Parameters", tabName = "settings")
  )
  
  
  
)#,

#shiny::bookmarkButton(label = "Save Session", title= "Save your Session on the server for later use")
)




##############
#### body ####
##############
#tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "test.css")), 
body <- dashboardBody(

  shiny::tags$head(
    #shiny::tags$style(HTML(config$stylesheet)),
    #shiny::includeScript("analytics.js"),
    #shiny::includeScript("tooltip-delay.js")
  ),
  
  shinydashboard::tabItems(
    
    ## Welcome
    source(file.path(config$appDir, "welcome_ui.R"), local = TRUE)$value,
    
    ## Dual Luciferase Assay
    source(file.path(config$appDir, "luc_data.R"), local = TRUE)$value,
    source(file.path(config$appDir, "luc_qc.R"), local = TRUE)$value,
    source(file.path(config$appDir, "luc_analysis.R"), local = TRUE)$value,
    
    ## Incucyte
    source(file.path(config$appDir, "incucyte_data.R"), local = TRUE)$value,
    source(file.path(config$appDir, "incucyte_qc.R"), local = TRUE)$value,
    source(file.path(config$appDir, "incucyte_analysis.R"), local = TRUE)$value,
    
    
    ## qPCR
    source(file.path(config$appDir, "qpcr_data.R"), local = TRUE)$value,
    source(file.path(config$appDir, "qpcr_qc.R"), local = TRUE)$value,
    source(file.path(config$appDir, "qpcr_analysis.R"), local = TRUE)$value
   
    
  )#,
  
  #### MODALS
  ### So that modals appear on any page
  

  # shinyBS::bsModal(id = "fastqextraction_finished", title = "Data Upload and Data Check finished", trigger = "test", size = "large", 
  #                  fluidRow(
  #                    style="width100%;",
  #                    column(width=8, offset=2, class="alert alert-success",
  #                           shiny::tags$span(style="float:left;padding:10px;", HTML('<i class="fa fa-check fa-4x"></i>')),
  #                           shiny::tags$span(
  #                             shiny::tags$p(class="lead text-center", "Your data files have been uploaded and checked successfully."),
  #                             shiny::tags$p(class="text-center","As a next step, please go to the Data Review section.")
  #                           )
  #                    )
  #                  )
  # )

  
  
  
  )




################
#### run UI ####
################
# compatible with bookmarking state
ui <- dashboardPage( 
  header,
  sidebar,
  body )



# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  cData <- session$clientData
  
  # once session is done, remove config$userDir
  session$onSessionEnded(function() {
    print(paste("remove UserDirectory: ", config$userDir))
    system2(command = "rm", args = c("-r", "-f", config$userDir))
    unlink(config$userDir, recursive = TRUE)
  })
  
  # 4096MB upload limit per file
  options(shiny.maxRequestSize = 4096 * 1024^2)
 
  
    # load incucyte module
  
    # load  Dual luciferase module
  
    # load qPCR module
}

# Run the application 
shinyApp(ui = ui, server = server)

