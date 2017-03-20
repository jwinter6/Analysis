# server.R

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


source("config.R")

# create userDir
startTime <- Sys.time()
config$userID <- paste0(format(startTime, format = "%y%m%d_%H%M%S_"), paste(sample(0:9, 4), collapse = ""))
config$userDir <- file.path(config$mainDir, config$userID)

dir.create(config$userDir)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  cData <- session$clientData
  
  # once session is done, remove config$userDir
  session$onSessionEnded(function() {
    print(paste("remove UserDirectory: ", config$userDir))
    system2(command = "rm", args = c("-r", "-f", config$userDir))
    unlink(config$userDir, recursive = TRUE)
  })
  
  # 4096MB upload limit per file
  options(shiny.maxRequestSize = 4096 * 1024^2)
  
  source(file.path(config$appDir, "outputs.R"), local=TRUE)
  source(file.path(config$appDir, "functions.R"), local=TRUE)
  
  # load incucyte module
  
  # load  Dual luciferase module
  
  
  # output$luc_xlsx_filename <- renderText({
  #   return(input$luc_xlsx$datapath)
  # })
  
  # load qPCR module
}
)