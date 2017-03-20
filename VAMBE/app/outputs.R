#saved as outputs.R

########################
#####   Luciferase Assay
########################


## File Upload

# XLSX file
 output$luc_xlsx_filename <- renderText({
   shiny::validate(
     shiny::need(input$luc_xlsx, message = "No file uploaded yet.")
   )
   return(input$luc_xlsx$name)
 })
luc_file_xlsx <- reactive( { #input$submit_luc_xlsx,
  luc_file <- input$luc_xlsx
  
  error$luc_xlsx = FALSE
  
  if(luc_file == "" || is.null(luc_file))
  {
    print("no File")
    return(NULL)
  } else
  {
    print("File uploaded")
    # make list to store informaiton
    out <- list()
    # read file
    workbook <- try(openxlsx::loadWorkbook(luc_file$datapath,xlsxFile = NULL))
    if(class(workbook) == "try-error")
    {
      print("TRY ERROR")
      error$luc_xlsx = TRUE
      return()
    }
    
    wb_names <- names(workbook)

    # Get Protocol
    protocols <- openxlsx::read.xlsx(xlsxFile = luc_file$datapath, sheet = "Protocol", colNames = FALSE, rowNames = FALSE)
    protocol <- protocols[,2]
    names(protocol) <- protocols[,1]

    # Get Sheets
    plates <- list()
    groups <- NA
    ## check if multiple
    for(i in 2:length(wb_names))
    {
      if(grepl(pattern = "Plate.+", x=wb_names[i]) )
      {
        plates[[wb_names[i]]]$layout <- as.data.frame(openxlsx::read.xlsx(xlsxFile = luc_file$datapath, sheet = wb_names[i], colNames = TRUE, rowNames = TRUE), stringsAsFactors=FALSE)

        plates[[wb_names[i]]]$shape <- reshape2::melt(as.matrix(plates[[wb_names[i]]]$layout))
        pattern <- expression("^(\\d{1})$")
        plates[[wb_names[i]]]$shape$Var2 <- sub(pattern = pattern, replacement = paste("0","\\1", sep=""), x=  plates[[wb_names[i]]]$shape$Var2, fixed = FALSE )
        plates[[wb_names[i]]]$shape$position <- paste0(plates[[wb_names[i]]]$shape$Var1, plates[[wb_names[i]]]$shape$Var2)

        # set treatment groups
        groups <- c(groups, as.character(plates[[wb_names[i]]]$shape$value))

      }

    }

    # according to tidyverse, data will be kept like that:
    ## Column1: treatment
    ## Column2: FLUC signal
    ## Column3: RLUC signal

    # Set plate size
    if(nrow(plates[[1]][[2]]) == 384)
    {
      plate_rows = 16
      plate_cols = 24
    } else
    {
      # 96 by default
      plate_rows = 8
      plate_cols = 12
    }

    # We achieve this by loading the data, parsing the position as plate X at well A12 and putting it together with the treatment in the row.
    # this rrequires that FLUC and RLUC signals of the same plate are in the same dataframe
    out$protocol <- protocol
    out$plates <- plates
    out$groups <-  unique(groups)
    out$plate_rows <- plate_rows 
    out$plate_cols <- plate_cols
    
    return(out)
  }


})

# Luciferase data
output$luc_data_filename <- renderUI({
  shiny::validate(
    shiny::need(input$luc_data, message = "No files uploaded yet.")
  )
  return(HTML(paste(input$luc_data$name, collapse = "</br>")))
})

luc_file_data <- reactive({ #input$submit_luc_data, 
  shiny::validate(
    shiny::need(!is.null(luc_file_xlsx), message = "Please upload a valid XLSX file first")
  )
  error$luc_data_error <- FALSE
  
  if(is.null(input$luc_data) || input$luc_data =="")
  {return(NULL)}
  else
  {
    # Uploaded files need to match the setting:
    # XXXXX_plateY_FLUC for Fluc data of plate Y
    # XXXXX_plateY_RLUC for RLUC data of plate Y
    
    # get names of files
    files <- list()
    platemap <- list()
    
    # get plate information
    data <- as.list(names(luc_file_xlsx()$plates))
    names(data) <- names(luc_file_xlsx()$plates)
    
    # test <- data.frame("name" = c("2017-03-17_HEKTCF-HCT116-TCF_Plate1-RLUC.TXT","2017-03-17_HEKTCF-HCT116-TCF_Plate1-FLUC.TXT"),
    #                    "datapath" = c("RLUC","FLUC"),
    #                    stringsAsFactors = FALSE
    #                    )
    # 
    # grepl(pattern = paste(".*",names(plates), ".*FLUC", sep=""),x = test$name )
    # grepl(pattern = paste(".*",names(plates), ".*RLUC", sep=""), x = test$name )
    # 
    for(i in 1:length(data))
    {
      # filename FLUC
      file_fluc <- grepl(pattern = paste(".*",names(data)[i], ".*FLUC", sep=""), x = input$luc_data$name )
      
      if(any(file_fluc))
      {
        # FLUC
        data[[i]]$FLUC <- readr::read_tsv(file = input$luc_data[file_fluc,"datapath"], col_names=FALSE, quoted_na = FALSE)
        colnames(data[[i]]$FLUC) <- c("Plate", "Well", "FLUC")
        data[[i]]$FLUC$Plate <- NULL
      } 
      
      # filename RLUC
      file_rluc <- grepl(pattern = paste(".*",names(data)[i], ".*RLUC", sep=""), x = input$luc_data$name )
      
      if(any(file_rluc))
      {
        # RLUC
        data[[i]]$RLUC <- readr::read_tsv(file = input$luc_data[file_rluc,"datapath"], col_names=FALSE, quoted_na = FALSE)
        colnames(data[[i]]$RLUC) <- c("Plate", "Well", "RLUC")
        data[[i]]$RLUC$Plate <- NULL
      }
      
      
      if(any(file_fluc) && any(file_rluc))
      {
        # Combine them in one
        data[[i]]$ALL <- dplyr::inner_join(data[[i]]$FLUC, data[[i]]$RLUC, by="Well")
        data[[i]]$ALL$Plate <- paste("Plate",i,sep="")
        
        # # Median normaliziation
        # if(normalize){
        #   data[[i]]$ALL$FLUC <- data[[i]]$ALL$FLUC/ median(data[[i]]$ALL$FLUC, na.rm = TRUE)
        #   data[[i]]$ALL$RLUC <- data[[i]]$ALL$RLUC/ median(data[[i]]$ALL$RLUC, na.rm = TRUE)
        # }
        
        # Add the treatment to it
        data[[i]]$ALL$Treatment <- sapply(data[[i]]$ALL$Well, function(x){
          # Get treatment for same plate from luc_file_xlsx()$plates$Plate1$[luc_file_xlsx()$plates$Plate1$$position == x , "value"]
          return(as.character(luc_file_xlsx()$plates[[names(data)[i]]]$shape[luc_file_xlsx()$plates[[names(data)[i]]]$shape$position == as.character(x),"value"]))
        })
        
        # Remove NA
        data[[i]]$ALL <- dplyr::filter(data[[i]]$ALL, !is.na(Treatment))
        
        # Create Platemap
        data[[i]]$platemap <- dplyr::mutate(as.data.frame(data[[i]]$ALL),
                                            Row=as.numeric(match(toupper(substr(Well, 1, 1)), LETTERS)),
                                            Column=as.numeric(substr(Well, 2, 5)))
        
        
        # PLate Overview of Samples for ggplot
        platemap[[names(data)[i]]] <- dplyr::mutate(luc_file_xlsx()$plates[[names(data)[i]]]$shape,
                                     Row=as.numeric(match(toupper(substr(position, 1, 1)), LETTERS)),
                                     Column=as.numeric(substr(position, 2, 5)))
        
      } else
      {
        error$luc_data_error <- TRUE
      }
     
      
      
    }
    
    
    # TRY ERROR CATCH for FILE OPENING
    if(error$luc_data_error)
    {
      print("Error with data files")
      error$luc_data = TRUE
      shinyBS::toggleModal(session, "luc_data_error_modal", toggle = "open")
      return()
    }
    
    # Now proceed to create plot tbl for ggplot2
    ## Aim: Merge all to have a single tbl
    
    plot <- list()
    
    # store in plot$data
    for(i in 1:length(data))
    {
      if(i==1)
      {
        # create tbl
        plot$data <- tibble::tibble("Plate" = data[[i]]$ALL$Plate,
                                    "Treatment" = data[[i]]$ALL$Treatment,
                                    "FLUC" = data[[i]]$ALL$FLUC,
                                    "RLUC" = data[[i]]$ALL$RLUC,
                                    "Well" = data[[i]]$ALL$Well,
                                    "FLUC.median" = NA,
                                    "FLUC.mean" = NA,
                                    "FLUC.sd" = NA,
                                    "RLUC.median" = NA,
                                    "RLUC.mean" = NA,
                                    "RLUC.sd" = NA
        )
        
      }
      else{
        # Add rows of other datasets
        plot$data <- dplyr::bind_rows(plot$data, data[[i]]$ALL[,c("Plate", "Treatment", "FLUC", "RLUC", "Well")])
      }
    }
    
    
    ## Normalize: FLUC / RLUC
    plot$data$Divided <- plot$data$FLUC/plot$data$RLUC
    
    
    ## Transform to log2
    plot$data$Log2 <- log2(plot$data$Divided)
    
    # Transform Inf and -Inf to NA
    plot$data$Divided <- sapply(plot$data$Divided, function(x){
      if(is.infinite(x) || is.nan(x)){
        return(NA)
      } else {
        return(x)
      }
    })
    
    plot$data$Log2 <- sapply(plot$data$Log2, function(x){
      if(is.infinite(x) || is.nan(x)){
        return(NA)
      } else {
        return(x)
      }
    })
    
    treatmentgroups <- tibble::tibble("Treatment" = luc_file_xlsx()$groups,
                                      "FLUC.median" = NA,
                                      "FLUC.mean" = NA,
                                      "FLUC.sd" = NA,
                                      "RLUC.median" = NA,
                                      "RLUC.mean" = NA,
                                      "RLUC.sd" = NA
    )
    treatmentgroups <- treatmentgroups[-is.na(treatmentgroups$Treatment),]
    
    treatmentgroups$FLUC.mean <- sapply(treatmentgroups$Treatment, function(x){
      return(mean(plot$data[plot$data$Treatment == as.character(x),"FLUC"]$FLUC, na.rm=TRUE))
    })
    treatmentgroups$FLUC.median <- sapply(treatmentgroups$Treatment, function(x){
      return(median(plot$data[plot$data$Treatment == as.character(x),"FLUC"]$FLUC, na.rm=TRUE))
    })
    treatmentgroups$FLUC.sd <- sapply(treatmentgroups$Treatment, function(x){
      return(sd(plot$data[plot$data$Treatment == as.character(x),"FLUC"]$FLUC, na.rm=TRUE))
    })
    
    treatmentgroups$RLUC.mean <- sapply(treatmentgroups$Treatment, function(x){
      return(mean(plot$data[plot$data$Treatment == as.character(x),"RLUC"]$RLUC, na.rm=TRUE))
    })
    treatmentgroups$RLUC.median <- sapply(treatmentgroups$Treatment, function(x){
      return(median(plot$data[plot$data$Treatment == as.character(x),"RLUC"]$RLUC, na.rm=TRUE))
    })
    treatmentgroups$RLUC.sd <- sapply(treatmentgroups$Treatment, function(x){
      return(sd(plot$data[plot$data$Treatment == as.character(x),"RLUC"]$RLUC, na.rm=TRUE))
    })
    
    treatmentgroups$activity.mean <- treatmentgroups$FLUC.mean / treatmentgroups$RLUC.mean
    #dplyr::arrange(treatmentgroups, Treatment)
    
    # add SD to plot$data so that it can be used for more plot
    for(i in 1:nrow(plot$data)) {
      
      plot$data$FLUC.median[i] <- treatmentgroups$FLUC.median[treatmentgroups$Treatment == plot$data$Treatment[i]]
      plot$data$FLUC.mean[i] <- treatmentgroups$FLUC.mean[treatmentgroups$Treatment == plot$data$Treatment[i]]
      plot$data$FLUC.sd[i] <- treatmentgroups$FLUC.sd[treatmentgroups$Treatment == plot$data$Treatment[i]]
      plot$data$RLUC.median[i] <- treatmentgroups$RLUC.median[treatmentgroups$Treatment == plot$data$Treatment[i]]
      plot$data$RLUC.mean[i] <- treatmentgroups$RLUC.mean[treatmentgroups$Treatment == plot$data$Treatment[i]]
      plot$data$RLUC.sd[i] <- treatmentgroups$RLUC.sd[treatmentgroups$Treatment == plot$data$Treatment[i]]
    }
    
    
    # Return data
    out <- list()
    out$data <- data
    out$plot <- plot$data
    out$platemap <- platemap
    out$treatmentgroups <- treatmentgroups
    
    return(out)
    
  }
  
  
})

# Observe
observe(luc_file_xlsx())
observe(luc_file_data())



# Errors
output$luc_xlsx_error <- renderUI({
  if(error$luc_xlsx == TRUE)
  {
    return(HTML("<strong>The uploaded file is not a correct overview XLSX file</strong>"))
  } else
  {return()}
})

output$luc_data_error <- renderUI({
  if(error$luc_data == TRUE)
  {
    return(HTML("<strong>The uploaded files do not have the correct file naming</strong>"))
  } else
  {return()}
})





## Overview Plots

get_plot_output_list <- function(max_plots, platemapinfo, plate_cols = NULL, plate_rows = NULL) {
  # Insert plot output objects the list
  plot_output_list <- lapply(1:max_plots, function(i) {
    plotname <- paste("plot", i, sep="")
    plot_output_object <- plotOutput(plotname, height = "auto", width = "90%")
    
    plot_output_object <- renderPlot({
      platemap <- platemapinfo[[i]]
      
      g <- ggplot2::ggplot(data=platemap, aes(x=Column, y=Row)) +
        geom_point(data=expand.grid(seq(1, plate_cols), seq(1, plate_rows)), aes(x=Var1, y=Var2),
                   color="grey90", fill="white", shape=21, size=6) +
        geom_point(aes(color=value), size=6) +
        coord_fixed(ratio=((plate_cols+1)/plate_cols)/((plate_rows+1)/plate_rows), xlim=c(0.5, plate_cols+1), ylim=c(0.5, plate_rows+1)) +
        scale_y_reverse(breaks=seq(1, plate_rows), labels=LETTERS[1:plate_rows]) +
        scale_x_continuous(breaks=seq(1, plate_cols)) +
        scale_shape_manual(values=seq(from=0, to = length(unique(platemap$value)), by=1 ) ) +
        labs(title=paste("Plate ", i ,": Sample Types") )
      
      return(g)
    }, res = 72)
  })
  
  do.call(tagList, plot_output_list) # needed to display properly.
  
  return(plot_output_list)
}


output$luc_plate_samplesview <- renderUI({
      shiny::validate(
        shiny::need(luc_file_data()$platemap, message = "No Data Available")
      )
  print(luc_file_data()$platemap)
  print(length(luc_file_data()$platemap))
  
      g <- get_plot_output_list(max_plots = length(luc_file_data()$platemap), platemapinfo = luc_file_data()$platemap, luc_file_xlsx()$plate_cols, luc_file_xlsx()$plate_rows)
      return(g)
})


###### Luciferase QC

# Select input for Plate to View
output$luc_qc_select_plate <- renderUI({ 
  shiny::validate(
    shiny::need(
      luc_file_data()$plot, message = "No Data Available"
    )
  )
  return(shiny::selectInput(inputId = "luc_qc_input_plate", label = "Select Plate" ,multiple = FALSE, choices = names(luc_file_data()$data)))
  
})

# FLUC Plot Overview

output$luc_qc_fluc_overview <- renderPlot({
  shiny::validate(
    shiny::need(input$luc_qc_input_plate, message = "Please select a plate")
  )
  
  data_use <- dplyr::filter(luc_file_data()$plot, Plate == input$luc_qc_input_plate)
  # if no treatment is specified -> set to NA
  data_use[is.na(data_use$Treatment),c("Treatment","FLUC","RLUC","Divided","Log2")] <- NA
  
  # Plate Overview of Samples
  platemap <- dplyr::mutate(data_use,
                            Row=as.numeric(match(toupper(substr(Well, 1, 1)), LETTERS)),
                            Column=as.numeric(substr(Well, 2, 5)))
  
  g <- ggplot2::ggplot(data=platemap, aes(x=Column, y=Row)) +
    geom_point(data=expand.grid(seq(1, luc_file_xlsx()$plate_cols), seq(1, luc_file_xlsx()$plate_rows)), aes(x=Var1, y=Var2),
               color="grey90", fill="white", shape=21, size=6) +
    geom_point(aes(color=FLUC), size=6) +
    coord_fixed(ratio=((luc_file_xlsx()$plate_cols+1)/luc_file_xlsx()$plate_cols)/((luc_file_xlsx()$plate_rows+1)/luc_file_xlsx()$plate_rows), xlim=c(0.5, luc_file_xlsx()$plate_cols+1), ylim=c(0.5, luc_file_xlsx()$plate_rows+1)) +
    scale_y_reverse(breaks=seq(1, luc_file_xlsx()$plate_rows), labels=LETTERS[1:luc_file_xlsx()$plate_rows]) +
    scale_x_continuous(breaks=seq(1, luc_file_xlsx()$plate_cols)) +
    #scale_shape_manual(values=seq(from=0, to = length(unique(platemap$value)), by=1 ) ) +
    labs(title=paste("Plate Layout:", "FLUC", input$luc_qc_input_plate, sep=" ") )
  
  print(g)
  
}, res = 72)


# RLUC Plot Overview
output$luc_qc_rluc_overview <- renderPlot({
  shiny::validate(
    shiny::need(input$luc_qc_input_plate, message = "Please select a plate")
  )
  
  data_use <- dplyr::filter(luc_file_data()$plot, Plate == input$luc_qc_input_plate)
  # if no treatment is specified -> set to NA
  data_use[is.na(data_use$Treatment),c("Treatment","FLUC","RLUC","Divided","Log2")] <- NA
  
  # Plate Overview of Samples
  platemap <- dplyr::mutate(data_use,
                            Row=as.numeric(match(toupper(substr(Well, 1, 1)), LETTERS)),
                            Column=as.numeric(substr(Well, 2, 5)))
  
  g <- ggplot2::ggplot(data=platemap, aes(x=Column, y=Row)) +
    geom_point(data=expand.grid(seq(1, luc_file_xlsx()$plate_cols), seq(1, luc_file_xlsx()$plate_rows)), aes(x=Var1, y=Var2),
               color="grey90", fill="white", shape=21, size=6) +
    geom_point(aes(color=RLUC), size=6) +
    coord_fixed(ratio=((luc_file_xlsx()$plate_cols+1)/luc_file_xlsx()$plate_cols)/((luc_file_xlsx()$plate_rows+1)/luc_file_xlsx()$plate_rows), xlim=c(0.5, luc_file_xlsx()$plate_cols+1), ylim=c(0.5, luc_file_xlsx()$plate_rows+1)) +
    scale_y_reverse(breaks=seq(1, luc_file_xlsx()$plate_rows), labels=LETTERS[1:luc_file_xlsx()$plate_rows]) +
    scale_x_continuous(breaks=seq(1, luc_file_xlsx()$plate_cols)) +
    #scale_shape_manual(values=seq(from=0, to = length(unique(platemap$value)), by=1 ) ) +
    labs(title=paste("Plate Layout:", "RLUC", input$luc_qc_input_plate, sep=" ") )
  
  print(g)
  
}, res = 72)

# Fluc/RLUC Overview
output$luc_qc_divided_overview <- renderPlot({
  shiny::validate(
    shiny::need(input$luc_qc_input_plate, message = "Please select a plate")
  )
  
  data_use <- dplyr::filter(luc_file_data()$plot, Plate == input$luc_qc_input_plate)
  # if no treatment is specified -> set to NA
  data_use[is.na(data_use$Treatment),c("Treatment","FLUC","RLUC","Divided","Log2")] <- NA
  
  # Plate Overview of Samples
  platemap <- dplyr::mutate(data_use,
                            Row=as.numeric(match(toupper(substr(Well, 1, 1)), LETTERS)),
                            Column=as.numeric(substr(Well, 2, 5)))
  
  g <- ggplot2::ggplot(data=platemap, aes(x=Column, y=Row)) +
    geom_point(data=expand.grid(seq(1, luc_file_xlsx()$plate_cols), seq(1, luc_file_xlsx()$plate_rows)), aes(x=Var1, y=Var2),
               color="grey90", fill="white", shape=21, size=6) +
    geom_point(aes(color=Divided), size=6) +
    coord_fixed(ratio=((luc_file_xlsx()$plate_cols+1)/luc_file_xlsx()$plate_cols)/((luc_file_xlsx()$plate_rows+1)/luc_file_xlsx()$plate_rows), xlim=c(0.5, luc_file_xlsx()$plate_cols+1), ylim=c(0.5, luc_file_xlsx()$plate_rows+1)) +
    scale_y_reverse(breaks=seq(1, luc_file_xlsx()$plate_rows), labels=LETTERS[1:luc_file_xlsx()$plate_rows]) +
    scale_x_continuous(breaks=seq(1, luc_file_xlsx()$plate_cols)) +
    #scale_shape_manual(values=seq(from=0, to = length(unique(platemap$value)), by=1 ) ) +
    labs(title=paste("Plate Layout:", "FLUC / RLUC", input$luc_qc_input_plate, sep=" ") )
  
  print(g)
  
}, res = 72)


## Datatable Output
output$luc_qc_datatable <- renderDataTable({
  shiny::validate(
    shiny::need(luc_file_data()$treatmentgroup, message = "No Data Available")
    
  )

  return(luc_file_data()$treatmentgroup)
  
  
})


## Analysis

output$luc_analysis_plot_rawdata_FLUC <- renderPlot({
  shiny::validate(
    shiny::need(luc_file_data()$plot, message = "No Data Available")
  )
  
  p <- plot_luc_analysis_plot_rawdata_FLUC(data = luc_file_data()$plot)
  
  print(p)
  
})


output$DL_luc_analysis_plot_rawdata_FLUC <- downloadHandler(
  filename = function() {
    paste('luc_analysis_plot_rawdata_FLUC', ".png", sep="")
  },
  content = function(con) {
    
    ggplot2::ggsave(filename = con, device= "png" , plot =  plot_luc_analysis_plot_rawdata_FLUC(data = luc_file_data()$plot), units="cm", height = 10, width=25)
  }
)


output$luc_analysis_plot_rawdata_RLUC <- renderPlot({
  shiny::validate(
    shiny::need(luc_file_data()$plot, message = "No Data Available")
  )
  
  p <- plot_luc_analysis_plot_rawdata_RLUC(data = luc_file_data()$plot)
  
  print(p)
  
})

output$luc_analysis_plot_rawdata_RATIO <- renderPlot({
  shiny::validate(
    shiny::need(luc_file_data()$plot, message = "No Data Available")
  )
  
  
  p <- plot_luc_analysis_plot_rawdata_RATIO(data = luc_file_data()$plot)
  
  print(p)
  
})


output$luc_analysis_plot_rawdata_LOG2RATIO <- renderPlot({
  shiny::validate(
    shiny::need(luc_file_data()$plot, message = "No Data Available")
  )
  
  p <- plot_luc_analysis_plot_rawdata_LOG2RATIO(data = luc_file_data()$plot)
  
  print(p)
  
})




########################
#####   Incucyte Assay
########################

















########################
#####   qPCR Assay
########################