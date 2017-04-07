# save as qpcr.R

######################################
## DATA UPLOAD
######################################


## XLSX FILE

output$qpcr_xlsx_filename <- renderText({
  shiny::validate(
    shiny::need(input$qpcr_xlsx, message = "No file uploaded yet.")
  )
  return(input$qpcr_xlsx$name)
})


qpcr_file_xlsx <- reactive({ #input$submit_qpcr_xlsx,
  qpcr_file <- input$qpcr_xlsx
  
  error$qpcr_xlsx = FALSE
  
  if(qpcr_file == "" || is.null(qpcr_file))
  {
    print("no qPCR File")
    return(NULL)
  } else
  {
    
    print("qPCR Overview upload")
    # make list to store informaiton
    out <- list()
    # read file
    workbook <- try(openxlsx::loadWorkbook(qpcr_file$datapath,xlsxFile = NULL))
    if(class(workbook) == "try-error")
    {
      print("TRY ERROR")
      error$qpcr_xlsx = TRUE
      shinyBS::toggleModal(session, "qpcr_xlsx_error_modal", toggle = "open")
      return(NULL)
    }
    
    wb_names <- names(workbook)
    
    # Get Overview
    xlsx_overview <- try(as.data.frame(openxlsx::read.xlsx(xlsxFile = qpcr_file$datapath, sheet = "qPCR Overview", colNames = TRUE, rowNames = TRUE), stringsAsFactors = FALSE))
    
    if(class(xlsx_overview) == "try-error")
    {
      print("Could not find qPCR Overview sheet. Please check the name!")
      error$qpcr_xlsx = TRUE
      shinyBS::toggleModal(session, "qpcr_xlsx_error_modal", toggle = "open")
      return(NULL)
    }
    
    # get Overview plate first -> stored in platemap
    
    overview_plate <- list()
    overview_plate$overview <- reshape2::melt(as.matrix(xlsx_overview))
    pattern <- expression("^(\\d{1})$")
    #overview_plate$overview$Var2 <- sub(pattern = pattern, replacement = paste("0","\\1", sep=""), x=  overview_plate$overview$Var2, fixed = FALSE )
    overview_plate$overview$position <- paste0(overview_plate$overview$Var1, overview_plate$overview$Var2)
    
    platemap <- dplyr::mutate(as.data.frame(overview_plate$overview),
                              Row=as.numeric(match(toupper(substr(position, 1, 1)), LETTERS)),
                              Column=as.numeric(substr(position, 2, 5)))
    
    df_samples <- NULL
    
    # check if robot was used and cDNA ans mastermix plate is available
    
    platemapMastermix <- NA
    platemapPrimer <- NA
    
    
    xlsx_primer <- try(openxlsx::read.xlsx(xlsxFile = qpcr_file$datapath, sheet = "cDNA", colNames = TRUE, rowNames = FALSE))
    xlsx_mastermix <- try(openxlsx::read.xlsx(xlsxFile = qpcr_file$datapath, sheet = "Mastermix", colNames = TRUE, rowNames = FALSE))
    
    
    if(class(xlsx_primer) == "try-error" || class(xlsx_mastermix) == "try-error" )
    {
      print("No cDNA or Mastermix sheet there")
      
      
      xlsx_mastermix <- try(as.data.frame(openxlsx::read.xlsx(xlsxFile = qpcr_file$datapath, sheet = "qPCR Samples", colNames = TRUE, rowNames = TRUE), stringsAsFactors = FALSE))
      
      if(class(xlsx_overview) == "try-error")
      {
        print("Could not find qPCR Overview sheet. Please check the name!")
        error$qpcr_xlsx = TRUE
        shinyBS::toggleModal(session, "qpcr_xlsx_error_modal", toggle = "open")
        return(NULL)
      }
      
      sample_plate <- list()
      sample_plate$overview <- reshape2::melt(as.matrix(xlsx_overview))
      pattern <- expression("^(\\d{1})$")
      #overview_plate$overview$Var2 <- sub(pattern = pattern, replacement = paste("0","\\1", sep=""), x=  overview_plate$overview$Var2, fixed = FALSE )
      sample_plate$overview$position <- paste0(sample_plate$overview$Var1, sample_plate$overview$Var2)
      
      platemapMastermix <- dplyr::mutate(as.data.frame(overview_plate$overview),
                                Row=as.numeric(match(toupper(substr(position, 1, 1)), LETTERS)),
                                Column=as.numeric(substr(position, 2, 5)))
      
      samplelist <- as.list(unique(sample_plate$overview$value))
      names(samplelist) <- unique(sample_plate$overview$value)
      
      
      df_samples <- tibble("Position" = sample_plate$overview$position,
                           "Type" = sample_plate$overview$value,
                           "Replicate" = sample_plate$overview$position
      )
      
      df_samples$SampleFinal <-  apply(df_samples,1, function(x) {
        sample <- sub(x = x[["Type"]], pattern = "(.*)_(.*)", replacement = "\\1")
        replicate = x[["Replicate"]]
        gene = sub(x = x[["Type"]], pattern = "(.*)_(.*)", replacement = "\\2")
        return(paste(sample,"-", gene, sep=""))
      })
      
      
    } else {
      
      samplelist <- as.list(unique(xlsx_mastermix$Sample))
      names(samplelist) <- unique(xlsx_mastermix$Sample)
      
      # name will stay old, but character will be the new name
      
      for(i in 1:length(samplelist))
      {
        xlsx_mastermix$Sample <- sub(pattern = names(samplelist)[i],replacement = samplelist[[i]],x = xlsx_mastermix$Sample)
        xlsx_primer$Sample <- sub(pattern = names(samplelist)[i],replacement = samplelist[[i]],x = xlsx_primer$Sample)
      }
      
      
      # Load more information and store it as platemap
      
      
      # load MASTERMIX sheet, which contains the Sample information and the layout where which sample is
      # Sample = Sample Name
      # Rack.Position = where to pipet from
      # q_PCR_Well_Pos = where to pipet to (final plate)
      #
      platemapMastermix <- as.data.frame(xlsx_mastermix)
      platemapMastermix <- dplyr::mutate(platemapMastermix,
                                         Row=as.numeric(match(toupper(substr(q_PCR_Well_Pos, 1, 1)), LETTERS)),
                                         Column=as.numeric(substr(q_PCR_Well_Pos, 2, 5)))
      
      # Primer Plate - tells us which target genes
      # Type = Target gene
      # Rack.Position = from where it takes the primer set
      # q_PCR_Plate = to where it pipettes (final plate)
      # Sample = Sample1 to Sample 4 -> same as in MasterMix Plate
      # Replicate = Which technical Replicate
      platemapPrimer <- dplyr::mutate(xlsx_primer,
                                      Row=as.numeric(match(toupper(substr(q_PCR_Well_Pos, 1, 1)), LETTERS)),
                                      Column=as.numeric(substr(q_PCR_Well_Pos, 2, 5)))
      
        df_samples <- tibble("Position" = xlsx_primer$q_PCR_Well_Pos,
                             "Type" = xlsx_primer$Type,
                             "Replicate" = xlsx_primer$Replicate
        )
        
        # add Sample information from xlsx_mastermix
        df_samples <- dplyr::left_join(x=df_samples, y = xlsx_mastermix, by=c("Position" = "q_PCR_Well_Pos"))
        
        df_samples$SampleFinal <-  apply(df_samples,1, function(x) {
          sample <- x[["Sample"]]
          replicate = x[["Replicate"]]
          gene = x[["Type"]]
          return(paste(sample,"-", gene, sep=""))
        })
        
      
      
    }
    
    
    ## AVAILABLE DATA STRUCTURES
    # file.read -> qPCR raw data
    # df.pcr -> flat-file qPCR rawdata in qpcR format
    # df.melting -> flat-file qPCR melting curve data (temp vs fluorescence)
    
    # Replace Sample with user names
    # Replace Sample by stoff from List that user gives
    
  
    # MAKE OUT
    
    out <- list(
      "overview_plate" = overview_plate$overview,
      "platemap" = platemap,
      "platemapMastermix" = platemapMastermix,
      "platemapPrimer" = platemapPrimer,
      "samplelist" = samplelist,
      "df_samples" = df_samples
    )
    return(out)
    
    
  }
  
  
})


## DATA FILE

output$qpcr_data_filename <- renderUI({
  shiny::validate(
    shiny::need(input$qpcr_data, message = "No files uploaded yet.")
  )
  return(HTML(paste(input$qpcr_data$name, collapse = "</br>")))
})


qpcr_file_data <- reactive({ #input$submit_qpcr_data, 
  shiny::validate(
    shiny::need(!is.null(qpcr_file_xlsx), message = "Please upload a valid XLSX file first"),
    shiny::need(qpcr_file_xlsx, message = "Please upload a valid XLSX file first")
  )
  
  error$qpcr_data_error <- FALSE
  
  if(is.null(input$qpcr_data) || input$qpcr_data =="")
  {return(NULL)}
  else
  {
    # read data file
    file.read <- try(readr::read_tsv(file = input$qpcr_data$datapath, col_names = c("SamplePos","SampleName","Prog","Seg","Cycle","Time","Temp","483-533"), skip = 2))
    
    if(class(file.read) == "try-error")
    {
      error$qpcr_data_error <- TRUE
      print("File not found or does not match Lightcycler Raw Data file structure.")
    }
    
    # convert to qpcR format flat file
    ## each row -> CYCLE
    ## each column -> Well
    df.pcr <- NULL
    ## make new tibble with maximum number of cycles
    df.pcr <- try(tibble::tibble("Cycles" = seq(from = min(file.read[,"Cycle"]), to = max(file.read[,"Cycle"]), by =1)))
    
    if(class(df.pcr) == "try-error")
    {
      error$qpcr_data_error <- TRUE
      print("File not found or does not match Lightcycler Raw Data file structure.")
      shinyBS::toggleModal(session, "qpcr_data_error_modal", toggle = "open")
      return(NULL)
    }
    
    ## Add names (unique ones of course) as columns, so we first make data frame for each Well that contains cycles 1-x and the 483-533 value
    wells <- NULL
    
    wells <- as.list(unique(file.read$SamplePos))
    names(wells) <- unique(file.read$SamplePos)
    
    # make df.data with all data information and put this to df.pcr
    df.data <- lapply(wells, function(x){
      # get subset
      subset <- dplyr::filter(file.read, SamplePos == as.character(x) & Prog == 2)  %>% dplyr::select( 5, 8)
      # Get column 483-533
      #subset <- dplyr::select(subset, 5, 8)
      colnames(subset) <- c("Cycles",as.character(x))
      # Add to tibble
      df.pcr <<- dplyr::left_join(df.pcr, subset, by = "Cycles")
      return(subset)
      
    })
    
    df.data <- NULL
    
    
    # create tibble with melting curve information
    ## get melting curve information (Prog == 3 in data)
    df.melting <- NULL
    df.melting <- tibble::tibble("Temp" = unique(file.read[file.read$Prog == 3, "Temp"]$Temp))
    if(nrow(df.melting) <= 10)
    {
      df.melting <- NULL
    }
    #t <- dplyr::filter(file.read, SamplePos == "A1" & Prog == 3)  %>% dplyr::select( 7, 8)
    #df.melting <<- dplyr::left_join(df.melting, t, by = "Temp")
    
    
    df.data <- lapply(wells, function(x){
      # get subset
      subset <- dplyr::filter(file.read, SamplePos == as.character(x) & Prog == 3)  %>% dplyr::select( 7, 8)
      # Get column 483-533
      #subset <- dplyr::select(subset, 5, 8)
      colnames(subset) <- c("Temp", as.character(x))
      # Add to tibble
      df.melting <<- dplyr::left_join(df.melting, subset, by = "Temp")
      return(subset)
      
    })
    
    
    # Return data structures
    out <- list(
      "success" = TRUE,
      "pcr" = df.pcr,
      "melting" = df.melting,
      "wells" = wells
    )
    shinyjs::enable("qpcr_start_calculation")
    
    return(out)
    
  }
  
  
})


# Observe
observe(qpcr_file_xlsx())
observe(qpcr_file_data())



##############################
########### CQ CALCULCATION ###
##############################


 qpcr_data <- eventReactive(input$qpcr_start_calculation, ignoreNULL = FALSE, {
  shiny::validate(
    shiny::need(input$qpcr_normalize_method, "Please select a normaliation method"),
    shiny::need(input$qpcr_cq_method, "Please select a Cq calculation method"),
    shiny::need(qpcr_file_xlsx, "Please upload XLSX file"),
    shiny::need(qpcr_file_data, "Please upload data file"),
    shiny::need(input$minCT, "Please select CQ thresholds"),
    shiny::need(input$maxCT, "Please select CQ thresholds"),
    shiny::need(input$qpcr_input_control, "Please select a control")
  )
  
  
  
  if(qpcr_file_data()$success)
  {
    
    # Disble button
    shinyjs::disable("qpcr_start_calculation")
    shinyjs::enable("qpcr_reset_calculation")
    
    print(input$qpcr_cq_method)
    # Calculate CQ
    cq <- calculate_cq(df.pcr = qpcr_file_data()$pcr, cq_calc_method = input$qpcr_cq_method)
    
    
    
    # write calc list wiht all cq values to a tibble for htqpcr
    cq <- lapply(calc,function(x){
      return(x[["cq"]])
    })
    cq_df <- as.data.frame(t(as.data.frame(cq)))
    cq_df$V1 <- as.numeric(cq_df$V1)
    
    
    
    # FLAG Cq falues as NA for weird values
    # Input: maxCT and minCT for maximum and minum threshold. Everything above/below is set to NA and will be marked as FLAGGED
    
    
    cq_df$V1[cq_df$V1 >= as.numeric(input$maxCT)] <- NA
    cq_df$V1[cq_df$V1 <= as.numeric(input$minCT)] <- NA
    
    # Flag
    cq_df$Flag[is.na(cq_df$V1)] <- "undetermined"
    cq_df$Flag[!is.na(cq_df$V1)] <- "Passed"
    
    cq_df$Position <- rownames(cq_df)
    cq_df <- tibble::as_tibble(cq_df)
    colnames(cq_df) <- c("Cq", "Flag", "Position")
    
    # make another df_samples
    df_samples <- dplyr::left_join(x=qpcr_file_xlsx()$df_samples, y=cq_df, by="Position")
    
    df_samples$Control <- df_samples$Type
    df_samples$Control[df_samples$Control %in% input$qpcr_input_control] <- "Endogenous Control"
    df_samples$Control[df_samples$Control %in% input$qpcr_input_poscontrol] <- "Positive Control"
    df_samples$Control[df_samples$Control %in% input$qpcr_input_negcontrol] <- "Negative Control"
    df_samples$Control[!(df_samples$Control %in% c("Endogenous Control", "Positive Control", "Negative Control") )] <- "Target"
    
    # raw Cq data output for htqpcr
    output_cqdata <- df_samples[, c("Position", "Type", "Sample", "Cq", "Control", "Flag")]
    
    n_samples <- unique(output_cqdata$Sample)
    n_samples <- n_samples[!is.na(n_samples)]
    n_samples <- tibble::tibble("Files" = n_samples, "Treatment" = n_samples)
    
    n_features <- nrow(dplyr::filter(df_samples, Sample == n_samples$Files[1]) %>% dplyr::select(Position))
    # 
    for(i in 1:length(n_samples$Files))
    {
      
      fullplate_df <- NULL
      pos1 <- dplyr::filter(df_samples, Sample == n_samples$Treatment[i]) %>% dplyr::select( Position)
      fullplate_df <- tibble("Position" = pos1$Position)
      fullplate_df <- dplyr::left_join(fullplate_df, dplyr::filter(output_cqdata, Sample == n_samples$Treatment[i]))
      #fullplate_df <- dplyr::filter(output_cqdata, Sample == n_samples[i]) %>% dplyr::arrange(Type)
      pos <- dplyr::filter(df_samples, Sample == n_samples$Treatment[1]) %>% dplyr::select( Position)
      fullplate_df$Position <- pos$Position
      # fullplate_df <- dplyr::full_join(fullplate_df, dplyr::select(df_samples, Position), by= "Position")
      fullplate_df <- dplyr::arrange(fullplate_df,Type)
      #readr::write_tsv(x = dplyr::filter(output_cqdata, Sample == n_samples[i]), path = file.path(getwd(), n_samples[i]), col_names = FALSE)
      readr::write_tsv(x = fullplate_df, path = file.path(config$userDir, n_samples$Treatment[i]), col_names = FALSE, append = FALSE)
    }
    
    # config$userDir
    
    # read back in for htqpcr as qcprdataset
    qPCRraw <- HTqPCR::readCtData(files = n_samples$Files, path=config$userDir, column.info = list("position" = 1,"feature" = 2,"Ct" = 4, "type"=5, "flag" = 6), n.features = n_features)
    # 
    
    ## FLAG and FILTER
    qPCRraw <- setCategory(qPCRraw, Ct.max = as.numeric(input$maxCT), Ct.min = as.numeric(input$minCT), groups=NULL, replicates = FALSE, flag = TRUE, flag.out = "Failed", verbose = TRUE)
    
    # Normalize
    qPCRnorm <- normalizeCtData(qPCRraw, norm = input$qpcr_normalize_method, Ct.max = as.numeric(input$maxCT), deltaCt.genes = input$qpcr_input_refgenes)
    
    # make tidy data
    
    qpcr_tidy <- qpcr_tidy_calc(qPCRnormobject = qPCRnorm, qPCRrawobject = qPCRraw)
    
    ###### data structures to use
    # qPCRraw <- rawdata qPCR for HTqPCR
    # qPCRnorm <- normalized qPCR for HTqPCR
    # qpcr_tidy <- data in tidy format
    
    out <- list(
      "qPCRraw" = qPCRraw,
      "qPCRnorm" = qPCRnorm,
      "tidy" = qpcr_tidy
    )
    
    shinyBS::toggleModal(session, "qpcr_data_calc_modal", toggle = "open")
    
    
    
  } else {
    out <- list(
      "qPCRraw" = NA,
      "qPCRnorm" = NA,
      "tidy" = NA
    )
    shinyBS::toggleModal(session, "qpcr_data_error_modal", toggle = "open")
    
  }
   return(out)
})

observe(qpcr_data())

observeEvent(input$qpcr_reset_calculation, {
  
  shinyjs::enable("qpcr_start_calculation")
  shinyjs::disable("qpcr_reset_calculation")
  
})





###############################
########### PLOTS #############
###############################


## Overview Plates (QC)

## from qpcr_file_xlsx
#"overview_plate"
#"platemap"
#"platemapMastermix"
#"platemapPrimer"
#"samplelist" 
#"df_samples" 

output$qpcr_overview_genes <- renderPlot({
  shiny::validate(
    shiny::need(qpcr_file_xlsx, "Please upload data.")
  )
  
  if(is.null(qpcr_file_xlsx()$overview_plate) )
  {
    return("Overview data not available.")
  }
  
  p <- plot_platemap_overview(data=qpcr_file_xlsx()$platemap)
  return(p)
})

# this plot only is shown if robot i used
output$qpcr_overview_samples <- renderPlot({
  shiny::validate(
    shiny::need(qpcr_file_xlsx, "Please upload data.")
  )
  
  if(is.null(qpcr_file_xlsx()$platemapMastermix) )
  {
    return("Overview data not available.")
  }
  
  p <- plot_target_overview(data = qpcr_file_xlsx()$platemapMastermix, title = "Plate Layout: Samples", save=FALSE, samples=TRUE)
  
  return(p)
})

# this plot is only used if robot is used
output$qpcr_overview_target <- renderPlot({
  shiny::validate(
    shiny::need(qpcr_file_xlsx, "Please upload data.")
  )
  
  if(is.null(qpcr_file_xlsx()$platemapPrimer) )
  {
    return("Overview data not available.")
  }
  
  p <- plot_target_overview(data = qpcr_file_xlsx()$platemapPrimer, title = "Plate Layout: Target", save=FALSE, samples=FALSE)
  
  return(p)
})

## Overview CQ

output$qpcr_qc_cqraw <- renderPlot({
  shiny::validate(
    shiny::need(!is.na(qpcr_data()$tidy), "Please upload data."),
    shiny::need(input$input_qc_qpcr_sample, "Please select a sample")#,
    #shiny::need(input$qpcr_input_refgenes,"Please select reference genes")
  )
  
  if(is.na(qpcr_data()$tidy) )
  {
    return("Overview data not available.")
  }
  
  p <- plot_cq_overview(data=qpcr_data()$tidy, norm = FALSE, sample=input$input_qc_qpcr_sample, refgenes="")
  
  return(p)
})

output$DL_qpcr_qc_cqraw <- downloadHandler(
  filename = function() {
    paste('qPCR_QC_RAW_Cq_',paste(input$input_qc_qpcr_sample, collapse = "-" ), ".png", sep="")
  },
  content = function(con) {
    ggplot2::ggsave(filename = con, device= "png" , plot =  plot_cq_overview(data=qpcr_data()$tidy, norm = FALSE, sample=input$input_qc_qpcr_sample, refgenes=""), units="cm", height = 10, width=25, dpi = 600)
  }
)

output$qpcr_qc_cqnorm <- renderPlot({
  shiny::validate(
    shiny::need(!is.na(qpcr_data()$tidy), "Please upload data."),
    shiny::need(input$input_qc_qpcr_sample, "Please select a sample"),
    shiny::need(input$qpcr_input_refgenes,"Please select reference genes")
  )
  
  if(is.na(qpcr_data()$tidy) )
  {
    return("Overview data not available.")
  }
  
  p <- plot_cq_overview(data=qpcr_data()$tidy, norm = TRUE, sample=input$input_qc_qpcr_sample, refgenes=input$qpcr_input_refgenes)
  
  return(p)
})

output$DL_qpcr_qc_cqnorm <- downloadHandler(
  filename = function() {
    paste('qPCR_QC_RAW_ddCq_',paste(input$input_qc_qpcr_sample, collapse = "-" ),"-", paste(input$qpcr_input_refgenes, collapse = "-" ) , ".png", sep="")
  },
  content = function(con) {
    ggplot2::ggsave(filename = con, device= "png" , plot =  plot_cq_overview(data=qpcr_data()$tidy, norm = TRUE, sample=input$input_qc_qpcr_sample, refgenes=input$qpcr_input_refgenes), units="cm", height = 10, width=25, dpi = 600)
  }
)

######### QC detailed


## normalization plot
output$qpcr_qc_plot_normalization <- renderPlot({
  shiny::validate(
    shiny::need(!is.na(qpcr_data()$tidy), "Please upload data."),
    shiny::need(!is.na(qpcr_data()$qPCRraw), "Please run the calculation"),
    shiny::need(!is.na(qpcr_data()$qPCRnorm), "Please run the calculation"),
    shiny::need(input$qpcr_input_refgenes,"Please select reference genes")
  )
  
  if(is.na(qpcr_data()$tidy) )
  {
    return("Overview data not available.")
  }
  
  plot_qpcr_normalization(qPCRraw=qpcr_data()$qPCRraw, qPCRnorm=qpcr_data()$qPCRnorm, refgenes=input$qpcr_input_refgenes, title=NULL, xlab ="Cq before normalization", ylab="ddCT normalized Cq")
  
})

output$DL_qpcr_qc_plot_normalization <- downloadHandler(
  filename = function() {
    paste('qPCR_QC_Normalization_',"-", paste(input$qpcr_input_refgenes, collapse = "-" ) , ".png", sep="")
  },
  content = function(con) {
    png(filename = con,units = "cm", width = 8, height = 8, res = 1200, pointsize = 4)
    plot_qpcr_normalization(qPCRraw=qpcr_data()$qPCRraw, qPCRnorm=qpcr_data()$qPCRnorm, refgenes=input$qpcr_input_refgenes, title=NULL, xlab ="Cq before normalization", ylab="ddCT normalized Cq")
    dev.off()
  }
)

## Standard Deviation of different Samples

output$qpcr_qc_plot_variation <- renderPlot({
  shiny::validate(
    shiny::need(!is.na(qpcr_data()$tidy), "Please upload data."),
    shiny::need(!is.na(qpcr_data()$qPCRraw), "Please run the calculation"),
    shiny::need(!is.na(qpcr_data()$qPCRnorm), "Please run the calculation"),
    shiny::need(input$qpcr_input_refgenes,"Please select reference genes")
  )
  
  if(is.na(qpcr_data()$tidy) )
  {
    return("Overview data not available.")
  }
  
  plot_qpcr_variation(qPCRraw=qpcr_data()$qPCRraw)
  
})

output$DL_qpcr_qc_plot_variation <- downloadHandler(
  filename = function() {
    paste('qPCR_QC_Normalization_', paste(input$qpcr_input_refgenes, collapse = "-" ) , ".png", sep="")
  },
  content = function(con) {
    png(filename = con,units = "cm", width = 12, height = 8, res = 1200, pointsize = 4)
    plot_qpcr_variation(qPCRraw=qpcr_data()$qPCRraw)
    dev.off()
  }
)

## Target gene quality

output$qpcr_qc_plot_determined <- renderPlot({
  shiny::validate(
    shiny::need(!is.na(qpcr_data()$tidy), "Please upload data."),
    shiny::need(!is.na(qpcr_data()$qPCRraw), "Please run the calculation"),
    shiny::need(!is.na(qpcr_data()$qPCRnorm), "Please run the calculation"),
    shiny::need(input$qpcr_input_refgenes,"Please select reference genes")
  )
  
  
  plot_qpcr_determined(qPCRnorm=qpcr_data()$qPCRnorm, title="Status of qPCR wells", ylab ="Number of Wells")
  
})

output$DL_qpcr_qc_plot_determined <- downloadHandler(
  filename = function() {
    paste('qPCR_QC_TargetGeneQuality_', paste(input$qpcr_input_refgenes, collapse = "-" ) , ".png", sep="")
  },
  content = function(con) {
    png(filename = con,units = "cm", width = 8, height = 8, res = 1200, pointsize = 4)
    plot_qpcr_determined(qPCRnorm=qpcr_data()$qPCRnorm, title="Status of qPCR wells", ylab ="Number of Wells")
    dev.off()
  }
)


## Density

output$qpcr_qc_plot_density <- renderPlot({
  shiny::validate(
    shiny::need(!is.na(qpcr_data()$tidy), "Please upload data."),
    shiny::need(!is.na(qpcr_data()$qPCRraw), "Please run the calculation"),
    shiny::need(!is.na(qpcr_data()$qPCRnorm), "Please run the calculation"),
    shiny::need(input$qpcr_input_refgenes,"Please select reference genes")
  )
  
  if(is.na(qpcr_data()$tidy) )
  {
    return("Overview data not available.")
  }
  
  plot_qpcr_density(qPCRraw = qpcr_data()$qPCRraw, qPCRnorm=qpcr_data()$qPCRnorm, refgenes = input$qpcr_input_refgenes, maxCT = input$maxCT, minCT = input$minCT)
  
})

output$DL_qpcr_qc_plot_density <- downloadHandler(
  filename = function() {
    paste('qPCR_QC_Density' , ".png", sep="")
  },
  content = function(con) {
    png(filename = con,units = "cm", width = 12, height = 8, res = 1200, pointsize = 4)
    plot_qpcr_density(qPCRraw = qpcr_data()$qPCRraw, qPCRnorm=qpcr_data()$qPCRnorm, refgenes = input$qpcr_input_refgenes, maxCT = input$maxCT, minCT = input$minCT)
    dev.off()
  }
)

## Boxes

output$qpcr_qc_plot_boxes <- renderPlot({
  shiny::validate(
    shiny::need(!is.na(qpcr_data()$tidy), "Please upload data."),
    shiny::need(!is.na(qpcr_data()$qPCRraw), "Please run the calculation"),
    shiny::need(!is.na(qpcr_data()$qPCRnorm), "Please run the calculation"),
    shiny::need(input$qpcr_input_refgenes,"Please select reference genes")
  )
  
  
  plot_qpcr_boxplot_qc(qPCRraw = qpcr_data()$qPCRraw, qPCRnorm=qpcr_data()$qPCRnorm, refgenes = input$qpcr_input_refgenes, maxCT = input$maxCT, minCT = input$minCT)
  
})

output$DL_qpcr_qc_plot_boxes <- downloadHandler(
  filename = function() {
    paste('qPCR_QC_Density' , ".png", sep="")
  },
  content = function(con) {
    png(filename = con,units = "cm", width = 12, height = 8, res = 1200, pointsize = 4)
    plot_qpcr_boxplot_qc(qPCRraw = qpcr_data()$qPCRraw, qPCRnorm=qpcr_data()$qPCRnorm, refgenes = input$qpcr_input_refgenes, maxCT = input$maxCT, minCT = input$minCT)
    dev.off()
  }
)


## PAIRS
output$qpcr_qc_plot_pairs <- renderPlot({
  shiny::validate(
    shiny::need(!is.na(qpcr_data()$qPCRraw), "Please run the calculation"),
    shiny::need(!is.na(qpcr_data()$qPCRnorm), "Please run the calculation"),
  )
  
  
  plot_qpcr_qc_pairs(qPCRraw = qpcr_data()$qPCRraw, qPCRnorm=qpcr_data()$qPCRnorm, title="qPCR Sample Correlation", maxCT = input$maxCT)
  
})

output$DL_qpcr_qc_plot_pairs <- downloadHandler(
  filename = function() {
    paste('qPCR_QC_Density' , ".png", sep="")
  },
  content = function(con) {
    png(filename = con,units = "cm", width = 12, height = 8, res = 1200, pointsize = 4)
    plot_qpcr_qc_pairs(qPCRraw = qpcr_data()$qPCRraw, qPCRnorm=qpcr_data()$qPCRnorm, title="qPCR Sample Correlation", maxCT = input$maxCT)
    dev.off()
  }
)


######### INDIVIDUAL PLOTS





################################
##### DATA TABLES
################################



## Datatable Output
output$qpcr_xlsx_rawdata <- renderDataTable({
  shiny::validate(
    shiny::need(qpcr_file_xlsx()$df_samples, message = "No Data Available")
  )
  
  filename <- paste("qPCR_Rawdata")
  opts <- list( dom = "Bflrtip",
                lengthMenu = list(c(5, 15, 50, 100, -1), c('5', '15', '50', '100', 'All')), 
                pageLength = 15, scrollX = FALSE)
  opts[["order"]] <- NULL
  opts[["buttons"]] <- list("copy","print", list("extend" = 'csv', "text"='csv', "filename" = filename, "title" = filename), list("extend" = 'excel', "text"='Excel', "filename" = filename, "title" = filename), list("extend" = 'pdf', "text"='pdf', "filename" = filename, "title" = filename))#buttons
  
  ext <- character(0)
  ext <- c(ext, "Buttons")
  ext <- c(ext, "Responsive")
  
  # data
  
  return(DT::datatable(qpcr_file_xlsx()$df_samples, style = "default", class = "display",  options = opts, extensions = ext))
  
  
})


## Tody normalized data
output$qpcr_tidydata <- renderDataTable({
  shiny::validate(
    shiny::need(qpcr_data()$tidy, message = "No Data Available")
  )
  
  filename <- paste("qPCR_data_tidy")
  opts <- list( dom = "Bflrtip",
                lengthMenu = list(c(5, 15, 50, 100, -1), c('5', '15', '50', '100', 'All')), 
                pageLength = 15, scrollX = FALSE)
  opts[["order"]] <- NULL
  opts[["buttons"]] <- list("copy","print", list("extend" = 'csv', "text"='csv', "filename" = filename, "title" = filename), list("extend" = 'excel', "text"='Excel', "filename" = filename, "title" = filename), list("extend" = 'pdf', "text"='pdf', "filename" = filename, "title" = filename))#buttons
  
  ext <- character(0)
  ext <- c(ext, "Buttons")
  ext <- c(ext, "Responsive")
  
  # data
  
  return(DT::datatable(qpcr_data()$tidy, style = "default", class = "display",  options = opts, extensions = ext))
  
  
})




################################
##### INPUTS
################################



output$qpcr_refgenes <- renderUI({
  shiny::validate(
    shiny::need(qpcr_file_xlsx, "Please upload the XLSX overview file")
  )
  
  genes <- unique(qpcr_file_xlsx()$df_samples$Type)
  
  
  return(shiny::selectizeInput(inputId = "qpcr_input_refgenes", label = "Select the Reference Genes for deltaCT normalization" , multiple = TRUE, options = list(maxItems = 50), choices = genes, width = "30%") )
  
})


output$qpcr_control <- renderUI({
  shiny::validate(
    shiny::need(qpcr_file_xlsx, "Please upload the XLSX overview file")
  )
  
  genes <- unique(qpcr_file_xlsx()$df_samples$Type)
  
  
  return(shiny::selectizeInput(inputId = "qpcr_input_control", label = "Select the endogeneous control genes" , multiple = TRUE, options = list(maxItems = 50), choices = genes, width = "30%") )
  
})

output$qpcr_poscontrol <- renderUI({
  shiny::validate(
    shiny::need(qpcr_file_xlsx, "Please upload the XLSX overview file")
  )
  
  genes <- unique(qpcr_file_xlsx()$df_samples$Type)
  
  
  return(shiny::selectizeInput(inputId = "qpcr_input_poscontrol", label = "Select the positive control genes" , multiple = TRUE, options = list(maxItems = 50), choices = genes, width = "30%") )
  
})

output$qpcr_negcontrol <- renderUI({
  shiny::validate(
    shiny::need(qpcr_file_xlsx, "Please upload the XLSX overview file")
  )
  
  genes <- unique(qpcr_file_xlsx()$df_samples$Type)
  
  
  return(shiny::selectizeInput(inputId = "qpcr_input_negcontrol", label = "Select the negative control genes" , multiple = TRUE, options = list(maxItems = 50), choices = genes, width = "30%") )
  
})

output$qc_qpcr_sample <- renderUI({
  shiny::validate(
    shiny::need(!is.na(qpcr_data()$tidy), "Please perform the calculation first")
  )
  
  genes <- unique(qpcr_data()$tidy$Sample)
  
  
  return(shiny::selectizeInput(inputId = "input_qc_qpcr_sample", label = "Select Sample" , multiple = FALSE, choices = genes, width = "30%") )
  
})











