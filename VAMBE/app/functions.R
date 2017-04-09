# saved as functions.R




####################################
#### LUCIFERASE
####################################





plot_luc_analysis_plot_rawdata_FLUC <- function(data = luc_file_data()$plot)
{
  p <- ggplot(data, aes(x=Treatment, y=FLUC, fill=Treatment)) + 
    geom_boxplot() +
    geom_jitter(shape=16, position=position_jitter(0.2)) +
    labs(title = "FLUC", x = "Samples", y = "FLUC") +
    ggplot2::theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = rel(1.1)), legend.position="none")
  return(p)
}

plot_luc_analysis_plot_rawdata_FLUC_plate <- function(data = luc_file_data()$plot)
{
  p <- ggplot(data, aes(x=Plate, y=FLUC, fill=Treatment)) + 
    geom_boxplot() +
    #geom_jitter(shape=16, position=position_jitter(0.2)) +
    labs(title = "FLUC", x = "Samples", y = "FLUC") +
    ggplot2::theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = rel(1.1)), legend.position="bottom")
  return(p)
}


plot_luc_analysis_plot_rawdata_RLUC <- function(data = luc_file_data()$plot)
{
  p <- ggplot(data, aes(x=Treatment, y=RLUC, fill=Treatment)) + 
    geom_boxplot() +
    geom_jitter(shape=16, position=position_jitter(0.2)) +
    labs(title = "RLUC", x = "Samples", y = "RLUC") +
    ggplot2::theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = rel(1.1)), legend.position="none")
  return(p)
}

plot_luc_analysis_plot_rawdata_RLUC_plate <- function(data = luc_file_data()$plot)
{
  p <- ggplot(data, aes(x=Plate, y=RLUC, fill=Treatment)) + 
    geom_boxplot() +
    #geom_jitter(shape=16, position=position_jitter(0.2)) +
    labs(title = "RLUC", x = "Samples", y = "RLUC") +
    ggplot2::theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = rel(1.1)), legend.position="bottom")
  return(p)
}

plot_luc_analysis_plot_rawdata_RATIO <- function(data = luc_file_data()$plot)
{
  p <- ggplot(data, aes(x=Treatment, y=Divided, fill=Treatment)) + 
    geom_boxplot() +
    geom_jitter(shape=16, position=position_jitter(0.2)) +
    labs(title = "FLUC normalized by RLUC", x = "Samples", y = "Ratio FLUC/RLUC") +
    ggplot2::theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = rel(1.1)), legend.position="none")
  return(p)
}

plot_luc_analysis_plot_rawdata_RATIO_plate <- function(data = luc_file_data()$plot)
{
  p <- ggplot(data, aes(x=Plate, y=Divided, fill=Treatment)) + 
    geom_boxplot() +
    #geom_jitter(shape=16, position=position_jitter(0.2)) +
    labs(title = "FLUC normalized by RLUC", x = "Samples", y = "Ratio FLUC/RLUC") +
    ggplot2::theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = rel(1.1)), legend.position="bottom")
  return(p)
}

plot_luc_analysis_plot_rawdata_LOG2RATIO <- function(data = luc_file_data()$plot)
{
  p <- ggplot(data, aes(x=Treatment, y=Log2, fill=Treatment)) + 
    geom_boxplot() +
    geom_jitter(shape=16, position=position_jitter(0.2)) +
    ggplot2::theme_minimal() +
    labs(title = "Log2 FLUC normalized by RLUC", x = "Samples", y = "Log2 Ratio FLUC/RLUC") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = rel(1.1)), legend.position="none")
  return(p)
}


plot_luc_analysis_plot_individual_FLUC <- function(data = luc_file_data()$plot, groups = NULL, calibrator = NULL, normalize = "none")
{
    if(!is.null(groups))
    {
      
      if(!is.null(calibrator))
      {
        
        # calibrate to sample
        # divide FLUC by the calibrator
        data$FLUC <- data$FLUC/median(dplyr::filter(data, Treatment %in% calibrator)$FLUC, na.rm = TRUE)
        data_cal <- dplyr::filter(data, Treatment %in% calibrator)
        data <- dplyr::filter(data, Treatment %in% groups)
        data <- dplyr::bind_rows(data, data_cal)
        title  = paste("FLUC - Data calibrated to ", calibrator, sep="")
        xlab = "Samples"
        ylab = "Fold Change FLUC"

      } else {
        data <- dplyr::filter(data, Treatment %in% groups)
        title  = "FLUC"
        xlab = "Samples"
        ylab = "FLUC"
      }
      
      p <- ggplot(data, aes(x=Treatment, y=FLUC, fill=Treatment)) + 
        geom_boxplot() +
        geom_jitter(shape=16, position=position_jitter(0.2)) +
        labs(title = title, x = xlab, y = ylab) +
        ggplot2::theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = rel(1.1)), legend.position="none")
      return(p)
      
    } else {
      return("No Groups Selected")
    }
 
 
}

plot_luc_analysis_plot_individual_RLUC <- function(data = luc_file_data()$plot, groups = NULL, calibrator = NULL, normalize = "none")
{

    if(!is.null(groups))
    {
      
      
      if(!is.null(calibrator))
      {
        
        # calibrate to sample
        # divide RLUC by the calibrator
        data$RLUC <- data$RLUC/median(dplyr::filter(data, Treatment %in% calibrator)$RLUC, na.rm = TRUE)
        data_cal <- dplyr::filter(data, Treatment %in% calibrator)
        data <- dplyr::filter(data, Treatment %in% groups)
        data <- dplyr::bind_rows(data, data_cal)
        
        title  = paste("RLUC - Data calibrated to ", calibrator, sep="")
        xlab = "Samples"
        ylab = "Fold Change RLUC"
        
      } else {
        data <- dplyr::filter(data, Treatment %in% groups)
        title  = "RLUC"
        xlab = "Samples"
        ylab = "RLUC"
      }
      
      p <- ggplot(data, aes(x=Treatment, y=RLUC, fill=Treatment)) + 
        geom_boxplot() +
        geom_jitter(shape=16, position=position_jitter(0.2)) +
        labs(title = title, x = xlab, y = ylab) +
        ggplot2::theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = rel(1.1)), legend.position="none")
      return(p)
     
    } else {
      return("No Groups Selected")
    }
  
 
  
}

plot_luc_analysis_plot_individual_RATIO <- function(data = luc_file_data()$plot, groups = NULL, calibrator = NULL, normalize = "none")
{
 
    if(!is.null(groups))
    {
      
      if(!is.null(calibrator))
      {
        
        # calibrate to sample
        # divide RATIO by the calibrator
        data$Divided <- data$Divided/median(dplyr::filter(data, Treatment %in% calibrator)$Divided, na.rm = TRUE)
        data_cal <- dplyr::filter(data, Treatment %in% calibrator)
        data <- dplyr::filter(data, Treatment %in% groups)
        data <- dplyr::bind_rows(data, data_cal)
        
        title  = paste("Ratio - Data calibrated to ", calibrator, sep="")
        xlab = "Samples"
        ylab = "Fold Change Ratio FLUC/RLUC"
        
      } else {
        data <- dplyr::filter(data, Treatment %in% groups)
        title  = "Ratio FLUC/RLUC"
        xlab = "Samples"
        ylab = "Ratio FLUC/RLUC"
      }
      
      p <- ggplot(data, aes(x=Treatment, y=Divided, fill=Treatment)) + 
        geom_boxplot() +
        geom_jitter(shape=16, position=position_jitter(0.2)) +
        labs(title = title, x = xlab, y = ylab) +
        ggplot2::theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = rel(1.1)), legend.position="none")
      return(p)
     
    } else {
      return("No Groups Selected")
    }

}

plot_luc_analysis_plot_individual_LOG2RATIO <- function(data = luc_file_data()$plot, groups = NULL, calibrator = NULL, normalize = "none")
{
    if(!is.null(groups))
    {
      
      
      
      if(!is.null(calibrator))
      {
        # calibrate to sample
        # divide FLUC by the calibrator
        data$Divided <- data$Divided/median(dplyr::filter(data, Treatment %in% calibrator)$Divided, na.rm = TRUE)
        data$Log2 <- log2(data$Divided)
        data_cal <- dplyr::filter(data, Treatment %in% calibrator)
        data <- dplyr::filter(data, Treatment %in% groups)
        data <- dplyr::bind_rows(data, data_cal)
        
        title  = paste("Log2 FLUC normalized by RLUC - Data calibrated to ", calibrator, sep="")
        xlab = "Samples"
        ylab = "Fold Change Log2 Ratio FLUC/RLUC"
        
      } else {
        data <- dplyr::filter(data, Treatment %in% groups)
        title  = "Log2 Ratio FLUC/RLUC"
        xlab = "Samples"
        ylab = "Log2 Ratio FLUC/RLUC"
      }
      
      p <- ggplot(data, aes(x=Treatment, y=Log2, fill=Treatment)) + 
        geom_boxplot() +
        geom_jitter(shape=16, position=position_jitter(0.2)) +
        labs(title = title, x = xlab, y = ylab) +
        ggplot2::theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = rel(1.1)), legend.position="none")
      return(p)
      
      
    } else {
      return("No Groups Selected")
    }

}


###############################
## QPCR
###############################

## GGPLOT2 Overview Samples
plot_platemap_overview <- function(data=NA){
  g <- ggplot2::ggplot(data=data, aes(x=Column, y=Row)) +
    geom_point(data=expand.grid(seq(1, 24), seq(1, 16)), aes(x=Var1, y=Var2),
               color="grey90", fill="white", shape=21, size=6) +
    geom_point(aes(color=value), size=6) +
    coord_fixed(ratio=(26/24)/(18/16), xlim=c(0.5, 25), ylim=c(0.5, 17)) +
    scale_y_reverse(breaks=seq(1, 16), labels=LETTERS[1:16]) +
    scale_x_continuous(breaks=seq(1, 24)) +
    scale_shape_manual(values=seq(from=0, to = length(unique(platemap$value)), by=1 ) ) +
    labs(title="Overview plate") 
  return(g)
}

## OVERVIEW TARGET GENES
plot_target_overview <- function(data=NULL, title="Plate Layout: Target Genes", save=FALSE, samples=FALSE)
{
  if(!is.na(data))
  {
    g <- ggplot(data=data, aes(x=Column, y=Row)) +
      geom_point(data=expand.grid(seq(1, 24), seq(1, 16)), aes(x=Var1, y=Var2),
                 color="grey90", fill="white", shape=21, size=6) 
    if(samples)
    {
      g <- g +  geom_point(aes(colour=Sample), size=6) 
    } else {
      g <- g + geom_point(aes(colour=Type), size=6) 
    }
    
    g <- g + coord_fixed(ratio=(13/12)/(9/8), xlim=c(0.5, 24.5), ylim=c(0.5, 16.5)) +
      scale_y_reverse(breaks=seq(1, 16), labels=LETTERS[1:16]) +
      scale_x_continuous(breaks=seq(1, 24)) +
      labs(title=title)
    return(g)
  } else {return(NULL)}
  
}

## OVERVIEW CQ RAW AND NORMALIZED

plot_cq_overview <- function(data=NA, norm = FALSE, sample=NULL, refgenes=""){
  data <- as.data.frame(data)
  if(norm)
  {
    selectcol <- "Cqnorm"
    title <- paste("ddCq values normalized to", paste(refgenes, collapse = ","), "for ", sample, sep= " ")
    
    low = "green"
    high = "red"
    
    
  } else
  {
    selectcol <- "Cqraw"
    title <- paste("Raw Cq values", "for", sample, sep= " ")
    
    low = "lightblue"
    high = "darkblue"
  }
  
  if(is.null(sample))
  {
    sampleselect <- 1
  } else
  {
    sampleselect <- sample
  }
  data <- dplyr::filter(data, Sample == sampleselect)

  g <- ggplot2::ggplot(data=data, aes(x=Column, y=Row)) +
    geom_point(data=expand.grid(seq(1, max(data$Column)), seq(1, max(data$Row))),aes(x=Var1, y=Var2),
               color="grey90", fill="white", shape=21, size=9) +
    geom_point(aes_string(color=selectcol), size=9) +
    scale_colour_gradient(low=low, high=high) +
    geom_text(aes_string(label = selectcol), size=2) +
    coord_fixed(ratio=((max(data$Column)+2)/max(data$Column))/((max(data$Row)+2)/max(data$Row)), xlim=c(0.5, (max(data$Column)+1) ), ylim=c(0.5, (max(data$Row)+1))) +
    scale_y_reverse(breaks=seq(1, max(data$Row)), labels=LETTERS[1:max(data$Row)]) +
    scale_x_continuous(breaks=seq(1, max(data$Column))) +
    #scale_shape_manual(values=seq(from=0, to = length(unique(data$value)), by=1 ) ) +
    labs(title=title) 
  
  return(g)
}

#####################
##### Quality Control

#  Normalization

plot_qpcr_normalization <- function(qPCRraw=NULL, qPCRnorm=NULL, refgenes=NULL, samples=FALSE, title=NULL, xlab ="Cq before normalization", ylab="ddCT normalized Cq")
{
  if(!is.na(qPCRraw) && !is.na(qPCRnorm) && !is.null(refgenes))
  {
    if(!is.null(title)){
      main <- title
    } else {
      main <- paste("ddCT normalization using", paste(refgenes, collapse = ", ") , sep= " ")
    }
    ntarget <- length(featureNames(qPCRraw))
      
    return(plot(exprs(qPCRraw), exprs(qPCRnorm), pch = 20, main=main,  col = rep(brewer.pal(6, "Spectral"), each = ntarget), xlab=xlab, ylab=ylab))
    
  } else {return()}
  
}

# sample SD

plot_qpcr_variation <- function(qPCRraw=NULL)
{
  if(!is.na(qPCRraw))
  {
    return(plotCtVariation(qPCRraw, variation = "sd", log = TRUE, col = "lightgrey", type = "detail", add.featurenames = TRUE, pch = " ", cex = 1.2))
    
  } else {return()}
  
}


# Target quality (determined or ok)

plot_qpcr_determined <- function(qPCRnorm=NULL, title="Status of qPCR wells", ylab ="Number of Wells")
{
  if(!is.na(qPCRnorm))
  {
    return(plotCtCategory(q = qPCRnorm, stratify = "type", main = title, ylab = ylab))
    
  } else {return()}
  
}


# Density

plot_qpcr_density <- function(qPCRraw = NA, qPCRnorm=NA, refgenes = NA, maxCT = NULL, minCT = NULL)
{
  if(!is.na(qPCRraw) && !is.na(qPCRnorm) && !is.na(refgenes))
  {
    par(mfrow = c(1,2))
    # raw data
    plotCtDensity(qPCRraw,xlab = "Calculated Cq value", ylab = "Density ",main = "Distribution of calculated Cq values")
    if(!is.null(maxCT))
    {
      abline(v = as.numeric(maxCT), lty = 2, col = "red")
      text(x=as.numeric(maxCT), y = 0.01, labels = "Max Cq Limit", col = "red")
    }
    if(!is.null(minCT))
    {
      abline(v = as.numeric(minCT), lty = 2, col = "red")
      text(x=as.numeric(minCT), y = 0.01, labels = "Min Cq Limit", col = "red")
    }
    
    # normalized data
    plotCtDensity(qPCRnorm,xlab = paste("Calculated ddCT value, normalized to ", paste(refgenes, collapse = "-"), sep=""), ylab = "Density",main = "Distribution of calculated ddCT values")
    par(mfrow=c(1,1))
    
  } else {return()}
  
}


# CQ Values as Boxplot

plot_qpcr_boxplot_qc <- function(qPCRraw = NA, qPCRnorm=NA, refgenes = NA, maxCT = NULL, minCT = NULL)
{
  if(!is.na(qPCRraw) && !is.na(qPCRnorm) && !is.na(refgenes))
  {
    par(mfrow = c(1,2))
    # raw data
    plotCtBoxes(qPCRraw, stratify = "type",main = "Cq Distribution", ylab = "Calculated Cq" )
    if(!is.null(maxCT))
    {
      abline(h = as.numeric(maxCT), lty = 2, col = "red")
      text(y=as.numeric(maxCT), x = 1, labels = "Max Cq Limit", col = "red")
    }
    if(!is.null(minCT))
    {
      abline(h = as.numeric(minCT), lty = 2, col = "red")
      text(y=as.numeric(minCT), x = 1, labels = "Min Cq Limit", col = "red")
    }
    
    # normalized data
    plotCtBoxes(qPCRnorm, stratify = "type",main = paste("ddCT Distribution - normalized to ", paste(refgenes, collapse = "-"), sep=""), ylab = "Calculated Cq" )
    
    par(mfrow=c(1,1))
    
  } else {return()}
  
}



## Pairs

plot_qpcr_qc_pairs <- function(qPCRraw = NULL, qPCRnorm=NULL, title="qPCR Sample Correlation", maxCT = "35")
{
  if(!is.na(qPCRnorm) && !is.na(qPCRnorm) && !is.na(maxCT))
  {
    par(mfrow = c(1,2))
    plotCtPairs(qPCRraw, col = "type", diag = TRUE, cor = TRUE,Ct.max = maxCT, main = title)
    plotCtPairs(qPCRnorm, col = "type", diag = TRUE, cor = TRUE,Ct.max = maxCT, main = title)
    par(mfrow = c(1,1))
    
  } else {return()}
  
}

## PCA

plot_qpcr_qc_pca <- function(qPCRraw = NULL, qPCRnorm=NULL)
{
  if(!is.na(qPCRnorm) && !is.na(qPCRnorm) && !is.na(maxCT))
  {
    par(mfrow = c(1,2))
    plotCtPCA(qPCRraw,scale = FALSE)
    plotCtPCA(qPCRnorm,scale = FALSE)
    par(mfrow = c(1,1))
    
  } else {return()}
  
}

## Heatmap

plot_qpcr_qc_heatmap <- function(qPCRraw = NULL, qPCRnorm = NULL, title = "", distance = "euclidean")
{
   if(!is.null(qPCRraw))
   {
     plotCtHeatmap(qPCRraw, gene.names = featureNames(qPCRraw), dist = distance, main = title) 
   } else {
     plotCtHeatmap(qPCRnorm, gene.names = featureNames(qPCRnorm), dist = distance, main = title)
   }
  
}


###### ANALYSIS


plot_qpcr_analysis_CQ <- function(data = tidy_qpcr, target = NULL, yval = "Cqnorm", SD = "SD", refgenes = NULL)
{
  if(length(target) == 1)
  {
    xval <- "Sample"
    fillval <- "Gene"
  } else
  {
    xval <- "Gene"
    fillval <- "Sample" 
  }
  
  # get data
  data <- dplyr::filter(data, Gene %in% target) %>% na.omit()
  
  # set title and axes
  if(yval == "Cqraw")
  {
    title <- "Calculated Cq values"
    ylab <- "Cq"
  } else {
    title <- paste("dCq values normalized to ", paste(refgenes, collapse = ","), sep="")
    ylab <- "dCq"
  }
  
  # make plot
  p <- ggplot(data, aes_string(x=xval, y=yval,fill=fillval)) + 
    geom_bar(stat="identity", position = "dodge", na.rm = TRUE)
  
  if(yval == "Cqraw")
  {
    p <- p + geom_errorbar(aes(ymin=Cqraw-SD, ymax=Cqraw+SD),
                           size=.3,    # Thinner lines
                           width=.2,
                           position=position_dodge(.9)
    ) 
  }
  if(yval == "Cqnorm")
  {
    p <- p + geom_errorbar(aes(ymin=Cqnorm-SD, ymax=Cqnorm+SD),
                           size=.3,    # Thinner lines
                           width=.2,
                           position=position_dodge(.9)
    ) 
  }
  
  p <- p + ggplot2::theme_minimal() +
    labs(fill = fillval) +
    labs(title = title, y = ylab) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = rel(1.1)), legend.position="bottom")
  
  return(p)
}



qpcr_get_analysis <- function(tidydata = NULL, samples = NULL, genes = NULL, calibrator = NULL){
  
  if(any(is.null(tidydata), is.null(samples), is.null(genes), is.null(calibrator)))
  {
    return()
  }
  
  for(i in 1:length(genes))
  {
    df_analysis <- NULL
    df_analysis <- dplyr::filter(tidydata, Sample %in% samples & Gene %in% genes[i])
    
    division <- unique(dplyr::filter(tidydata, Sample %in% calibrator & Gene %in% genes[i]) %>% dplyr::select(Cqnorm))
    
    df_analysis$ddCq <- apply(df_analysis, 1, function(x, gene = genes[i]){
      df_tmp <- dplyr::filter(tidydata, Sample %in% x["Sample"] & Gene %in% gene)
      # print(df_tmp)
      df_tmp$FC <- df_tmp$Cqnorm - division$Cqnorm
      return(unique(df_tmp$FC))
      
    })
    
    df_analysis$FC <- apply(df_analysis, 1, function(x, gene = genes[i]){
      df_tmp <- dplyr::filter(tidydata, Sample %in% x["Sample"] & Gene %in% gene)
      df_tmp$FC <- 2^-(df_tmp$Cqnorm - division$Cqnorm)
      return(unique(df_tmp$FC))
      
    })
    
    df_analysis$FCsem <- apply(df_analysis, 1, function(x, gene = genes[i]){
      df_tmp <- dplyr::filter(tidydata, Sample %in% x["Sample"] & Gene %in% gene)
      df_tmp$FC <- df_tmp$Cqnorm / division$Cqnorm
      SD <- dplyr::filter(tidydata,  Sample %in% x["Sample"] & Gene %in% gene) %>% dplyr::select(SD)
      SD <- unique(SD$SD)
      
      if(!is.na(SD))
      {
        df_tmp$FCsem <- 1-(2^(-(SD) / sqrt(nrow(dplyr::filter(tidydata,  Sample %in% x["Sample"] & Gene %in% gene)))))
      } else {
        df_tmp$FCsem <- NA
      }
      return(unique(df_tmp$FCsem))
      
    })
    
    # add to df_analysis if necessary
    if(i!=1)
    {
      df_return <- dplyr::bind_rows(df_return, df_analysis)
    } else {
      df_return <- df_analysis
    }
    
  }
  
  # return whole tidy dataframe
  return(df_return)
  
}



plot_qpcr_analysis_calibrated <- function(data = NULL, yval = "FC", samples = NULL, calibrator = NULL, refgenes = NULL)
{
  
  ####
  # data must be tibble derived by qpcr_get_analysis
  ####
  
  if(any(is.null(data), is.null(samples), is.null(calibrator)))
  {
    return()
  }
  
  if(length(unique(data$Gene)) == 1)
  {
    xval <- "Sample"
    fillval <- "Gene"
  } else
  {
    xval <- "Gene"
    fillval <- "Sample" 
  }
  
  # DATA is already pre-defined by qpcr_get_analysis
  data <- data %>% na.omit()
  # set title and axes
  if(yval == "ddCq")
  {
    title <- paste("ddCq calibrated to ", calibrator, sep="")
    ylab <- "ddCq"
  } else {
    title <- paste("Foldchanges calibrated to ", calibrator, sep="")
    ylab <- "Foldchange"
  }
  
  # make plot
  p <- ggplot(data, aes_string(x=xval, y=yval,fill=fillval)) + 
    geom_bar(stat="identity", position = "dodge", na.rm = TRUE)
  
  if(yval == "FC")
  {
    p <- p + geom_errorbar(aes(ymin=FC-FCsem, ymax=FC+FCsem),
                           size=.3,    # Thinner lines
                           width=.2,
                           position=position_dodge(.9)
    ) 
  } else if(yval == "ddCq")
  {
    p <- p + geom_errorbar(aes(ymin=ddCq-SD, ymax=ddCq+SD),
                           size=.3,    # Thinner lines
                           width=.2,
                           position=position_dodge(.9)
    ) 
  }
  
  p <- p + ggplot2::theme_minimal() +
    labs(fill = fillval) +
    labs(title = title, y = ylab) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = rel(1.1)), legend.position="bottom")
  
  return(p)
}

##############
#### CQ CALCULATION

calculate_cq <- function(df.pcr = NULL, cq_calc_method = "Cy0"){
  
  calc <- as.list(colnames(df.pcr[,2:ncol(df.pcr)]))
  names(calc) <- colnames(df.pcr[,2:ncol(df.pcr)])
  #length(colnames(df.pcr))
  length(calc)
  
  for(i in 1:length(calc))
  {
    x <- names(calc)[i]
    out <- list()
    fitted <- try(qpcR::pcrfit(data = as.data.frame(df.pcr), 1, grep(pattern = paste("^",as.character(x),"$" , sep=""),x = colnames(df.pcr) ), model = l4 ))
    
    if(class(fitted) == "try-error")
    {
      out$fitted <- NA
      out$efficiency <- NA
      out$cq <- NA
    } else {
      # fitting worked
      
      out$fitted <- fitted
      efficiency = try(qpcR::efficiency(out$fitted, plot=FALSE, type = "Cy0", method="spline"))
      if(class(efficiency) == "try-error")
      {
        out$efficiency <- NA
        out$cq <- NA
      } else {
        
        out$efficiency <- efficiency
        # Fourth: get calculated Cq or calculate Cq for Cy0
        if(cq_calc_method == "Cy0")
        {
          cq <- try(qpcR::Cy0(object = out$fitted, plot = FALSE))
          if(class(cq) == "try-error")
          {
            cq <- NA
          } else {
            out$cq <- cq
          }
        } else
        {
          out$cq <- out$efficiency[[cq_calc_method]]
          
        }
      }
      
    }
    # return
    calc[[i]] <- out
  }
  return(calc)
  
}


## get tody data
qpcr_tidy_calc <- function(qPCRnormobject = NULL, qPCRrawobject = NULL){
  
  #tidy_qpcr <- tibble::as_tibble(fData(qPCRnorm))
  tidy_qpcr <- NULL
  
  # norm in object qPCRnorm, raw in qPCRraw
  
  for(i in 1:length(sampleNames(qPCRnormobject)))
  {
    if(i==1)
    {
      # tidy Ct
      ct <- NULL
      ct <- tibble::as_tibble( HTqPCR::getCt(qPCRrawobject)[,i])
      ct$value[ct$value >= maxCT] <- NA
      ct$value[ct$value <= minCT] <- NA
      tidy_qpcr <- tibble::as_tibble(fData(qPCRnormobject)) %>% dplyr::bind_cols(tibble::as_tibble(rep(sampleNames(qPCRnormobject)[i], times = nrow(fData(qPCRnormobject))))) %>% dplyr::bind_cols(tibble::as_tibble(getCt(qPCRnormobject)[,i])) %>% dplyr::bind_cols(ct) %>% dplyr::bind_cols(featureCategory(qPCRnormobject)[i])
      colnames(tidy_qpcr) <- c("Gene", "Type", "Pos", "Sample" , "Cqnorm", "Cqraw", "Flagged")
      
    } else
    {
      # tidy Ct
      ct <- NULL
      tidy_qpcr_temp <- NULL
      ct <- tibble::as_tibble(getCt(qPCRrawobject)[,i])
      ct$value[ct$value >= maxCT] <- NA
      ct$value[ct$value <= minCT] <- NA
      
      tidy_qpcr_temp <- tibble::as_tibble(fData(qPCRnormobject)) %>% dplyr::bind_cols(tibble::as_tibble(rep(sampleNames(qPCRnormobject)[i], times = nrow(fData(qPCRnormobject))))) %>% dplyr::bind_cols(tibble::as_tibble(getCt(qPCRnormobject)[,i])) %>% dplyr::bind_cols(ct) %>% dplyr::bind_cols(featureCategory(qPCRnormobject)[i])
      
      colnames(tidy_qpcr_temp) <- c("Gene", "Type", "Pos", "Sample" , "Cqnorm", "Cqraw", "Flagged")
      # add rows of sample
      tidy_qpcr <- dplyr::bind_rows(tidy_qpcr, tidy_qpcr_temp)
      
    }
  }
  
  ## add SD
  ## add SD for each target gene and sample
  
  tidy_qpcr$SD <- apply(tidy_qpcr,1, function(x){
    
    df_sd <- dplyr::filter(tidy_qpcr, Gene == x["Gene"], Sample == x["Sample"]) %>% dplyr::select(Gene, Cqraw)
    df_sd$SD <- sd(df_sd$Cqraw, na.rm = TRUE)
    df_sd$Cqraw <- NULL
    # bind to tmp
    #tidy_qpcr_temp <- dplyr::left_join(tidy_qpcr_temp,unique(df_sd),by = "Gene")
    return(unique(df_sd$SD))
  })

  
  
  # make undetermined NA
  tidy_qpcr <- dplyr::mutate(tidy_qpcr,
                                Row=as.numeric(match(toupper(substr(Pos, 1, 1)), LETTERS)),
                                Column=as.numeric(substr(Pos, 2, 5)) ) 
  
  tidy_qpcr$Var1 <- LETTERS[tidy_qpcr$Row]
  tidy_qpcr$Var2 <- tidy_qpcr$Column
  
  tidy_qpcr$Cqnorm <- round(tidy_qpcr$Cqnorm, digits = 2)
  
  tidy_qpcr$Cqnorm[tidy_qpcr$Flagged == "Undetermined"] <- NA
  
  return(tidy_qpcr)
  
}




####### FOLD CHANGE AND SEM calculation

qpcr_get_analysis <- function(tidydata = NULL, samples = NULL, genes = NULL, calibrator = NULL){
  
  # remove NA
  tidydata <- tidydata  %>% na.omit()
  
  if(any(is.null(tidydata), is.null(samples), is.null(genes), is.null(calibrator)))
  {
    return()
  }
  
  for(i in 1:length(genes))
  {
    df_analysis <- NULL
    df_analysis <- dplyr::filter(tidydata, Sample %in% samples) %>% dplyr::filter(Gene %in% genes[i])
    
    division <- unique(dplyr::filter(tidydata, Sample %in% calibrator & Gene %in% genes[i]) %>% dplyr::select(Cqnorm))
    
    df_analysis$ddCq <- apply(df_analysis, 1, function(x, gene = genes[i]){
      df_tmp <- dplyr::filter(tidydata, Sample %in% x["Sample"] & Gene %in% gene)
      # print(df_tmp)
      df_tmp$FC <- df_tmp$Cqnorm - division$Cqnorm
      return(unique(df_tmp$FC))
      
    })
    
    df_analysis$FC <- apply(df_analysis, 1, function(x, gene = genes[i]){
      df_tmp <- dplyr::filter(tidydata, Sample %in% x["Sample"] & Gene %in% gene)
      df_tmp$FC <- 2^-(df_tmp$Cqnorm - division$Cqnorm)
      return(unique(df_tmp$FC))
      
    })
    
    df_analysis$FCsem <- apply(df_analysis, 1, function(x, gene = genes[i]){
      df_tmp <- dplyr::filter(tidydata, Sample %in% x["Sample"] & Gene %in% gene)
      df_tmp$FC <- df_tmp$Cqnorm / division$Cqnorm
      SD <- dplyr::filter(tidydata,  Sample %in% x["Sample"] & Gene %in% gene) %>% dplyr::select(SD)
      SD <- unique(SD$SD)
      
      if(!is.na(SD))
      {
        df_tmp$FCsem <- 1-(2^(-(SD) / sqrt(nrow(dplyr::filter(tidydata,  Sample %in% x["Sample"] & Gene %in% gene)))))
      } else {
        df_tmp$FCsem <- NA
      }
      return(unique(df_tmp$FCsem))
      
    })
    
    # add calibrator
    df_analysis$Calibrator <- calibrator
    
    # add to df_analysis if necessary
    if(i!=1)
    {
      df_return <- dplyr::bind_rows(df_return, df_analysis)
    } else {
      df_return <- df_analysis
    }
    
  }
  # return whole tidy dataframe
  return(df_return)
  
}












