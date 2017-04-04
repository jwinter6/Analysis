# saved as functions.R

#### LUCIFERASE





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
