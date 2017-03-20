# saved as functions.R

#### LUCIFERASE


plot_luc_analysis_plot_rawdata_FLUC <- function(data = luc_file_data()$plot)
{
  p <- ggplot(data, aes(x=Treatment, y=FLUC, fill=Treatment)) + 
    geom_boxplot() +
    geom_jitter(shape=16, position=position_jitter(0.2)) +
    labs(title = "FLUC", x = "Samples", y = "FLUC") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = rel(1.1)))
  return(p)
}


plot_luc_analysis_plot_rawdata_RLUC <- function(data = luc_file_data()$plot)
{
  p <- ggplot(data, aes(x=Treatment, y=RLUC, fill=Treatment)) + 
    geom_boxplot() +
    geom_jitter(shape=16, position=position_jitter(0.2)) +
    labs(title = "RLUC", x = "Samples", y = "RLUC") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = rel(1.1)))
  return(p)
}

plot_luc_analysis_plot_rawdata_RATIO <- function(data = luc_file_data()$plot)
{
  p <- ggplot(data, aes(x=Treatment, y=Divided, fill=Treatment)) + 
    geom_boxplot() +
    geom_jitter(shape=16, position=position_jitter(0.2)) +
    labs(title = "FLUC normalized by RLUC", x = "Samples", y = "Ratio FLUC/RLUC") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = rel(1.1)))
  return(p)
}

plot_luc_analysis_plot_rawdata_LOG2RATIO <- function(data = luc_file_data()$plot)
{
  p <- ggplot(data, aes(x=Treatment, y=Log2, fill=Treatment)) + 
    geom_boxplot() +
    geom_jitter(shape=16, position=position_jitter(0.2)) +
    labs(title = "Log2 FLUC normalized by RLUC", x = "Samples", y = "Log2 Ratio FLUC/RLUC") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = rel(1.1)))
  return(p)
}


