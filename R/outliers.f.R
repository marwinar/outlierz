say_hi <- function() {
  ("hello world")
}

plot_z_histogram <- function(data, limits = TRUE, z_limit = 3.29) {
  plot <- ggplot2::ggplot(mapping = aes(x=data)) +
    geom_histogram() +
    scale_x_continuous(sec.axis = sec_axis(
      trans = scale, 
      name = "z-score")) 
  
  if (limits) {
    data_mean <- mean(data, na.rm = TRUE)
    data_sd <- sd(data, na.rm = TRUE)

    limit <- z_limit
    lower <- data_mean - limit * data_sd
    upper <- data_mean + limit * data_sd
    
    plot <- plot +
      geom_vline(xintercept = lower, color = "red") +
      geom_vline(xintercept = upper, color = "red")
  }
  plot
}