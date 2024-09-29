say_hi <- function() {
  ("hello world")
}

plot_z_histogram <- function(data,
                             limits = TRUE,
                             z_limit = 3.29, fill = "blue") {
  
  plot <- ggplot2::ggplot(mapping = aes(x = data, fill = fill)) +
    geom_histogram(show.legend = FALSE) 
  
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

find_iqr_outliers <- function(){
  boxLimits <- quantile(
    df2$value,
    probs = c(.25, .75),
    na.rm = TRUE,
    type = 7
  ) # S calculation method (differs from SPSS)
  iqr <- diff(boxLimits)
  mild_limit <- self$options$iqrLimitMild
  extreme_limit <- self$options$iqrLimitExtreme
  
  calculate_distance <- function(value) {
    if (value > boxLimits[2])
      return((value - boxLimits[2]) / iqr)
    if (value < boxLimits[1])
      return ((value - boxLimits[1]) / iqr)
    return(NA)
  }
  
  calculate_distance_v <- Vectorize(calculate_distance)
  
  df2 <- df2 %>%
    mutate(
      iqr_distance = calculate_distance_v(value),
      mild_iqr = abs(iqr_distance) >= mild_limit,
      extreme_iqr = abs(iqr_distance) >= extreme_limit
    )
}
  
plot_boxplot <- function() {
  plotData <- image$state
  
  if (self$options$useIQR) {
    points <- plotData %>%
      filter(mild_iqr) %>%
      mutate(x = 0, shape = ifelse(extreme_iqr, 8, 1))
    
    plot <- ggplot(plotData, aes(x = 0, y = value)) +
      geom_boxplot(coef = self$options$iqrLimitMild,
                   outlier.shape = NA) +
      geom_point(data = points, aes(shape = shape), size = 2) +
      scale_shape_identity() +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  } else {
    plot <- ggplot(plotData, aes(x = factor(0), y = value)) +
      geom_boxplot() +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  }
  plot
}


update_results_IQR <- function() {
  if(self$options$useIQR) {
    iqr_outliers <- outliers %>%
      filter(mild_iqr) %>%
      mutate(type = ifelse(extreme_iqr, "extreme", "mild"))
    
    table <- self$results$iqr
    
    if (nrow(iqr_outliers > 0))
      for(i in 1:nrow(iqr_outliers)){
        table$addRow(rowKey = i,
                     values = list(
                       id = iqr_outliers$rownum[i],
                       value = iqr_outliers$value[i],
                       distance = iqr_outliers$iqr_distance[i],
                       type = iqr_outliers$type[i]
                     ))
        
      }
  }
}