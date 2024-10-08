say_hi <- function() {
  ("hello world")
}

plot_z_histogram <- function(data,
                             limits = TRUE,
                             z_limit = 3.29,
                             fill = "blue") {
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
      geom_vline(xintercept = upper, color = "red") +
      geom_text(
        x = lower,
        y = Inf,
        hjust = 1,
        vjust = 1.4,
        angle = 90,
        label = sprintf("z = %.2f", -limit),
        color = "red"
      ) +
      geom_text(
        x = upper,
        y = Inf,
        hjust = 1,
        vjust = -1,
        angle = 90,
        label = sprintf("z = %.2f", limit),
        color = "red"
      )
  }
  plot
}

find_z_outliers <- function(df, var, rownum, limit = 3.29) {
  data <- df[[var]]
  results <- tibble(value = data,
                    z_value = as.numeric(scale(data)),
                    rownum = rownum) %>%
    mutate(
      type = case_when(
        z_value < -limit ~ "below limit",
        z_value > limit ~ "above limit",
        TRUE ~ "not an outlier"
      )
    )
  
  outliers <- results %>%
    filter(type != "not an outlier") %>%
    arrange(type, abs(z_value)) %>%
    mutate(rowKey = row_number())
  
  summary <- list(
    variable = var,
    n = length(data),
    missing = sum(is.na(data)),
    outliers_low = sum(outliers$type == "below limit"),
    outliers_high = sum(outliers$type == "above limit")
  )
  summary$outliers_total <- summary$outliers_low + summary$outliers_high
  
  
  return(list(
    data = df,
    outliers = outliers,
    summary = summary
  ))
}
