iqr_values <- function(values) {
  boxLimits <- quantile(values,
                        probs = c(.25, .75),
                        na.rm = TRUE,
                        type = 7) # S calculation method (differs from SPSS)
  iqr <- boxLimits[2] - boxLimits[1]
  upper_limit_box = boxLimits[2]
  lower_limit_box = boxLimits[1]
  
  purrr::map_dbl(values, function(.x) {
    ifelse (
      .x > upper_limit_box,
      (.x - upper_limit_box) / iqr,
      ifelse(.x < lower_limit_box, (.x - lower_limit_box) / iqr, NA)
    )
  })
}

find_iqr_outliers <- function(df, var, rownum, limit_mild, limit_extreme) {
  results <- df %>%
    mutate(
      value = df[[var]],
      rownum = rownum,
      iqr_distance = iqr_values(df[[var]]),
      type = case_when(
        iqr_distance > 0 &
          abs(iqr_distance) >= limit_extreme ~ "high extreme",
        iqr_distance < 0 &
          abs(iqr_distance) >= limit_extreme ~ "low extreme",
        iqr_distance < 0 & abs(iqr_distance) >= limit_mild ~ "low mild",
        iqr_distance > 0 &
          abs(iqr_distance) >= limit_mild ~ "high mild",
        is.na(iqr_distance) ~ "not an outlier",
        TRUE ~ "not an outlier"
      )
    )
  
  outliers <- results %>%
    filter(type != "not an outlier") %>%
    arrange(iqr_distance) %>%
    mutate(rowKey = row_number())
  
  summary <- list(
    variable = var,
    n = nrow(results),
    missing = sum(is.na(df[[var]])),
    outliers_low_extreme = sum(outliers$type == "low extreme"),
    outliers_low_mild = sum(outliers$type == "low mild"),
    outliers_high_extreme = sum(outliers$type == "high extreme"),
    outliers_high_mild = sum(outliers$type == "high mild")
  )
  summary$outliers_total <- with(
    summary,
    outliers_low_extreme + outliers_low_mild +
      outliers_high_extreme + outliers_high_mild
  )
  
  list(data = results, outliers = outliers, summary = summary)
}

plot_boxplot <- function(data, iqr_limit_mild, fill = "blue") {
    points <- data %>%
      filter(type != "not an outlier") %>%
      mutate(x = 0, 
             shape = case_when(
               type == "high extreme" ~ 8,
               type == "low extreme" ~ 8,
               TRUE ~ 1))
    
    plot <- ggplot(data, aes(x = 0, y = value, fill = fill)) +
      geom_boxplot(coef = iqr_limit_mild,
                   outlier.shape = NA, show.legend = FALSE) +
      geom_point(data = points, aes(shape = shape, alpha = .6), size = 2, show.legend = FALSE) +
      scale_shape_identity() +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  plot
}


