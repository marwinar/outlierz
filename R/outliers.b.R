
# This file is a generated template, your changes will not be overwritten

outliersClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "outliersClass",
  inherit = outliersBase,
  private = list(
    .run = function() {
      outliers <- private$.compute()
      private$.populateResults(outliers)
      
      image <- self$results$hist
      image$setState(outliers)
      
      image2 <- self$results$boxplot
      image2$setState(outliers)
      
    },
    
    .compute = function() {
      data <- self$data
      variable <- self$options$dep
      df <- jmvcore::select(data, variable)
      
      if (nrow(df) == 0)
        return(NULL)
      
      df2 <- cbind(rownum = 1:nrow(df), df)
      colnames(df2)[2] <- "value"
      
      
      if (self$options$useMin) {
        df2 <- mutate(df2, outOfRangeLow = value <= !!self$options$minValue)
      }
      
      if (self$options$useMax) {
        df2 <- mutate(df2, outOfRangeHigh = value >= !!self$options$maxValue)
      }
      
      if (self$options$useZ) {
        df2 <- df2 %>%
          mutate(z_value = scale(value),
                 z_out_of_range = abs(z_value) >= !!self$options$zLimit)
      }
      
      if (self$options$useIQR) {
        boxLimits <- quantile(df2$value, probs = c(.25, .75), na.rm = TRUE,
                              type = 7) # S calculation method (differs from SPSS)
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
          mutate(iqr_distance = calculate_distance_v(value),
                 mild_iqr = abs(iqr_distance) >= mild_limit,
                 extreme_iqr = abs(iqr_distance) >= extreme_limit)
      }
      
      return(df2)          
    },
    
    .populateResults = function(outliers) {
      if(self$options$useMin | self$options$useMax) {
        table <- self$results$outofrange
        
        range_outliers <- outliers %>%
          filter(if_any(starts_with("outOfRange"), ~ . == TRUE))
        
        #            self$results$text$setContent(outliers)
        
        if(nrow(range_outliers) > 0)
          for (i in 1:nrow(range_outliers)) {
            table$addRow(rowKey=i, values=list(
              id = range_outliers$rownum[i],
              value = range_outliers$value[i],
              outlier = "yes"))
          }
      }
      
      if(self$options$useZ) {
        table <- self$results$zscores
        
        z_outliers <- outliers %>%
          filter(z_out_of_range == TRUE)
        
        #            self$results$text$setContent(outliers)
        
        if(nrow(z_outliers) > 0)
          for (i in 1:nrow(z_outliers)) {
            table$addRow(rowKey=i, values=list(
              id = z_outliers$rownum[i],
              value = z_outliers$value[i],
              zvalue = z_outliers$z_value[i]))
          }
      }
      
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
    },
    
    .plotHistogram = function(image, ...) {
      plotData <- image$state
      
      data_mean <- mean(plotData$value, na.rm = TRUE)
      data_sd <- sd(plotData$value, na.rm = TRUE)
      
      plot <- ggplot(plotData, aes(x=value)) +
        geom_histogram() +
        scale_x_continuous(sec.axis = sec_axis(
          trans = ~ (. - data_mean) / data_sd, 
          name = "z-score")) 
      
      if (self$options$useZ) {
        limit <- self$options$zLimit
        lower <- data_mean - limit * data_sd
        upper <- data_mean + limit * data_sd
        
        plot <- plot +
          geom_vline(xintercept = lower, color = "red") +
          geom_vline(xintercept = upper, color = "red")
      }
      
      print(plot)
      TRUE
    },
    .plotBoxplot = function(image, ...) {
      plotData <- image$state
      
      if(self$options$useIQR) {
        points <- plotData %>%
          filter(mild_iqr) %>%
          mutate(x = 0,
                 shape = ifelse(extreme_iqr, 8, 1))
        
        plot <- ggplot(plotData, aes(x = 0, y = value)) +
          geom_boxplot(coef = self$options$iqrLimitMild, outlier.shape = NA) +
          geom_point(data = points, aes(shape = shape), size = 2) +
          scale_shape_identity() +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())
      } else {
        plot <- ggplot(plotData, aes(x = factor(0), y = value)) +
          geom_boxplot() +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())
      }
      print(plot)
      return(TRUE)
      
    }
  )
)
