
# This file is a generated template, your changes will not be overwritten

outliersClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "outliersClass",
  inherit = outliersBase,
  private = list(
    .run = function() {
      outliers <- private$.compute()
      if(!is.null(outliers)) {
        private$.populateResults(outliers)
        
        image <- self$results$hist
        image$setState(outliers)
      }      
    },
    
    .compute = function() {
      data <- self$data
      variable <- self$options$dep
      df <- jmvcore::select(data, variable)
      
      if (nrow(df) == 0)
        return(NULL)
      
      df2 <- cbind(rownum = 1:nrow(df), df)
      colnames(df2)[2] <- "value"
      

      if (self$options$useZ) {
        df2 <- df2 %>%
          mutate(z_value = scale(value),
                 z_out_of_range = abs(z_value) >= !!self$options$zLimit)
      }
      return(df2)          
    },
    
    .populateResults = function(outliers) {
      if(self$options$useZ) {
        table <- self$results$zscores
        
        z_outliers <- outliers %>%
          filter(z_out_of_range == TRUE)
        

        if(nrow(z_outliers) > 0)
          for (i in 1:nrow(z_outliers)) {
            table$addRow(rowKey=i, values=list(
              id = z_outliers$rownum[i],
              value = z_outliers$value[i],
              zvalue = z_outliers$z_value[i]))
          }
      }
    },
    
    .plotHistogram = function(image, ggtheme, theme, ...) {
      plotData <- image$state
      if(is.null(plotData)) {
        return()
      }
      
      plot <- plot_z_histogram(plotData$value, 
                               limits = self$options$useZ, 
                               z_limit = self$options$zLimit,
                               fill = theme$fill[2]) +
        ggtheme 
      print(plot)
      TRUE
    }

  )
)
