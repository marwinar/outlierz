



# This file is a generated template, your changes will not be overwritten

outliersClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "outliersClass",
    inherit = outliersBase,
    private = list(
      .run = function() {
        results <- private$.compute()
        if (!is.null(results$outliers)) {
          private$.populateResults(results$outliers)
          private$.populateSummary(results)
          
          image <- self$results$hist
          image$setState(results$data)
        }
      },
      
      .compute = function() {
        df <- jmvcore::select(self$data, self$options$dep) 
        if (nrow(df) == 0)
          return(NULL)
        
        z_limit = self$options$zLimit
        rownum = as.integer(rownames(self$data))  
        
        outliers <- find_z_outliers(df[,1], 
                                    rownum, 
                                    z_limit)
        return(list(data = df, outliers = outliers))
      },
      
      .populateResults = function(outliers) {
        table <- self$results$zscores
        
        if (nrow(outliers) > 0) {
          results <- tidyr::nest(.data = outliers, data = c(type, rownum, value, z_value))
          
          purrr::walk2(
            .x = results$rowKey,
            .y = results$data,
            .f = function(.x, .y) {
              table$addRow(rowKey = .x, values = .y)
            }
          )
        }
      },
      
      .populateSummary = function(results) {
        table <- self$results$summary
        values = list(
          variable = "variabele a",
          n = 100,
          missing = 5,
          outliers_low = 2,
          outliers_high = 5,
          outliers_total = 7
        )
        
        table$setRow(rowNo = 1, values = values)
        TRUE
      },
      
      .plotHistogram = function(image, ggtheme, theme, ...) {
        plotData <- image$state
        if (is.null(plotData)) {
          return(FALSE)
        }
        
        plot <- plot_z_histogram(
          plotData[, 1],
          limits = TRUE,
          z_limit = self$options$zLimit,
          fill = theme$fill[2]
        ) +
          ggtheme
        print(plot)
        TRUE
      }
      
    )
  )
