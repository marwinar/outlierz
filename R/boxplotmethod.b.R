


# This file is a generated template, your changes will not be overwritten

boxplotmethodClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "boxplotmethodClass",
    inherit = boxplotmethodBase,
    private = list(
      .run = function() {
        results <- private$.compute()
        if (!is.null(results$outliers)) {
          private$.populateResults(results$outliers)
          private$.populateSummary(results)
          
          image <- self$results$boxplot
          image$setState(results$data)
        }
      },
      
      .compute = function() {
        df <- jmvcore::select(self$data, self$options$dep)
        dep <- self$options$dep
        if (nrow(df) == 0)
          return(NULL)
        
        limit_mild <- self$options$iqrLimitMild
        limit_extreme <- self$options$iqrLimitExtreme
        rownum = as.integer(rownames(self$data))
        
        results <- find_iqr_outliers(df, dep, rownum, limit_mild, limit_extreme)
        results
      },
      .populateResults = function(outliers) {
        table <- self$results$iqrscores
        
        if (nrow(outliers) > 0) {
          results <- tidyr::nest(.data = outliers,
                                 data = c(type, rownum, value, iqr_distance))

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
        table$setRow(rowNo = 1, values = results$summary)
        TRUE
      },
      
      .plotBoxplot = function(image, ggtheme, theme, ...) {
        plotData <- image$state
        if (is.null(plotData)) {
          return(FALSE)
        }

        plot <- plot_boxplot(plotData, self$options$iqrLimitMild, fill = theme$fill[2]) +
          ggtheme +
          ylab(self$options$dep) +
          theme(
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
          )

        print(plot)
        TRUE
      }
    ),
  )
