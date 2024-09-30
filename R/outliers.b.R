


# This file is a generated template, your changes will not be overwritten

outliersClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "outliersClass",
    inherit = outliersBase,
    private = list(
      .run = function() {
        outliers <- private$.compute()
        if (!is.null(outliers)) {
          private$.populateResults(outliers)
          
          image <- self$results$hist
          image$setState(outliers)
        }
      },
      
      .compute = function() {
        df <- jmvcore::select(self$data, self$options$dep)
        if (nrow(df) == 0)
          return(NULL)
        

        df2 <- df %>%
          mutate(rownum = rownames(self$data)) %>%
          rename(value = 1) %>%
          mutate(
            z_value = as.numeric(scale(value)),
            z_out_of_range = abs(z_value) >= !!self$options$zLimit
          )
        return(df2)
      },
      
      .populateResults = function(outliers) {
        table <- self$results$zscores
        
        z_outliers <- outliers %>%
          filter(z_out_of_range == TRUE) %>%
          select(-z_out_of_range) %>%
          mutate(rowKey = row_number()) %>%
          arrange(z_value)
        
        if (nrow(z_outliers) > 0) {
          results <- tidyr::nest(.data = z_outliers, data = c(rownum, value, z_value))
          
          purrr::walk2(
            .x = results$rowKey,
            .y = results$data,
            .f = function(.x, .y) {
              table$addRow(rowKey = .x, values = .y)
            }
          )
        }
      },
      
      .plotHistogram = function(image, ggtheme, theme, ...) {
        plotData <- image$state
        if (is.null(plotData)) {
          return(FALSE)
        }
        
        plot <- plot_z_histogram(
          plotData$value,
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
