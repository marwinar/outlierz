test_that("finding specific outlier by z-score", {
  data <- tibble(myvar = c(1:100,1000))
  rownum <- c(1:101)

  result <- find_z_outliers(data, var = "myvar", rownum = rownum, limit =  3.29)

  expect_type(result, "list")
  expect_equal(result$outliers$value[1], 1000)
  expect_equal(result$outliers$z_value[1], 9.516, tolerance=1e-3)
  expect_equal(result$outliers$type[1], "above limit")
})

test_that("outlier summaries by z-score", {
  data <- tibble(myvar = c(1:5, 500:700, 1000))
  rownum <- seq_len(nrow(data))
  
  result <- find_z_outliers(data, var = "myvar", rownum = rownum, limit =  3.29)
  
  expect_type(result$summary, "list")
  expect_equal(result$summary$variable, "myvar")
  expect_equal(result$summary$outliers_low, 5)
  expect_equal(result$summary$outliers_high, 1)
  expect_equal(result$summary$outliers_total, 6)
  
})
