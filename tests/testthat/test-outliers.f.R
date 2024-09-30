test_that("outlier finding works", {
  df <- tibble(value = c(1:100,1000))
  result <- find_z_outliers(df, value, 3.29)
  expect_s3_class(result, class="data.frame")
  expect_equal(length(result$z_value), 1)
})
