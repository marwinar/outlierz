test_that("outlier finding works", {
  data <- c(1:100,1000)
  names(data) <- 1:length(data)
  result <- find_z_outliers(data, names(data), 3.29)

  expect_s3_class(result, class="data.frame")
  expect_equal(length(result$z_value), 1)
})
