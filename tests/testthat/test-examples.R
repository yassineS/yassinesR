test_that("example_scatterplot returns a ggplot object", {
  plot <- example_scatterplot()
  expect_s3_class(plot, "ggplot")
})

test_that("example_barplot returns a ggplot object", {
  plot <- example_barplot()
  expect_s3_class(plot, "ggplot")
})

test_that("example_log_scatterplot returns a ggplot object", {
  plot <- example_log_scatterplot()
  expect_s3_class(plot, "ggplot")
})

test_that("scale_log_axis works for x-axis", {
  scale <- scale_log_axis("x")
  expect_s3_class(scale, "ScaleContinuousPosition")
})

test_that("scale_log_axis works for y-axis", {
  scale <- scale_log_axis("y")
  expect_s3_class(scale, "ScaleContinuousPosition")
})

test_that("scale_log_axis throws error for invalid axis", {
  expect_error(scale_log_axis("z"), "axis must be either 'x' or 'y'")
})

test_that("scale_log_axis accepts custom breaks and labels", {
  scale <- scale_log_axis("x", 
                          breaks = c(0.001, 0.01, 0.1), 
                          labels = c('0.1%', '1%', '10%'))
  expect_s3_class(scale, "ScaleContinuousPosition")
  # Verify that custom breaks were passed through
  expect_equal(scale$breaks, c(0.001, 0.01, 0.1))
  # Verify that custom labels were passed through
  expect_equal(scale$labels, c('0.1%', '1%', '10%'))
})

test_that("scale_log_axis accepts custom limits", {
  scale <- scale_log_axis("y", limits = c(0.01, 1))
  expect_s3_class(scale, "ScaleContinuousPosition")
  # Verify that custom limits were passed through
  expect_equal(scale$limits, c(0.01, 1))
})
