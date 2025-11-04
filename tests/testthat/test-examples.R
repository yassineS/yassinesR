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
