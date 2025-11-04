#' Example plots for demonstrating color palettes
#'
#' Utility functions that return simple ggplot2 examples to demonstrate discrete
#' color/fill scales and themes.
#'
#' @details
#' - `example_scatterplot()` creates a scatter plot of the iris dataset,
#'   mapping Sepal.Length to the x-axis, Sepal.Width to the y-axis,
#'   and Species to color. Uses theme_yassine() with default geom_point aesthetics.
#' - `example_barplot()` creates a bar chart showing counts by Species.
#' - `example_log_scatterplot()` creates a scatter plot with log-scaled x-axis,
#'   demonstrating the use of logarithmic scales with proper tick marks. Uses
#'   simulated data with a wide range suitable for log scale demonstration.
#'
#' @return A `ggplot` object.
#'
#' @rdname example_plots
#'
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @importFrom stats rnorm
#'
#' @export example_scatterplot
#'
#' @examples
#' example_scatterplot()
#' example_barplot()
#' example_log_scatterplot()
example_scatterplot <- function() {
  ggplot2::ggplot(
    iris,
    ggplot2::aes(x = .data$Sepal.Length, y = .data$Sepal.Width, colour = .data$Species)
  ) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_smooth(method = "loess", alpha = 0.05, linewidth = 1, span = 1) +
    scale_color_yassine() +
    theme_yassine() +
    ggplot2::labs(caption = "Data source: iris dataset")
}

#' @rdname example_plots
#' @export example_barplot
example_barplot <- function() {
  ggplot2::ggplot(
    iris,
    ggplot2::aes(x = .data$Species, fill = .data$Species)
  ) +
    ggplot2::geom_bar() +
    scale_fill_yassine() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )
}

#' @rdname example_plots
#' @export example_log_scatterplot
example_log_scatterplot <- function() {
  # Create a dataset with wider range for log scale demonstration
  set.seed(123)  # for reproducibility
  
  df <- tibble::tibble(
    x = abs(rnorm(500)) * 100000,
    y = abs(rnorm(500)) * 10,
    col = sample(c("A", "B", "C"), size = 500, replace = TRUE)
  )
  
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$x, y = .data$y, colour = .data$col)
  ) +
    ggplot2::geom_point() +
    scale_log_axis(axis = "x") +
    scale_color_yassine() +
    theme_yassine() +
    ggplot2::labs(caption = "Data source: Simulated data")
}


#' Apply logarithmic scale with proper tick marks
#'
#' A helper function to apply logarithmic scaling (base 10) to either x or y axis
#' with properly formatted tick marks showing powers of 10.
#'
#' @param axis Character string specifying which axis to apply the log scale to.
#'   Either "x" or "y".
#' @param ... Additional arguments passed to scale_x_log10() or scale_y_log10()
#'
#' @return A ggplot2 scale object
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   scale_log_axis("x")
#'
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   scale_log_axis("y")
#' }
scale_log_axis <- function(axis = "x", ...) {
  if (!axis %in% c("x", "y")) {
    stop("axis must be either 'x' or 'y'")
  }
  
  if (axis == "x") {
    ggplot2::scale_x_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::label_math(10^.x),
      ...
    )
  } else {
    ggplot2::scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::label_math(10^.x),
      ...
    )
  }
}
