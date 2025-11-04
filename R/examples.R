#' Example plots for demonstrating color palettes
#'
#' Utility functions that return simple ggplot2 examples using the iris dataset
#' to demonstrate discrete color/fill scales.
#'
#' @details
#' - `example_scatterplot()` creates a scatter plot of the iris dataset,
#'   mapping Sepal.Length to the x-axis, Sepal.Width to the y-axis,
#'   and Species to color.
#' - `example_barplot()` creates a bar chart showing counts by Species.
#' - `example_log_scatterplot()` creates a scatter plot with log-scaled x-axis,
#'   demonstrating the use of logarithmic scales with proper tick marks.
#'
#' @return A `ggplot` object.
#'
#' @rdname example_plots
#'
#' @importFrom rlang .data
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
    ggplot2::geom_point(alpha = 0.7, size = 2) +
    ggplot2::geom_smooth(method = "loess", alpha = 0.05, linewidth = 1, span = 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank()
    )
}

#' @rdname example_plots
#' @export example_barplot
example_barplot <- function() {
  ggplot2::ggplot(
    iris,
    ggplot2::aes(x = .data$Species, fill = .data$Species)
  ) +
    ggplot2::geom_bar() +
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
  # Using Petal.Length (which has larger range) on x-axis
  ggplot2::ggplot(
    iris,
    ggplot2::aes(x = .data$Petal.Length, y = .data$Sepal.Width, colour = .data$Species)
  ) +
    ggplot2::geom_point(alpha = 0.7, size = 2) +
    scale_log_axis(axis = "x") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank()
    )
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
