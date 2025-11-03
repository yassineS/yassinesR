#' Format Numbers for Plotting
#'
#' A helper function to format numbers for use in plots, with options for
#' rounding, scientific notation, and adding suffixes.
#'
#' @param x Numeric vector to format
#' @param digits Number of digits to round to (default: 2)
#' @param scientific Whether to use scientific notation (default: FALSE)
#' @param big_mark Character to use for thousands separator (default: ",")
#'
#' @return Character vector of formatted numbers
#' @export
#'
#' @examples
#' format_numbers(c(1234.567, 8901.234))
#' format_numbers(c(1234567, 8901234), scientific = TRUE)
format_numbers <- function(x, digits = 2, scientific = FALSE, big_mark = ",") {
  if (scientific) {
    format(x, scientific = TRUE, digits = digits)
  } else {
    format(round(x, digits), big.mark = big_mark, trim = TRUE)
  }
}


#' Calculate Summary Statistics
#'
#' Calculate common summary statistics for a numeric vector.
#'
#' @param x Numeric vector
#' @param na.rm Whether to remove NA values (default: TRUE)
#'
#' @return A named list with summary statistics
#' @export
#'
#' @examples
#' summary_stats(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
summary_stats <- function(x, na.rm = TRUE) {
  list(
    n = if (na.rm) sum(!is.na(x)) else length(x),
    mean = mean(x, na.rm = na.rm),
    median = median(x, na.rm = na.rm),
    sd = sd(x, na.rm = na.rm),
    min = min(x, na.rm = na.rm),
    max = max(x, na.rm = na.rm),
    q25 = quantile(x, 0.25, na.rm = na.rm),
    q75 = quantile(x, 0.75, na.rm = na.rm)
  )
}


#' Percentage Calculator
#'
#' Calculate percentages with optional rounding.
#'
#' @param x Numerator value(s)
#' @param total Denominator value
#' @param digits Number of digits to round to (default: 1)
#'
#' @return Numeric vector of percentages
#' @export
#'
#' @examples
#' percent(25, 100)
#' percent(c(10, 20, 30), 100)
percent <- function(x, total, digits = 1) {
  round((x / total) * 100, digits)
}


#' Not In Operator
#'
#' A helper operator for checking if values are NOT in a vector.
#' This is the complement of the %in% operator.
#'
#' @param x Vector of values to check
#' @param y Vector to check against
#'
#' @return Logical vector
#' @export
#'
#' @examples
#' 1:5 %notin% c(3, 4, 5)
`%notin%` <- function(x, y) {
  !(x %in% y)
}
