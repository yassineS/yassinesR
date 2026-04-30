## Raincloud plots — Majorelle-styled wrappers.
##
## A raincloud (Allen et al. 2019) combines a half-violin density curve, a
## narrow boxplot, and jittered raw points so distribution shape, summary
## statistics, and raw observations all show together. ggrain (njudd/ggrain)
## is the canonical R package but isn't on conda-forge; we compose the same
## visual ourselves using ggplot2 + gghalves which both ARE conda-friendly.

#' Raincloud layers for Majorelle-styled ggplot2 plots.
#'
#' Returns a list of ggplot2 layers (half-violin + boxplot + jittered points)
#' that you `+` onto an existing ggplot. Map the categorical variable to `x`
#' (or `y` if `flip = TRUE`) and the numeric variable to the other axis.
#'
#' Pair with `theme_majorelle()` and one of the discrete fill scales
#' (`scale_fill_majorelle()`) so the variant's qualitative palette flows
#' through automatically.
#'
#' @param violin_alpha Alpha for the half-violin density curve.
#' @param violin_width Width of the half-violin (relative units).
#' @param box_width Width of the inner boxplot.
#' @param box_alpha Alpha for the boxplot fill.
#' @param jitter_width Horizontal jitter for the raw points.
#' @param point_size,point_alpha Styling for the raw points.
#' @param flip If `TRUE`, swap to a horizontal raincloud (categorical on the
#'   y-axis). Defaults to `FALSE` (vertical, categorical on x).
#'
#' @export
geom_raincloud_majorelle <- function(violin_alpha = 0.55,
                                     violin_width = 0.8,
                                     box_width    = 0.12,
                                     box_alpha    = 0.8,
                                     jitter_width = 0.06,
                                     point_size   = 1.4,
                                     point_alpha  = 0.55,
                                     flip         = FALSE) {
  half_side <- if (flip) "b" else "r"
  box_nudge <- if (flip) 0 else -box_width / 2
  jitter_nudge <- if (flip) 0 else -box_width * 1.6

  layers <- list(
    ## Half-violin — density curve, drawn to one side of the category centre.
    gghalves::geom_half_violin(
      side      = half_side,
      width     = violin_width,
      alpha     = violin_alpha,
      trim      = TRUE,
      linewidth = 0.4
    ),
    ## Narrow boxplot — sits beside the violin (slightly nudged left/up).
    ggplot2::geom_boxplot(
      width         = box_width,
      alpha         = box_alpha,
      outlier.shape = NA,
      linewidth     = 0.4,
      position      = ggplot2::position_nudge(x = box_nudge)
    ),
    ## Raw points — jittered cloud on the opposite side of the violin.
    ggplot2::geom_jitter(
      shape    = 21,
      stroke   = 0.25,
      size     = point_size,
      alpha    = point_alpha,
      width    = jitter_width,
      height   = 0,
      position = ggplot2::position_jitter(width = jitter_width, height = 0,
                                          seed = 7)
    )
  )

  if (flip) {
    layers <- c(layers, list(ggplot2::coord_flip()))
  }
  layers
}

#' Convenience: build a complete Majorelle-styled raincloud plot in one call.
#'
#' @param data A data frame.
#' @param x,y Column names — `x` categorical, `y` numeric (or swap with
#'   `flip = TRUE` for horizontal).
#' @param fill Optional grouping aesthetic; defaults to `x` so each category
#'   gets its own colour from the variant's qualitative palette.
#' @param ... Extra args forwarded to `geom_raincloud_majorelle()`.
#'
#' @export
raincloud_majorelle <- function(data, x, y, fill = NULL, ...) {
  fill_var <- if (is.null(fill)) x else fill
  ggplot2::ggplot(data, ggplot2::aes(.data[[x]], .data[[y]],
                                     fill = .data[[fill_var]])) +
    geom_raincloud_majorelle(...) +
    scale_fill_majorelle() +
    theme_majorelle()
}
