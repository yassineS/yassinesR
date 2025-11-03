#' Custom ggplot2 Theme
#'
#' A clean and modern ggplot2 theme with customizable options.
#' This theme provides a minimal look with good defaults for scientific plots.
#'
#' @param base_size Base font size (default: 12)
#' @param base_family Base font family (default: "")
#' @param base_line_size Base line size (default: base_size/22)
#' @param base_rect_size Base rect size (default: base_size/22)
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @importFrom ggplot2 theme theme_minimal element_text element_line element_blank element_rect %+replace%
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   theme_yassine()
#' }
theme_yassine <- function(base_size = 12,
                          base_family = "",
                          base_line_size = base_size / 22,
                          base_rect_size = base_size / 22) {
  
  theme_minimal(base_size = base_size,
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    theme(
      # Plot background
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      
      # Grid
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.5),
      panel.grid.minor = element_line(colour = "grey95", linewidth = 0.25),
      
      # Axes
      axis.line = element_line(colour = "grey20", linewidth = 0.5),
      axis.ticks = element_line(colour = "grey20", linewidth = 0.5),
      axis.text = element_text(colour = "grey20", size = rel(0.9)),
      axis.title = element_text(colour = "grey20", size = rel(1.0), face = "bold"),
      
      # Legend
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key = element_rect(fill = "white", colour = NA),
      legend.text = element_text(colour = "grey20", size = rel(0.9)),
      legend.title = element_text(colour = "grey20", size = rel(1.0), face = "bold"),
      
      # Plot title and subtitle
      plot.title = element_text(
        colour = "grey20",
        size = rel(1.2),
        face = "bold",
        hjust = 0,
        margin = margin(b = base_size / 2)
      ),
      plot.subtitle = element_text(
        colour = "grey30",
        size = rel(1.0),
        hjust = 0,
        margin = margin(b = base_size / 2)
      ),
      plot.caption = element_text(
        colour = "grey50",
        size = rel(0.8),
        hjust = 1,
        margin = margin(t = base_size / 2)
      ),
      
      # Strip (for facets)
      strip.background = element_rect(fill = "grey90", colour = NA),
      strip.text = element_text(colour = "grey20", size = rel(1.0), face = "bold"),
      
      complete = TRUE
    )
}
