#' Custom ggplot2 Theme
#'
#' A clean ggplot2 theme resembling base R with legible text.
#' This theme is based on theme_classic() with customizations for scientific plots.
#'
#' @param base_size Base font size (default: 12)
#' @param base_family Base font family (default: "")
#' @param base_line_size Base line size (default: base_size/22)
#' @param base_rect_size Base rect size (default: base_size/22)
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @importFrom ggplot2 theme theme_classic element_text element_line element_blank element_rect %+replace% margin
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
  
  theme_classic(base_size = base_size,
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    theme(
      # Plot title - plain text, size 13, centered
      plot.title = element_text(size = 13, face = "plain", hjust = 0.5),
      
      # Axis styling - size 13 for titles, size 12 black for text
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 12, color = "black"),
      
      # Plot margins - top 5, right 2, bottom 5, left 16
      plot.margin = margin(t = 5, r = 2, b = 5, l = 16, unit = "pt"),
      
      # Legend - bottom position, size 12
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      
      complete = TRUE
    )
}
