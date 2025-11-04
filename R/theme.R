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


#' Custom ggplot2 Dark Theme
#'
#' A dark version of theme_yassine() with a CrowBlack background and GhostGumGrey text.
#' This theme preserves the visual identity of theme_yassine() while providing a dark aesthetic
#' suitable for presentations and screens.
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
#'   theme_yassine_dark()
#' }
theme_yassine_dark <- function(base_size = 12,
                               base_family = "",
                               base_line_size = base_size / 22,
                               base_rect_size = base_size / 22) {
  
  # Define dark theme colors
  crow_black <- "#2B3036"
  ghost_gum_grey <- "#EAEEE8"
  
  theme_classic(base_size = base_size,
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    theme(
      # Plot title - plain text, size 13, centered, light color
      plot.title = element_text(size = 13, face = "plain", hjust = 0.5, color = ghost_gum_grey),
      
      # Axis styling - size 13 for titles, size 12 for text, light color
      axis.title = element_text(size = 13, color = ghost_gum_grey),
      axis.text = element_text(size = 12, color = ghost_gum_grey),
      axis.line = element_line(color = ghost_gum_grey),
      axis.ticks = element_line(color = ghost_gum_grey),
      
      # Plot background - dark
      plot.background = element_rect(fill = crow_black, color = NA),
      panel.background = element_rect(fill = crow_black, color = NA),
      
      # Plot margins - top 5, right 2, bottom 5, left 16 (same as light theme)
      plot.margin = margin(t = 5, r = 2, b = 5, l = 16, unit = "pt"),
      
      # Legend - bottom position, size 12, light color and dark background
      legend.position = "bottom",
      legend.text = element_text(size = 12, color = ghost_gum_grey),
      legend.title = element_text(color = ghost_gum_grey),
      legend.background = element_rect(fill = crow_black, color = NA),
      legend.key = element_rect(fill = crow_black, color = NA),
      
      complete = TRUE
    )
}
