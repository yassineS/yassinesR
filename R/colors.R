#' Yassine's Colour Palette
#'
#' A carefully curated colour palette for data visualization.
#' These colours work well together and are suitable for both print and screen.
#'
#' @param palette Name of the palette. Options: "main", "cool", "warm", "contrast", "bodl"
#' @param n Number of colours to return. If NULL, returns all colours in the palette.
#' @param type Either "discrete" or "continuous"
#'
#' @return A character vector of hex colour codes
#' @export
#'
#' @examples
#' # Get all colours from the main palette
#' yassine_colors("main")
#'
#' # Get 5 colours from the cool palette
#' yassine_colors("cool", n = 5)
yassine_colors <- function(palette = "main", n = NULL, type = "discrete") {
  
  # Define colour palettes
  palettes <- list(
    main = c(
      "#0066CC",  # Blue
      "#FF6B35",  # Orange
      "#004E89",  # Dark Blue
      "#F77F00",  # Dark Orange
      "#06A77D",  # Teal
      "#D62828",  # Red
      "#8338EC",  # Purple
      "#FFBA08"   # Yellow
    ),
    cool = c(
      "#0066CC",  # Blue
      "#004E89",  # Dark Blue
      "#06A77D",  # Teal
      "#00B4D8",  # Light Blue
      "#8338EC",  # Purple
      "#4CC9F0"   # Cyan
    ),
    warm = c(
      "#FF6B35",  # Orange
      "#F77F00",  # Dark Orange
      "#D62828",  # Red
      "#FFBA08",  # Yellow
      "#E63946",  # Bright Red
      "#FCA311"   # Gold
    ),
    contrast = c(
      "#0066CC",  # Blue
      "#FF6B35",  # Orange
      "#06A77D",  # Teal
      "#D62828",  # Red
      "#8338EC",  # Purple
      "#FFBA08"   # Yellow
    ),
    bodl = c(
      "#9E2900",  # CentralRed
      "#F58C05",  # BanksiaOrange
      "#759C78",  # EucalyptGreen
      "#68B0E3",  # FairyWrenBlue
      "#B84700",  # KangarooRed
      "#FFB814",  # DesertFlameYellow
      "#75B24D",  # LorikeetGreen
      "#85B8DB",  # BudgerigarBlue
      "#E05C0A",  # BushTomatoRed
      "#7869E8",  # CoralPeaPurple
      "#368AC4",  # KingfisherBlue
      "#D6E8F2",  # KookaburraBlue
      "#2B3036",  # CrowBlack
      "#3D3D40",  # WillieWagtailBlack
      "#EBEDEB",  # EmuGrey
      "#EAEEE8"   # GhostGumGrey
    )
  )
  
  # Get the selected palette
  if (!palette %in% names(palettes)) {
    stop("Palette '", palette, "' not found. Available palettes: ", 
         paste(names(palettes), collapse = ", "))
  }
  
  selected_palette <- palettes[[palette]]
  
  # Return appropriate number of colours
  if (is.null(n)) {
    return(selected_palette)
  } else if (type == "discrete") {
    if (n > length(selected_palette)) {
      warning("Requested ", n, " colours but palette only has ", 
              length(selected_palette), " colours. Recycling colours.")
      return(rep(selected_palette, length.out = n))
    }
    return(selected_palette[1:n])
  } else {
    # For continuous, interpolate
    return(grDevices::colorRampPalette(selected_palette)(n))
  }
}


#' Yassine's Colour Scale for ggplot2 (Colour/Color)
#'
#' Apply Yassine's colour palettes to ggplot2 plots (for colour aesthetic).
#'
#' @param palette Name of the palette. Options: "main", "cool", "warm", "contrast", "bodl"
#' @param discrete Whether to use discrete (TRUE) or continuous (FALSE) colours
#' @param reverse Whether to reverse the colour order
#' @param ... Additional arguments passed to ggplot2 scale functions
#'
#' @return A ggplot2 scale object
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_yassine()
#' }
scale_color_yassine <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  
  pal <- yassine_colors(palette = palette)
  
  if (reverse) {
    pal <- rev(pal)
  }
  
  if (discrete) {
    # Store palette colors to avoid recalculation
    palette_func <- function(n) {
      if (n > length(pal)) {
        warning("Requested ", n, " colours but palette only has ", 
                length(pal), " colours. Recycling colours.")
        return(rep(pal, length.out = n))
      }
      pal[1:n]
    }
    ggplot2::discrete_scale("colour", paste0("yassine_", palette), 
                            palette = palette_func, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal, ...)
  }
}


#' Yassine's Colour Scale for ggplot2 (Fill)
#'
#' Apply Yassine's colour palettes to ggplot2 plots (for fill aesthetic).
#'
#' @param palette Name of the palette. Options: "main", "cool", "warm", "contrast", "bodl"
#' @param discrete Whether to use discrete (TRUE) or continuous (FALSE) colours
#' @param reverse Whether to reverse the colour order
#' @param ... Additional arguments passed to ggplot2 scale functions
#'
#' @return A ggplot2 scale object
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
#'   geom_bar() +
#'   scale_fill_yassine()
#' }
scale_fill_yassine <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  
  pal <- yassine_colors(palette = palette)
  
  if (reverse) {
    pal <- rev(pal)
  }
  
  if (discrete) {
    # Store palette colors to avoid recalculation
    palette_func <- function(n) {
      if (n > length(pal)) {
        warning("Requested ", n, " colours but palette only has ", 
                length(pal), " colours. Recycling colours.")
        return(rep(pal, length.out = n))
      }
      pal[1:n]
    }
    ggplot2::discrete_scale("fill", paste0("yassine_", palette), 
                           palette = palette_func, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal, ...)
  }
}
