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
      Blue = "#0066CC",
      Orange = "#FF6B35",
      DarkBlue = "#004E89",
      DarkOrange = "#F77F00",
      Teal = "#06A77D",
      Red = "#D62828",
      Purple = "#8338EC",
      Yellow = "#FFBA08"
    ),
    cool = c(
      Blue = "#0066CC",
      DarkBlue = "#004E89",
      Teal = "#06A77D",
      LightBlue = "#00B4D8",
      Purple = "#8338EC",
      Cyan = "#4CC9F0"
    ),
    warm = c(
      Orange = "#FF6B35",
      DarkOrange = "#F77F00",
      Red = "#D62828",
      Yellow = "#FFBA08",
      BrightRed = "#E63946",
      Gold = "#FCA311"
    ),
    contrast = c(
      Blue = "#0066CC",
      Orange = "#FF6B35",
      Teal = "#06A77D",
      Red = "#D62828",
      Purple = "#8338EC",
      Yellow = "#FFBA08"
    ),
    bodl = c(
      CentralRed = "#9E2900",
      BanksiaOrange = "#F58C05",
      EucalyptGreen = "#759C78",
      FairyWrenBlue = "#68B0E3",
      KangarooRed = "#B84700",
      DesertFlameYellow = "#FFB814",
      LorikeetGreen = "#75B24D",
      BudgerigarBlue = "#85B8DB",
      BushTomatoRed = "#E05C0A",
      CoralPeaPurple = "#7869E8",
      KingfisherBlue = "#368AC4",
      KookaburraBlue = "#D6E8F2",
      CrowBlack = "#2B3036",
      WillieWagtailBlack = "#3D3D40",
      EmuGrey = "#EBEDEB",
      GhostGumGrey = "#EAEEE8"
    )
  )
  
  # Make colour names available globally
  invisible(
    lapply(palettes, function(pal) {
      list2env(as.list(pal), globalenv())
    })
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
