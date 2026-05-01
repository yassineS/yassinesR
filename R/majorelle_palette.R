## Majorelle palette (Apple-skinned)
## Brand: typographic scaffolding from Apple HIG, palette inspired by
## Yves Saint Laurent's Jardin Majorelle in Marrakech.

majorelle_colours <- list(
  ## Brand
  primary             = "#3820ED",  # Majorelle Blue
  primary_hover       = "#2A18C4",
  primary_light       = "#5C46F0",  # link on dark
  secondary           = "#FFD700",  # Majorelle Gold
  secondary_hover     = "#E6C200",
  tertiary            = "#E2725B",  # Terracotta
  tertiary_hover      = "#C75D45",
  green               = "#2D6A4F",  # Lush Green
  green_soft          = "#52796F",

  ## Surfaces
  white               = "#FFFFFF",
  light_gray          = "#F5F5F7",
  near_black          = "#1D1D1F",
  pure_black          = "#000000",

  ## Functional grays
  gray_04             = "#FBFBFD",
  gray_08             = "#F5F5F7",
  gray_16             = "#D2D2D7",
  gray_48             = "#86868B",
  gray_80             = "#424245",
  gray_90             = "#1D1D1F"
)

## ---- 9.3 Qualitative — 12-category palette --------------------------------
#' Majorelle 12-category palette
#'
#' Ordered: brand primary first, then **gold → terracotta → green** (the
#' user-approved order for the first four slots), then the §9.3 complements
#' for higher-cardinality categorical encodings. Subset to the first N for
#' fewer categories.
#'
#' @export
majorelle_pal_qual <- c(
  "#3820ED",  #  1 Majorelle Blue
  "#FFD700",  #  2 Gold
  "#E2725B",  #  3 Terracotta
  "#2D6A4F",  #  4 Lush Green
  "#5C46F0",  #  5 Periwinkle
  "#9C4633",  #  6 Burnt Sienna
  "#52796F",  #  7 Sage
  "#B89800",  #  8 Antique Gold
  "#7A4FB5",  #  9 Aubergine Violet
  "#3F88C5",  # 10 Atlas Sky
  "#D6A99A",  # 11 Adobe Pink
  "#1D1D1F"   # 12 Near Black
)

#' Majorelle qualitative palette — terracotta-surface variant
#'
#' Tuned to evoke Jardin Majorelle on a sun-warmed adobe ground:
#' cobalt blue + saffron + cactus green + the iconic turquoise trim,
#' all reading cleanly against a warm terracotta surface. Terracotta-
#' family hues (terracotta, burnt sienna, adobe pink) are dropped — the
#' surface itself now carries that hue.
#' @export
majorelle_pal_qual_on_terracotta <- c(
  "#3820ED",  # Majorelle Blue
  "#FFD700",  # Gold
  "#2D6A4F",  # Lush Green
  "#2A9D8F",  # Jardin Teal       (Majorelle trim / bench)
  "#5C46F0",  # Periwinkle
  "#52796F",  # Sage
  "#B89800",  # Antique Gold
  "#7A4FB5",  # Aubergine Violet
  "#1D1D1F"   # Near Black
)

## ---- 9.1 Sequential — converging palettes ---------------------------------
#' Sequential blues, anchored on Majorelle Blue (9 stops)
#' @export
majorelle_pal_blues <- c(
  "#F4F2FE", "#E0DBFC", "#C2B7F8", "#9C8AF3", "#715BEC",
  "#4A2EE8", "#3820ED", "#2A18C4", "#1A0E7A"
)

#' Sequential golds (9 stops)
#' @export
majorelle_pal_golds <- c(
  "#FFFBE5", "#FFF3B3", "#FFE980", "#FFDE4D", "#FFD700",
  "#E6BD00", "#B89800", "#7A6500", "#3D3300"
)

#' Sequential terracottas (9 stops)
#' @export
majorelle_pal_terracottas <- c(
  "#FBEFEB", "#F5D5C9", "#EEB7A4", "#E89A80", "#E2725B",
  "#C75D45", "#9C4633", "#6E3024", "#3A1812"
)

#' Sequential greens (9 stops)
#' @export
majorelle_pal_greens <- c(
  "#EAF3EE", "#C6DFCF", "#9BC8AB", "#6FB089", "#4A9670",
  "#2D6A4F", "#214F3B", "#163528", "#0B1B14"
)

#' Default sequential palette — alias for `majorelle_pal_blues`
#' @export
majorelle_pal_seq <- majorelle_pal_blues

## ---- 9.2 Diverging palettes -----------------------------------------------
#' Brand diverging: terracotta -> cream -> Majorelle Blue (11 stops)
#' @export
majorelle_pal_diverging <- c(
  "#9C4633", "#C75D45", "#E2725B", "#EE9F8B", "#F5CFC2",
  "#FAF6F0",
  "#C8BFFA", "#9C8AF3", "#4A2EE8", "#3820ED", "#1A0E7A"
)

#' Gold diverging: terracotta -> cream -> green
#' @export
majorelle_pal_diverging_gold <- c(
  "#9C4633", "#C75D45", "#E2725B", "#EE9F8B", "#FAE8D9",
  "#FAF6F0",
  "#DCEFE1", "#6FB089", "#4A9670", "#2D6A4F", "#163528"
)

#' Cool diverging (color-blind safer): green -> cream -> Majorelle Blue
#' @export
majorelle_pal_diverging_cool <- c(
  "#214F3B", "#2D6A4F", "#4A9670", "#9BC8AB", "#DCEFE1",
  "#FAF6F0",
  "#C8BFFA", "#9C8AF3", "#4A2EE8", "#3820ED", "#1A0E7A"
)

#' Default diverging — alias for `majorelle_pal_diverging`
#' @export
majorelle_pal_div <- majorelle_pal_diverging
