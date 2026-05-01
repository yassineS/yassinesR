## theme_majorelle() — ggplot2 theme matching the Majorelle (Apple-skinned)
## design language.

## ---- Variants -----------------------------------------------------------
##
## Each variant defines a coherent set of colours — surface plus matched
## ink tones, axis colour, scatter contour, qualitative palette, sequential
## cmap key, and the annotation/highlight colours used by the helpers in
## scales.R. Mirrors `THEME_VARIANTS` in python/majorelle/mpl.py.
##
## - "rabat"     — the white-walled Moroccan capital. Crisp, formal, neutral.
## - "palmeraie" — the Marrakech palm grove + Jardin Majorelle. Sun-warmed
##                 adobe ground; cobalt blue + saffron + cactus + Jardin teal
##                 in the data palette.
#' @export
majorelle_variants <- list(
  rabat = list(
    surface  = "#FFFFFF",                           # white
    ink      = "#1D1D1F",                           # near-black title/labels
    ink_soft = "#86868B",                           # gray-48 subtitle/ticks
    axis     = "#424245",                           # gray-80 spines
    edge     = "#1D1D1F",                           # scatter contour
    data     = majorelle_pal_qual,
    cmap     = "blues",
    anomaly  = "#E2725B",                           # terracotta callout
    target   = "#2D6A4F",                           # green callout
    focus    = "#3820ED",                           # highlight focus
    demoted  = "#86868B"                            # highlight demoted
  ),
  palmeraie = list(
    surface  = "#D97461",                           # sun-warmed terracotta
    ink      = "#FAF6F0",                           # warm off-white
    ink_soft = "#F5D5C9",                           # PAL_TERRACOTTAS[1]
    axis     = "#FBEFEB",                           # PAL_TERRACOTTAS[0]
    edge     = "#FFFFFF",                           # white scatter contour on warm surface
    data     = majorelle_pal_qual_on_terracotta,
    cmap     = "greens",
    anomaly  = "#FFD700",                           # gold (terracotta vanishes)
    target   = "#2D6A4F",
    focus    = "#3820ED",
    demoted  = "#3A1812"                            # deep terracotta brown
  )
)

## Active-variant accessor — read by the annotate_*/highlight_series
## helpers and by use_majorelle_defaults().
#' @export
majorelle_active_variant <- function() {
  v <- getOption("majorelle.variant", "rabat")
  if (!v %in% names(majorelle_variants)) "rabat" else v
}
##
## Conventions
##   * theme_classic() base — no grid lines by default, clean axes
##   * Typography: SF Pro Text on macOS where available; falls back to
##     "Helvetica Neue" (the explicit fallback in the design's system stack).
##   * Display sizes (>= 20px effectively) use SF Pro Display / Helvetica Neue
##     bold for plot.title and plot.tag — matches the design's optical sizing rule.
##   * Plot and panel share a single white surface — Apple-style.
##   * Axes are gray-80 (#424245) for clear, confident framing.
##   * Negative letter-spacing on display text isn't expressible in ggplot2's
##     theme (no tracking control), so we lean on weight + size for hierarchy.

#' Majorelle ggplot2 theme
#'
#' @param size.rel Multiplier applied to all relative text sizes. Default 1.
#' @param base_size Base font size in points. Default 12.
#' @param base_family Body font family. Defaults to "Helvetica Neue" — the
#'   design's documented fallback when SF Pro Text isn't registered.
#' @param display_family Font family for display-class text (title, tag). The
#'   design uses SF Pro Display for >= 20px sizes; defaults to `base_family`.
#' @param surface Background fill for plot AND panel. Defaults to the active
#'   base's surface; pass an explicit colour to override without changing
#'   the rest of the base's tokens.
#' @param base `"rabat"` (white surface, the Moroccan capital) or
#'   `"palmeraie"` (sun-warmed terracotta, the Marrakech palm grove).
#'   Selects the theme base — surface, ink, axes, scatter contour. Defaults
#'   to match `variant` for the single-knob preset.
#' @param variant `"rabat"` or `"palmeraie"`. Selects the qualitative
#'   palette and palette-dependent callout colours (anomaly / target /
#'   focus / demoted). The base and variant are decoupled so you can mix
#'   them: `theme_majorelle(base = "rabat", variant = "palmeraie")` gives a
#'   white-walled plot using the palmeraie palette.
#' @param grid Add faint horizontal (`"y"`), vertical (`"x"`), both (`"xy"`)
#'   or no (default `"none"`) grid lines.
#' @export
theme_majorelle <- function(size.rel    = 1,
                            base_size   = 12,
                            base_family = "Helvetica Neue",
                            display_family = base_family,
                            variant     = majorelle_active_variant(),
                            base        = NULL,
                            surface     = NULL,
                            grid        = c("none", "y", "x", "xy")) {
  grid <- match.arg(grid)
  if (is.null(base)) base <- variant
  base_cfg <- majorelle_variants[[base]]
  pal_cfg  <- majorelle_variants[[variant]]
  if (is.null(surface)) surface <- base_cfg$surface

  ink         <- base_cfg$ink                     # title / axis labels
  ink_soft    <- base_cfg$ink_soft                # subtitle / ticks
  axis_col    <- base_cfg$axis                    # spines / tick marks
  strip_bg    <- majorelle_colours$light_gray     # #F5F5F7 (default base)
  grid_col    <- majorelle_colours$gray_16        # #D2D2D7

  th <- ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(

      ## Display-class text (title, tag) — weight 400 per DESIGN.md (display
      ## sizes get their visual weight from size + tight tracking, not bold).
      plot.title    = ggplot2::element_text(size = ggplot2::rel(1.8 * size.rel),
                                            face = "plain",
                                            family = display_family,
                                            colour = ink,
                                            margin = ggplot2::margin(b = 8)),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(1.25 * size.rel),
                                            colour = ink_soft,
                                            margin = ggplot2::margin(b = 16)),
      plot.caption  = ggplot2::element_text(size = ggplot2::rel(0.9 * size.rel),
                                            colour = ink_soft, hjust = 0,
                                            margin = ggplot2::margin(t = 12)),
      plot.tag      = ggplot2::element_text(size = ggplot2::rel(1.4 * size.rel),
                                            face = "plain",
                                            family = display_family,
                                            colour = ink),

      plot.title.position    = "plot",
      plot.caption.position  = "plot",

      ## Axes — gray-80 lines, generous text. Axis-title margins follow the
      ## DESIGN.md "Plot inner padding" convention (axis label area ~48px).
      axis.title    = ggplot2::element_text(size = ggplot2::rel(1.2 * size.rel),
                                            colour = ink),
      axis.title.x  = ggplot2::element_text(margin = ggplot2::margin(t = 16)),
      axis.title.y  = ggplot2::element_text(margin = ggplot2::margin(r = 16)),
      axis.text     = ggplot2::element_text(size = ggplot2::rel(1.0 * size.rel),
                                            colour = ink_soft),
      ## Push tick labels away from the axis so log_ticks() (which extend
      ## outward up to ~12pt) don't crowd them.
      axis.text.x   = ggplot2::element_text(margin = ggplot2::margin(t = 14)),
      axis.text.x.top    = ggplot2::element_text(margin = ggplot2::margin(b = 14)),
      axis.text.y   = ggplot2::element_text(margin = ggplot2::margin(r = 14)),
      axis.text.y.right  = ggplot2::element_text(margin = ggplot2::margin(l = 14)),
      axis.ticks    = ggplot2::element_line(colour = axis_col, linewidth = 0.55),
      axis.ticks.length = grid::unit(4, "pt"),
      axis.line     = ggplot2::element_line(colour = axis_col, linewidth = 0.55),

      ## Legend
      legend.title       = ggplot2::element_text(size = ggplot2::rel(1.1 * size.rel),
                                                 face = "bold", colour = ink),
      legend.text        = ggplot2::element_text(size = ggplot2::rel(1.0 * size.rel),
                                                 colour = ink_soft),
      legend.background  = ggplot2::element_blank(),
      legend.key         = ggplot2::element_blank(),
      legend.key.height  = grid::unit(1.1 * size.rel, "line"),
      legend.key.width   = grid::unit(1.1 * size.rel, "line"),
      legend.position    = "right",
      legend.box.spacing = grid::unit(24, "pt"),    # 24px legend padding
      legend.spacing.y   = grid::unit(8, "pt"),
      legend.spacing.x   = grid::unit(12, "pt"),

      ## Strips (facets) — typography-only, no background block
      strip.background      = ggplot2::element_blank(),
      strip.text            = ggplot2::element_text(size = ggplot2::rel(1.1 * size.rel),
                                                    face = "bold", colour = ink,
                                                    hjust = 0,
                                                    margin = ggplot2::margin(t = 0, b = 6)),
      strip.placement       = "outside",
      strip.switch.pad.grid = grid::unit(0, "pt"),
      strip.switch.pad.wrap = grid::unit(0, "pt"),

      ## Backgrounds — single Apple-white surface
      plot.background    = ggplot2::element_rect(fill = surface, colour = NA),
      panel.background   = ggplot2::element_rect(fill = surface, colour = NA),
      panel.border       = ggplot2::element_blank(),

      ## Grid off by default
      panel.grid         = ggplot2::element_blank(),
      panel.grid.major   = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),

      panel.spacing.x    = grid::unit(14, "pt"),
      panel.spacing.y    = grid::unit(6, "pt"),

      ## Outer plot margin: title-area 32px top per DESIGN.md.
      plot.margin = ggplot2::margin(t = 32, r = 24, b = 16, l = 24)
    )

  if (grid != "none") {
    g <- ggplot2::element_line(colour = grid_col, linewidth = 0.3)
    if (grid %in% c("y", "xy")) th <- th + ggplot2::theme(panel.grid.major.y = g)
    if (grid %in% c("x", "xy")) th <- th + ggplot2::theme(panel.grid.major.x = g)
  }

  th
}

#' Apply the Rabat (white-walled) theme base.
#'
#' The function name fixes the theme base; `variant` picks the qualitative
#' palette (default rabat). Pass `variant = "palmeraie"` to keep the rabat
#' surface but use the palmeraie palette (no terracotta in the data colour
#' cycle, plus the Jardin teal accent).
#' @param variant Qualitative palette: `"rabat"` (default) or `"palmeraie"`.
#' @param ... Forwarded to [theme_majorelle()].
#' @export
theme_rabat <- function(variant = "rabat", ...) {
  theme_majorelle(variant = variant, base = "rabat", ...)
}

#' Apply the Palmeraie (sun-warmed terracotta) theme base.
#'
#' The function name fixes the theme base; `variant` picks the qualitative
#' palette (default palmeraie). Pass `variant = "rabat"` to keep the warm
#' surface but use the full rabat 12-colour palette.
#' @param variant Qualitative palette: `"palmeraie"` (default) or `"rabat"`.
#' @param ... Forwarded to [theme_majorelle()].
#' @export
theme_palmeraie <- function(variant = "palmeraie", ...) {
  theme_majorelle(variant = variant, base = "palmeraie", ...)
}

#' Convenience: set Majorelle as the session-wide default colour scales.
#'
#' Records the chosen variant as a session option (`majorelle.variant`), so the
#' annotate_* and highlight_series helpers in scales.R pick up the matching
#' anomaly / target / focus / demoted colours automatically.
#' @param variant `"rabat"` (white-walled default) or `"palmeraie"` (sun-warmed
#'   adobe).
#' @export
use_majorelle_defaults <- function(variant = "rabat") {
  cfg <- majorelle_variants[[variant]]
  options(majorelle.variant = variant)
  options(
    ggplot2.discrete.colour   = unname(cfg$data),
    ggplot2.discrete.fill     = unname(cfg$data),
    ggplot2.continuous.colour = function(...) scale_colour_majorelle_c(palette = cfg$cmap, ...),
    ggplot2.continuous.fill   = function(...) scale_fill_majorelle_c(palette = cfg$cmap, ...)
  )
  invisible(NULL)
}

#' Default `geom_point` to a filled circle (shape 21) with a near-black contour
#' for the duration of the session. Maps the `fill` aesthetic to the Majorelle
#' qualitative palette. Contour colour follows the active variant.
#' @param size point size (default 3.4)
#' @param stroke contour width (default 0.4)
#' @param colour contour colour. Defaults to the active variant's edge colour.
#' @export
use_majorelle_points <- function(size = 3.4, stroke = 0.4,
                                 colour = NULL) {
  if (is.null(colour)) colour <- majorelle_variants[[majorelle_active_variant()]]$edge
  ggplot2::update_geom_defaults("point", list(
    shape  = 21,
    size   = size,
    stroke = stroke,
    colour = colour
  ))
  invisible(NULL)
}
