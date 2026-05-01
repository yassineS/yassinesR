## ggplot2 scales using the Majorelle palette

#' Discrete colour/fill scales — use the active variant's qualitative palette
#' (or pass `variant=` to force a specific palette regardless of session
#' state).
#' @export
scale_colour_majorelle <- function(..., variant = majorelle_active_variant()) {
  pal <- majorelle_variants[[variant]]$data
  ggplot2::discrete_scale("colour", "majorelle",
                          palette = function(n) unname(pal)[seq_len(n)],
                          ...)
}

#' @export
scale_color_majorelle <- scale_colour_majorelle

#' @export
scale_fill_majorelle <- function(..., variant = majorelle_active_variant()) {
  pal <- majorelle_variants[[variant]]$data
  ggplot2::discrete_scale("fill", "majorelle",
                          palette = function(n) unname(pal)[seq_len(n)],
                          ...)
}

#' Continuous (sequential) scales — Majorelle Blues by default.
#'
#' Use `palette` to switch to one of the brand sequential alternates: `"blues"`
#' (default), `"golds"`, `"terracottas"`, `"greens"`.
#' @export
scale_colour_majorelle_c <- function(..., palette = "blues", guide = "colourbar") {
  ggplot2::scale_colour_gradientn(colours = .seq_palette(palette), guide = guide, ...)
}

#' @export
scale_color_majorelle_c <- scale_colour_majorelle_c

#' @export
scale_fill_majorelle_c <- function(..., palette = "blues", guide = "colourbar") {
  ggplot2::scale_fill_gradientn(colours = .seq_palette(palette), guide = guide, ...)
}

.seq_palette <- function(name = c("blues", "golds", "terracottas", "greens")) {
  name <- match.arg(name)
  switch(name,
    blues       = majorelle_pal_blues,
    golds       = majorelle_pal_golds,
    terracottas = majorelle_pal_terracottas,
    greens      = majorelle_pal_greens
  )
}

#' Diverging scales — terracotta to Majorelle by default.
#'
#' `palette`: `"brand"` (default; terracotta ↔ blue), `"gold"` (terracotta ↔
#' green), `"cool"` (green ↔ blue, color-blind safer).
#' @export
scale_colour_majorelle_d2 <- function(..., palette = "brand", guide = "colourbar") {
  ggplot2::scale_colour_gradientn(colours = .div_palette(palette), guide = guide, ...)
}

#' @export
scale_fill_majorelle_d2 <- function(..., palette = "brand", guide = "colourbar") {
  ggplot2::scale_fill_gradientn(colours = .div_palette(palette), guide = guide, ...)
}

.div_palette <- function(name = c("brand", "gold", "cool")) {
  name <- match.arg(name)
  switch(name,
    brand = majorelle_pal_diverging,
    gold  = majorelle_pal_diverging_gold,
    cool  = majorelle_pal_diverging_cool
  )
}

## ----- log10 helpers ------------------------------------------------------

#' Major breaks for a base-10 log axis: 1, 2, 5 × 10^k within the data range.
#' @export
breaks_log_majorelle <- function() {
  function(limits) {
    rng <- range(limits, na.rm = TRUE, finite = TRUE)
    lrng <- log10(rng)
    decades <- seq(floor(lrng[1]) - 1, ceiling(lrng[2]) + 1)
    cands <- as.numeric(outer(c(1, 2, 5), 10 ^ decades))
    eps <- 1e-9
    cands[cands >= rng[1] * (1 - eps) & cands <= rng[2] * (1 + eps)]
  }
}

#' Minor breaks for a base-10 log axis: integer multiples plus half-decades, so
#' the visual density of minor ticks stays uniform across decades — no gaps
#' between 1-2 or 10-20.
#' @export
minor_breaks_log_majorelle <- function() {
  function(limits) {
    rng <- log10(range(limits, na.rm = TRUE, finite = TRUE))
    decades <- seq(floor(rng[1]) - 1, ceiling(rng[2]) + 1)
    mults <- sort(unique(c(seq(1, 9, by = 1), 1.5, 2.5, 7.5)))
    vals <- as.numeric(outer(mults, 10 ^ decades))
    vals[vals >= 10 ^ rng[1] & vals <= 10 ^ rng[2]]
  }
}

#' Majorelle log10 scales — drop-in replacements for `scale_x_log10()` /
#' `scale_y_log10()`. Uses pretty `1, 2, 5 × 10^k` major breaks and a sensible
#' label formatter. For tiered minor ticks, add `annotation_logticks_majorelle()`
#' as a layer.
#'
#' @inheritParams ggplot2::scale_x_log10
#' @export
scale_x_log10_majorelle <- function(breaks = breaks_log_majorelle(),
                                    labels = scales::label_number(drop0trailing = TRUE),
                                    ...) {
  ggplot2::scale_x_log10(breaks = breaks, labels = labels, ...)
}

#' @rdname scale_x_log10_majorelle
#' @export
scale_y_log10_majorelle <- function(breaks = breaks_log_majorelle(),
                                    labels = scales::label_number(drop0trailing = TRUE),
                                    ...) {
  ggplot2::scale_y_log10(breaks = breaks, labels = labels, ...)
}

## ggplot2 4.0's `annotation_logticks()` silently fails to render when the
## main plot has a discrete colour aesthetic (e.g., `aes(colour = group)`).
## We sidestep that with a custom Geom that draws ticks directly via grid
## units, anchored to the panel edges (npc = 0 / 1) — independent of any
## aesthetic mapping in the host plot.

## Tick positions: long at 1, mid at 5, short at every other integer mantissa
## plus 1.5/2.5/7.5 so the visual density stays uniform inside the 1-2 and
## 10-20 stretches that integer-only ticks leave empty.
.log_tick_positions <- function(data_rng) {
  decades <- seq(floor(log10(data_rng[1])) - 1, ceiling(log10(data_rng[2])) + 1)
  short_mantissas <- c(2, 3, 4, 6, 7, 8, 9, 1.5, 2.5, 7.5)
  longs <- as.numeric(outer(1, 10 ^ decades))
  mids  <- as.numeric(outer(5, 10 ^ decades))
  shorts <- as.numeric(outer(short_mantissas, 10 ^ decades))
  bind <- function(values, tier) data.frame(value = values, tier = tier,
                                             stringsAsFactors = FALSE)
  out <- rbind(bind(longs, "long"), bind(mids, "mid"), bind(shorts, "short"))
  out <- out[out$value >= data_rng[1] & out$value <= data_rng[2], , drop = FALSE]
  out[order(out$value), , drop = FALSE]
}

GeomLogTicksMajorelle <- ggplot2::ggproto("GeomLogTicksMajorelle", ggplot2::Geom,
  required_aes = character(),
  default_aes  = ggplot2::aes(colour = "#464557", linewidth = 0.5),
  handle_na = function(self, data, params) data,
  draw_panel = function(self, data, panel_params, coord,
                        sides = "bl",
                        long = 7, mid = 4.5, short = 2.5,
                        colour = "#464557", linewidth = 0.5) {

    grobs <- list()

    push <- function(side) {
      is_x <- side %in% c("b", "t")
      lim <- if (is_x) panel_params$x.range else panel_params$y.range
      if (is.null(lim) || !all(is.finite(lim))) return()
      data_rng <- 10 ^ lim
      tp <- .log_tick_positions(data_rng)
      if (!nrow(tp)) return()
      npc <- (log10(tp$value) - lim[1]) / diff(lim)
      lengths <- ifelse(tp$tier == "long",  long,
                 ifelse(tp$tier == "mid",   mid,   short))

      ## Draw ticks OUTSIDE the panel — below the bottom axis, left of the
      ## left axis (and the mirrored sides). Pair with coord_cartesian
      ## (clip = "off"), which `log_ticks()` already adds.
      for (i in seq_len(nrow(tp))) {
        len_u <- grid::unit(lengths[i], "pt")
        if (is_x) {
          base_y <- if (side == "b") grid::unit(0, "npc") else grid::unit(1, "npc")
          tip_y  <- if (side == "b") base_y - len_u        else base_y + len_u
          grobs[[length(grobs) + 1L]] <<- grid::segmentsGrob(
            x0 = grid::unit(npc[i], "npc"), x1 = grid::unit(npc[i], "npc"),
            y0 = base_y, y1 = tip_y,
            gp = grid::gpar(col = colour, lwd = linewidth * ggplot2::.pt,
                            lineend = "butt")
          )
        } else {
          base_x <- if (side == "l") grid::unit(0, "npc") else grid::unit(1, "npc")
          tip_x  <- if (side == "l") base_x - len_u        else base_x + len_u
          grobs[[length(grobs) + 1L]] <<- grid::segmentsGrob(
            x0 = base_x, x1 = tip_x,
            y0 = grid::unit(npc[i], "npc"), y1 = grid::unit(npc[i], "npc"),
            gp = grid::gpar(col = colour, lwd = linewidth * ggplot2::.pt,
                            lineend = "butt")
          )
        }
      }
    }
    for (s in strsplit(sides, "")[[1]]) push(s)

    if (!length(grobs)) return(grid::nullGrob())
    ## Use gList so the panel viewport's clipping is bypassed by individual
    ## segmentsGrobs that draw to npc < 0 / > 1 (with coord_cartesian
    ## clip = "off" set by log_ticks()).
    grid::gTree(children = do.call(grid::gList, grobs))
  }
)

#' Tiered log ticks for the bottom and/or left axes.
#'
#' Renders ticks at 1 × 10^k (long), 5 × 10^k (mid), and 2-9 × 10^k (short)
#' within the visible range of a `scale_*_log10()` axis. Implemented via a
#' custom Geom anchored to panel edges, so it renders correctly even when the
#' main plot uses a discrete colour aesthetic (where ggplot2 4.0's
#' `annotation_logticks()` silently fails). Returns a list including
#' `coord_cartesian(clip = "off")` so the ticks aren't clipped.
#'
#' @param sides character containing any of "b", "l", "t", "r". Default "bl".
#' @param long,mid,short tick lengths in points.
#' @param colour tick colour.
#' @param linewidth tick line width.
#' @export
log_ticks <- function(sides = "bl",
                      long = 12, mid = 8, short = 5,
                      colour = majorelle_colours$on_surface_variant,
                      linewidth = 0.55) {
  layer <- ggplot2::layer(
    data        = data.frame(x = 1, y = 1),
    mapping     = NULL,
    stat        = "identity",
    geom        = GeomLogTicksMajorelle,
    position    = "identity",
    show.legend = FALSE,
    inherit.aes = FALSE,
    params      = list(
      sides = sides, long = long, mid = mid, short = short,
      colour = colour, linewidth = linewidth, na.rm = TRUE
    )
  )
  list(layer, ggplot2::coord_cartesian(clip = "off"))
}

#' Alias for users reaching for an `annotation_logticks()`-style name.
#' @export
annotation_logticks_majorelle <- function(sides = "bl", ...) log_ticks(sides = sides, ...)

## ----- Annotation helpers --------------------------------------------------

#' Annotate a point with the active variant's anomaly colour (terracotta on
#' the default surface, gold on the terracotta surface so the callout doesn't
#' blend into the background).
#'
#' Convention from DESIGN.md §Plot Component Conventions: anomaly /
#' warning annotations use `{colors.tertiary}`; target / "good" annotations
#' use `{colors.emphasis}`. Pass `colour=` to override the variant default.
#'
#' @param x,y data coordinates of the point being called out.
#' @param label the annotation text.
#' @param hjust,vjust label justification.
#' @param nudge_x,nudge_y offset of the label from the point (data units).
#' @param colour override the variant default callout colour.
#' @export
annotate_anomaly <- function(x, y, label,
                             hjust = 0, vjust = 0,
                             nudge_x = 0, nudge_y = 0,
                             size = 3.6,
                             colour = NULL) {
  if (is.null(colour))
    colour <- majorelle_variants[[majorelle_active_variant()]]$anomaly
  list(
    ggplot2::annotate("segment",
                      x = x, y = y,
                      xend = x + nudge_x, yend = y + nudge_y,
                      colour = colour,
                      linewidth = 0.5),
    ggplot2::annotate("text",
                      x = x + nudge_x, y = y + nudge_y, label = label,
                      colour = colour,
                      fontface = "bold", size = size,
                      hjust = hjust, vjust = vjust)
  )
}

#' Annotate a point with the active variant's target colour (lush green by
#' default).
#' @inheritParams annotate_anomaly
#' @export
annotate_target <- function(x, y, label,
                            hjust = 0, vjust = 0,
                            nudge_x = 0, nudge_y = 0,
                            size = 3.6,
                            colour = NULL) {
  if (is.null(colour))
    colour <- majorelle_variants[[majorelle_active_variant()]]$target
  list(
    ggplot2::annotate("segment",
                      x = x, y = y,
                      xend = x + nudge_x, yend = y + nudge_y,
                      colour = colour,
                      linewidth = 0.5),
    ggplot2::annotate("text",
                      x = x + nudge_x, y = y + nudge_y, label = label,
                      colour = colour,
                      fontface = "bold", size = size,
                      hjust = hjust, vjust = vjust)
  )
}

## ----- Single-series highlight --------------------------------------------

#' Build a colour vector that highlights one (or more) series and demotes the
#' rest. Supply to `scale_colour_manual()` / `scale_fill_manual()` to call
#' attention to a focus series. Default colours follow the active variant
#' (Majorelle Blue + gray-48 for `rabat`; Majorelle Blue + deep terracotta-
#' brown for `palmeraie`).
#'
#' @param n total number of series.
#' @param focus integer index (1-based) or vector of indices to highlight.
#' @param focus_colour highlight colour. Defaults to the active variant's
#'   focus colour.
#' @param others_colour colour for non-focus series. Defaults to the active
#'   variant's demoted colour.
#' @export
highlight_series <- function(n, focus = 1L,
                             focus_colour = NULL,
                             others_colour = NULL) {
  cfg <- majorelle_variants[[majorelle_active_variant()]]
  if (is.null(focus_colour))  focus_colour  <- cfg$focus
  if (is.null(others_colour)) others_colour <- cfg$demoted
  out <- rep(others_colour, n)
  out[focus] <- focus_colour
  out
}
