# yassinesR

<!-- badges: start -->
<!-- badges: end -->

An R package providing custom ggplot2 themes, colour palettes, and helper
functions for data visualisation and analysis. Two coexisting theme systems:

* **`theme_yassine()`** — the original yassinesR theme.
* **`theme_majorelle()`** — the Majorelle design language (Apple typographic
  scaffolding + Jardin Majorelle palette), with `rabat` and `palmeraie`
  variants and a complete helper suite (log ticks, annotations, highlight,
  raincloud).

## Installation

You can install the development version of yassinesR from [GitHub](https://github.com/yassineS/yassinesR) with:

``` r
# install.packages("devtools")
devtools::install_github("yassineS/yassinesR")
```

## Features

### Custom ggplot2 Theme

The package includes `theme_yassine()`, a clean and modern ggplot2 theme designed for scientific plots:

``` r
library(yassinesR)
library(ggplot2)

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  theme_yassine()
```

A dark version is also available with `theme_yassine_dark()`, which uses a CrowBlack background and GhostGumGrey text while preserving the visual identity:

``` r
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "#FF6B35") +
  theme_yassine_dark()
```

### Colour Palettes

Access carefully curated colour palettes optimized for data visualization:

``` r
# Get all colours from the main palette
yassine_colors("main")

# Get colours from different palettes
yassine_colors("cool", n = 5)
yassine_colors("warm", n = 4)
yassine_colors("contrast", n = 6)

# Access colors by name (all palettes use named vectors)
colors <- yassine_colors("main")
colors["Blue"]          # Returns the Blue color from the palette
colors["Orange"]        # Returns the Orange color from the palette

# BODL palette with Australian-themed names
bodl <- yassine_colors("bodl")
bodl["FairyWrenBlue"]   # Returns the FairyWrenBlue color from the palette
bodl["BanksiaOrange"]   # Returns the BanksiaOrange color from the palette

# Use in ggplot2
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 3) +
  scale_color_yassine(palette = "main") +
  theme_yassine()

# For fill aesthetic
ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar() +
  scale_fill_yassine(palette = "contrast") +
  theme_yassine()
```

Available palettes:
- `"main"` - A balanced mix of colors
- `"cool"` - Cool tones (blues, teals, purples)
- `"warm"` - Warm tones (oranges, reds, yellows)
- `"contrast"` - High contrast colors
- `"bodl"` - Australian-themed BODL colors

### Helper Functions

The package includes several utility functions:

``` r
# Format numbers for plotting
format_numbers(c(1234.567, 8901.234))
format_numbers(c(1234567, 8901234), scientific = TRUE)

# Calculate summary statistics
summary_stats(mtcars$mpg)

# Calculate percentages
percent(25, 100)  # Returns 25.0
percent(c(10, 20, 30), 100)  # Returns c(10.0, 20.0, 30.0)

# Not-in operator (complement of %in%)
1:5 %notin% c(3, 4, 5)  # Returns c(TRUE, TRUE, FALSE, FALSE, FALSE)
```

### Example Plots

The package provides example plotting functions using the iris dataset to demonstrate the color palettes:

``` r
# Scatter plot with color by Species
example_scatterplot()

# Bar plot of Species counts
example_barplot()

# Scatter plot with log-scaled x-axis
example_log_scatterplot()

# Use the log scale helper function on any plot
library(ggplot2)
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  scale_log_axis("x")  # Apply log scale to x-axis

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  scale_log_axis("y")  # Apply log scale to y-axis
```

## Majorelle design language

Majorelle is a separate theme system shipped alongside `theme_yassine()`.
It pairs Apple HIG typographic scaffolding with a palette inspired by
Yves Saint Laurent's Jardin Majorelle in Marrakech.

Majorelle has two **bases** (the surface + ink + axes scaffolding) and
two **palettes** (the qualitative data colours + callouts). They're
orthogonal — you can mix them.

| Name         | As a base (`theme_*` function name) | As a palette (`variant =` argument) |
|--------------|--------------------------------------|--------------------------------------|
| `rabat`      | white `#FFFFFF` surface, near-black ink, gray-80 axes | the standard 12-colour qualitative palette (blue / gold / terracotta / green / …) |
| `palmeraie`  | terracotta `#D97461` surface, warm off-white ink, light terracotta axes | the 9-colour Jardin-tuned palette (blue / gold / green / **teal** / …) — drops terracotta-family hues since the surface carries them |

```r
library(yassinesR)
library(ggplot2)

# The function name picks the BASE; `variant =` picks the PALETTE.
ggplot(...) + ... + theme_rabat()                          # rabat base + rabat palette
ggplot(...) + ... + theme_rabat(variant = "palmeraie")     # rabat base + palmeraie palette (no terracotta in the cycle)
ggplot(...) + ... + theme_palmeraie()                      # palmeraie base + palmeraie palette
ggplot(...) + ... + theme_palmeraie(variant = "rabat")     # palmeraie base + rabat palette (full 12-colour, terracotta will compete with the surface)

# `theme_majorelle()` is the explicit form with both knobs:
theme_majorelle(base = "rabat", variant = "palmeraie")

# Session-wide (sets the discrete + continuous palette via options()):
use_majorelle_defaults("rabat")
use_majorelle_defaults("palmeraie")
```

`use_majorelle_defaults()` records the palette in
`options("majorelle.variant")`; the annotation, highlight, and scale
helpers read this so you don't have to thread `variant=` everywhere.

### Scatter — discrete fill

```r
use_majorelle_defaults("rabat")
use_majorelle_points()  # geom_point defaults to filled circles + variant edge

ggplot(iris, aes(Sepal.Length, Sepal.Width, fill = Species)) +
  geom_point(alpha = 0.95) +
  scale_fill_majorelle() +
  theme_majorelle() +
  labs(title    = "Iris flowers, by species",
       subtitle = "Sepal dimensions across the three Iris species",
       caption  = "Source: Anderson 1935")
```

### Log-log with tiered ticks

```r
ggplot(df, aes(x, y, colour = grp, fill = grp)) +
  geom_point(alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10_majorelle() +
  scale_y_log10_majorelle(limits = c(0.17, 24)) +
  log_ticks(sides = "bl") +
  scale_colour_majorelle() +
  scale_fill_majorelle() +
  theme_majorelle()
```

`log_ticks()` renders long/mid/short ticks at 1×10ᵏ / 5×10ᵏ / 2-9×10ᵏ
mantissas plus 1.5 / 2.5 / 7.5 so the visual density stays uniform
inside the 1-2 and 10-20 stretches (where integer-only minor ticks
leave gaps). It works even when the plot uses a discrete colour
aesthetic, where ggplot2 4.0's `annotation_logticks()` silently fails.

### Continuous fill

```r
ggplot(diamonds[sample(nrow(diamonds), 4000), ],
       aes(carat, price, fill = depth)) +
  geom_point(alpha = 0.9, size = 2.4, stroke = 0.25) +
  facet_wrap(~ cut, nrow = 2) +
  scale_fill_majorelle_c() +    # palette = "blues" by default
  scale_y_continuous(labels = scales::label_dollar()) +
  theme_majorelle()
```

Pass `palette = "golds"`, `"terracottas"`, or `"greens"` for the other
sequential ramps. `scale_*_majorelle_d2()` gives diverging variants
(`"brand"` / `"gold"` / `"cool"`).

### Highlight + annotation helpers

```r
palette5 <- highlight_series(n_series = 6, focus = 3)

ggplot(ts, aes(t, value, colour = series, linewidth = series)) +
  geom_line() +
  scale_colour_manual(values = palette5, guide = "none") +
  scale_linewidth_manual(values = ifelse(seq_len(6) == 3, 1.2, 0.6),
                         guide = "none") +
  annotate_target(35, peak_y, "Peak",
                  nudge_x = 2.5, nudge_y = 1.6,
                  hjust = 0, vjust = 0) +
  annotate_anomaly(50, dip_y,  "Anomaly",
                   nudge_x = 2.5, nudge_y = -1.6,
                   hjust = 0, vjust = 1) +
  theme_majorelle()
```

`annotate_anomaly()` defaults to terracotta on `rabat` and gold on
`palmeraie` (terracotta would vanish into the surface). `annotate_target()`
is lush green on both. Pass `colour=` to override.

### Raincloud

```r
raincloud_majorelle(mpg, x = "class", y = "hwy")
```

Or compose layer-by-layer:

```r
ggplot(mpg, aes(class, hwy, fill = class)) +
  geom_raincloud_majorelle() +
  scale_fill_majorelle() +
  theme_majorelle()
```

Inspired by [njudd/ggrain](https://github.com/njudd/ggrain), composed
from `gghalves::geom_half_violin` + a narrow `geom_boxplot` + jittered
raw points so the dependency stays conda-forge friendly.

## Development

This package follows best practices as outlined in [R Packages (2e)](https://r-pkgs.org/) by Hadley Wickham and Jennifer Bryan.

After pulling Majorelle changes, run `devtools::document()` to
regenerate the `man/` pages from the roxygen comments in the new
`R/majorelle_*.R` files.

## License

This project is licensed under the Apache License 2.0 - see the [LICENSE](LICENSE) file for details.

