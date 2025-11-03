# yassinesR

<!-- badges: start -->
<!-- badges: end -->

An R package providing custom ggplot2 themes, colour palettes, and helper functions for data visualization and analysis.

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

### Colour Palettes

Access carefully curated colour palettes optimized for data visualization:

``` r
# Get all colours from the main palette
yassine_colors("main")

# Get colours from different palettes
yassine_colors("cool", n = 5)
yassine_colors("warm", n = 4)
yassine_colors("contrast", n = 6)

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

## Development

This package follows best practices as outlined in [R Packages (2e)](https://r-pkgs.org/) by Hadley Wickham and Jennifer Bryan.

## License

This project is licensed under the Apache License 2.0 - see the [LICENSE](LICENSE) file for details.

