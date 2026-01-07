# Theme function for plot with multiple panels

Specifies theme for a plot with multiple panels.

## Usage

``` r
theme_panel(show_text = TRUE, show_ticks = TRUE)
```

## Arguments

- show_text:

  A logical value that controls text display on the y axis. Default is
  `TRUE`.

- show_ticks:

  A logical value that controls ticks display on the y axis. Default is
  `TRUE`.

## Value

Theme for a specific panel.

## Examples

``` r
library(ggplot2)

p <- ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point()

p

p + theme_panel()
```
