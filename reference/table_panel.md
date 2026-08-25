# Create table panel ggplot2 object for rainfall or forest plot

Creates a table panel ggplot2 object for rainfall or forest plot.

## Usage

``` r
table_panel(
  tbl,
  n_cols = c("n_1", "n_2"),
  prop_cols = c("prop_1", "prop_2"),
  y_var,
  x_label = NULL,
  text_color = NULL,
  text_size = 8,
  text_format_by = "column",
  background_color = c("#69B8F7", "#FFFFFF"),
  theme = theme_panel(show_ticks = TRUE, show_text = TRUE),
  background_alpha = 0.3
)
```

## Arguments

- tbl:

  A data frame to be displayed in this table.

- n_cols:

  A character vector of columns for subject count to be used for a plot.

- prop_cols:

  A character vector of proportion columns to be used for a plot.

- y_var:

  A string of a variable name from `tbl` for the y axis variable.

- x_label:

  Labels displayed on the top of table for each column of table. Default
  is `NULL`, variable name will display as label.

- text_color:

  Defines colors to display each treatment group.

- text_size:

  Numeric font size for data on each column. Default is 8 for each
  column.

- text_format_by:

  An option for formatting a data by columns or rows. Default is
  `"column"` and text color will be varied by column. If
  `text_format_by = "row"`, then text color will be varied by row. If
  `text_format_by = "group"`, then text color will be varied by
  treatment group.

- background_color:

  Color for the plot background. Default is `c("#69B8F7", "#FFFFFF")`
  which are pastel blue and white. The value of this argument will be
  the input value for the `background_color` argument in
  [`background_panel()`](https://merck.github.io/forestly/reference/background_panel.md).

- theme:

  Controls display of y axis text, ticks and plot margin. By default,
  `theme_panel(show_text = TRUE, show_ticks = TRUE)` is used. Users are
  suggested to use
  [`theme_panel()`](https://merck.github.io/forestly/reference/theme_panel.md).

- background_alpha:

  Opacity of the background. Default is 0.3. The value of this argument
  will be the input value for the `background_alpha` argument in
  [`background_panel()`](https://merck.github.io/forestly/reference/background_panel.md).

## Value

A ggplot2 object for table panel.

## Examples

``` r
forestly_adsl$TRTA <- factor(
  forestly_adsl$TRT01A,
  levels = c("Xanomeline Low Dose", "Placebo"),
  labels = c("Low Dose", "Placebo")
)
forestly_adae$TRTA <- factor(
  forestly_adae$TRTA,
  levels = c("Xanomeline Low Dose", "Placebo"),
  labels = c("Low Dose", "Placebo")
)

analysis_plan <- metalite::plan(
  analysis = "ae_forestly",
  population = "apat",
  observation = "wk12",
  parameter = "any"
)
meta <- metalite::meta_adam(
  population = forestly_adsl,
  observation = forestly_adae
) |>
  metalite::define_plan(plan = analysis_plan) |>
  metalite::define_population(
    name = "apat",
    var = c("USUBJID", "SAFFL", "TRTA", "SITEID", "SEX", "RACE", "AGE"),
    group = "TRTA",
    subset = SAFFL == "Y",
    label = "All Participants as Treated"
  ) |>
  metalite::define_observation(
    name = "wk12",
    var = c(
      "USUBJID", "SAFFL", "TRTA", "SITEID", "SEX", "RACE", "AGE",
      "ASTDY", "AEDECOD", "AEBODSYS", "AESER", "AEREL", "AEACN",
      "AEOUT", "ADURN", "ADURU"
    ),
    group = "TRTA",
    subset = SAFFL == "Y",
    label = "Weeks 0 to 12"
  ) |>
  metalite::define_parameter(
    name = "any",
    term1 = "",
    term2 = "",
    var = "AEDECOD",
    soc = "AEBODSYS",
    label = "All AEs"
  ) |>
  metalite::define_analysis(
    name = "ae_forestly",
    label = "Interactive forest plot"
  ) |>
  metalite::meta_build()

outdata <- meta |>
  prepare_ae_forestly(parameter = "any") |>
  format_ae_forestly()

outdata_any <- outdata$tbl[1:20, ] |> dplyr::filter(parameter == "any")

outdata_any |>
  table_panel(y_var = "name")
```
