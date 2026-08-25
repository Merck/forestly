# Output static forest plot

Output static forest plot

## Usage

``` r
rtf_static_forestly(
  outdata,
  plot_calls,
  source,
  parameter = "any",
  n_rows = 25,
  orientation = "portrait",
  fig_size = c(6, 6),
  title = c("analysis", "observation", "population"),
  footnotes = NULL,
  text_font_size = 9,
  path_outdata = tempfile(fileext = ".Rdata"),
  path_outtable = tempfile(fileext = ".rtf")
)
```

## Arguments

- outdata:

  An `outdata` object created by
  [`format_ae_forestly()`](https://merck.github.io/forestly/reference/format_ae_forestly.md).

- plot_calls:

  A list or vector of function calls.

- source:

  A character value of the data source.

- parameter:

  A character value of parameter term name.

- n_rows:

  An integer value of the number of rows per page in a plot.

- orientation:

  Orientation in 'portrait' or 'landscape'.

- fig_size:

  A numeric vector of length 2 of figure width and height. The length
  should be 2 (width, height). The unit is inch.

- title:

  Term "analysis", "observation" and "population") for collecting title
  from metadata or a character vector of table titles.

- footnotes:

  A character vector of table footnotes.

- text_font_size:

  Text font size. To vary text font size by column, use numeric vector
  with length of vector equal to number of columns displayed e.g.
  c(9,20,40).

- path_outdata:

  A character string of the outdata path.

- path_outtable:

  A character string of the outtable path.

## Value

RTF file and source dataset for baseline characteristic table.

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

p1 <- substitute(
  plot_dot(
    tbl,
    prop_cols = c("prop_1", "prop_2"),
    y_var = "name",
    label = c("Treatment", "Placebo")
  )
)
p2 <- substitute(
  plot_errorbar(
    tbl,
    ci_cols = c("diff_1", "lower_1", "upper_1"),
    y_var = "name",
    label = c("Treatment", "Placebo"),
    legend_nrow = NULL,
    theme = theme_panel(show_ticks = FALSE, show_text = FALSE)
  )
)
p3 <- substitute(
  table_panel(
    tbl,
    n_cols = c("n_1", "n_2"),
    prop_cols = c("prop_1", "prop_2"),
    y_var = "name",
    theme = theme_panel(show_ticks = FALSE, show_text = FALSE),
    x_label = c("Treatment \n n(%)", "Placebo \n n(%)")
  )
)
outdata |> rtf_static_forestly(
  plot_calls = c(p1, p2, p3),
  source = "Source:  [CDISCpilot: adam-adsl; adae]",
  path_outdata = tempfile(fileext = ".Rdata"),
  path_outtable =  tempfile(fileext = ".rtf")
)
#> The outdata is saved in /tmp/RtmpWjnNQI/file19fe5399a29b.Rdata
#> The output is saved in /tmp/RtmpWjnNQI/file19fe3894ec5e.rtf
```
