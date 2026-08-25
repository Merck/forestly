# Display interactive forest plot

Display interactive forest plot

## Usage

``` r
ae_forestly(
  outdata,
  display_soc_toggle = TRUE,
  display_diff_toggle = FALSE,
  filter = c("prop", "n"),
  filter_label = NULL,
  filter_range = NULL,
  ae_label = NULL,
  width = 1400,
  max_page = NULL,
  dowload_button = FALSE
)
```

## Arguments

- outdata:

  An `outdata` object created by
  [`format_ae_forestly()`](https://merck.github.io/forestly/reference/format_ae_forestly.md).

- display_soc_toggle:

  A boolean value to display SOC toggle button.

- display_diff_toggle:

  A boolean value to display risk difference toggle button.

- filter:

  A character value of the filter variable. If NULL, the slider bar will
  not be displayed.

- filter_label:

  A character value of the label for slider bar.

- filter_range:

  A numeric vector of length 2 for the range of the slider bar.

- ae_label:

  A character value of the label for criteria. If NULL (default), the
  range is automatically calculated from the data. If only one value is
  provided, it will be used as the maximum and minimum will be 0.

- width:

  A numeric value of width of the table in pixels.

- max_page:

  A numeric value of max page number shown in the table.

- dowload_button:

  A logical value to display download button.

## Value

An AE forest plot saved as a `shiny.tag.list` object.

## Examples

``` r
adsl <- forestly_adsl
adae <- forestly_adae
adsl$TRTA <- factor(
  adsl$TRT01A,
  levels = c("Xanomeline Low Dose", "Placebo"),
  labels = c("Low Dose", "Placebo")
)
adae$TRTA <- factor(
  adae$TRTA,
  levels = c("Xanomeline Low Dose", "Placebo"),
  labels = c("Low Dose", "Placebo")
)

analysis_plan <- metalite::plan(
  analysis = "ae_forestly",
  population = "apat",
  observation = "wk12",
  parameter = "any"
)
meta <- metalite::meta_adam(population = adsl, observation = adae) |>
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

if (interactive()) {
  meta |>
    prepare_ae_forestly(parameter = "any") |>
    format_ae_forestly() |>
    ae_forestly()
}
```
