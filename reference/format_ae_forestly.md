# Format outdata for interactive forest plot

Format outdata for interactive forest plot

## Usage

``` r
format_ae_forestly(
  outdata,
  display = c("n", "prop", "fig_prop", "fig_diff"),
  digits = 1,
  width_term = 200,
  width_fig = 320,
  width_n = 40,
  width_prop = 60,
  width_diff = 80,
  footer_space = 90,
  prop_range = NULL,
  diff_range = NULL,
  color = NULL,
  ae_col_header = NULL,
  diff_label = "Treatment <- Favor -> Placebo",
  diff_col_header = NULL,
  diff_fig_header = NULL
)
```

## Arguments

- outdata:

  An `outdata` object created by
  [`prepare_ae_forestly()`](https://merck.github.io/forestly/reference/prepare_ae_forestly.md).

- display:

  A character vector of measurement to be displayed.

  - `n`: Number of subjects with AE.

  - `prop`: Proportion of subjects with AE.

  - `total`: Total columns.

  - `diff`: Risk difference.

- digits:

  A number of digits after decimal point to be displayed for proportion
  and risk difference.

- width_term:

  Width in px for AE term column.

- width_fig:

  Width in px for proportion and risk difference figure.

- width_n:

  Width in px for "N" columns.

- width_prop:

  Width in px for "(%)" columns.

- width_diff:

  Width in px for risk difference columns.

- footer_space:

  Space in px for footer to display legend.

- prop_range:

  A vector of lower and upper limit of x-axis for proportion figure.

- diff_range:

  A vector of lower and upper limit of x-axis for risk difference
  figure.

- color:

  A vector of colors for analysis groups. Default value supports up to 4
  groups.

- ae_col_header:

  Column header for adverse events item columns. If NULL (default) and
  "par" specified in `components` from
  [`prepare_ae_forestly()`](https://merck.github.io/forestly/reference/prepare_ae_forestly.md),
  uses "Adverse Event". If NULL and "soc" specified in `components` from
  [`prepare_ae_forestly()`](https://merck.github.io/forestly/reference/prepare_ae_forestly.md),
  uses "System Organ Class" for "soc".

- diff_label:

  x-axis label for risk difference.

- diff_col_header:

  Column header for risk difference table columns. If NULL (default),
  uses "Risk Difference (%)  
  vs. Reference Group".

- diff_fig_header:

  Column header for risk difference figure. If NULL (default), uses
  "Risk Difference (%) + 95% CI  
  vs. Reference Group".

## Value

An `outdata` object.

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

meta |>
  prepare_ae_forestly(parameter = "any") |>
  format_ae_forestly()
#> List of 26
#>  $ meta                   :List of 7
#>  $ population             : chr "apat"
#>  $ observation            : chr "wk12"
#>  $ parameter              : chr "any"
#>  $ n                      :'data.frame': 190 obs. of  3 variables:
#>  $ order                  : num [1:190] 1021 1022 1023 1024 1025 ...
#>  $ group                  : chr [1:3] "Low Dose" "Placebo" "Total"
#>  $ reference_group        : num 2
#>  $ parameter_order        : Factor w/ 1 level "any": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ components             : chr "par"
#>  $ prop                   :'data.frame': 190 obs. of  3 variables:
#>  $ diff                   :'data.frame': 190 obs. of  1 variable:
#>  $ n_pop                  :'data.frame': 1 obs. of  3 variables:
#>  $ name                   : chr [1:190] "Atrial fibrillation" "Atrial flutter" "Atrial hypertrophy" "Atrioventricular block first degree" ...
#>  $ soc_name               : chr [1:190] "CARDIAC DISORDERS" "CARDIAC DISORDERS" "CARDIAC DISORDERS" "CARDIAC DISORDERS" ...
#>  $ ci_lower               :'data.frame': 190 obs. of  1 variable:
#>  $ ci_upper               :'data.frame': 190 obs. of  1 variable:
#>  $ p                      :'data.frame': 190 obs. of  1 variable:
#>  $ ae_listing             :'data.frame': 736 obs. of  15 variables:
#>  $ tbl                    :'data.frame': 190 obs. of  14 variables:
#>  $ reactable_columns      :List of 14
#>  $ reactable_columns_group:List of 3
#>  $ display                : chr [1:4] "n" "prop" "fig_prop" "fig_diff"
#>  $ fig_prop_color         : chr [1:2] "#00857C" "#66203A"
#>  $ fig_diff_color         : chr "#00857C"
#>  $ hidden_column          : chr [1:6] "parameter" "diff_1" "lower_1" "upper_1" ...
```
