# Prepare datasets for interactive forest plot

Prepare datasets for interactive forest plot

## Usage

``` r
prepare_ae_forestly(
  meta,
  population = NULL,
  observation = NULL,
  parameter = NULL,
  components = "par",
  reference_group = NULL,
  ae_listing_display = c("USUBJID", "SITEID", "SEX", "RACE", "AGE", "ASTDY", "AESER",
    "AEREL", "AEACN", "AEOUT", "ADURN", "ADURU"),
  ae_listing_unique = FALSE,
  bisection = 100,
  ...
)
```

## Arguments

- meta:

  A metadata object created by metalite.

- population:

  A character value of population term name. The term name is used as
  key to link information.

- observation:

  A character value of observation term name. The term name is used as
  key to link information.

- parameter:

  A character value of parameter term name. The term name is used as key
  to link information.

- components:

  A character vector of components name.

- reference_group:

  An integer to indicate reference group. Default is 2 if there are 2
  groups, otherwise, the default is 1.

- ae_listing_display:

  A vector of name of variables used to display on AE listing table.

- ae_listing_unique:

  A logical value to display only unique records on AE listing table.

- bisection:

  A numeric value. A control parameter for the bisection method used to
  calculate confidence the lower and upper confidence interval bounds
  for the risk. The default value is `1e2`.

- ...:

  Additional arguments passed to
  [`metalite.ae::rate_compare_sum()`](https://merck.github.io/metalite.ae/reference/rate_compare_sum.html).

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

prepare_ae_forestly(meta, parameter = "any")
#> List of 19
#>  $ meta           :List of 7
#>  $ population     : chr "apat"
#>  $ observation    : chr "wk12"
#>  $ parameter      : chr "any"
#>  $ n              :'data.frame': 190 obs. of  3 variables:
#>  $ order          : num [1:190] 1021 1022 1023 1024 1025 ...
#>  $ group          : chr [1:3] "Low Dose" "Placebo" "Total"
#>  $ reference_group: num 2
#>  $ parameter_order: Factor w/ 1 level "any": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ components     : chr "par"
#>  $ prop           :'data.frame': 190 obs. of  3 variables:
#>  $ diff           :'data.frame': 190 obs. of  1 variable:
#>  $ n_pop          :'data.frame': 1 obs. of  3 variables:
#>  $ name           : chr [1:190] "Atrial fibrillation" "Atrial flutter" "Atrial hypertrophy" "Atrioventricular block first degree" ...
#>  $ soc_name       : chr [1:190] "CARDIAC DISORDERS" "CARDIAC DISORDERS" "CARDIAC DISORDERS" "CARDIAC DISORDERS" ...
#>  $ ci_lower       :'data.frame': 190 obs. of  1 variable:
#>  $ ci_upper       :'data.frame': 190 obs. of  1 variable:
#>  $ p              :'data.frame': 190 obs. of  1 variable:
#>  $ ae_listing     :'data.frame': 736 obs. of  15 variables:
```
