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
  ae_listing_unique = FALSE
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

## Value

An `outdata` object.

## Examples

``` r
adsl <- forestly_adsl[1:100, ]
adae <- forestly_adae[1:100, ]
meta_forestly(
  dataset_adsl = adsl,
  dataset_adae = adae
) |>
  prepare_ae_forestly()
#> Warning: In observation level data, force group variable 'TRTA' be a factor
#> Warning: In observation level data, force group variable 'TRTA' be a factor
#> List of 19
#>  $ meta           :List of 7
#>  $ population     : chr "apat"
#>  $ observation    : chr "safety"
#>  $ parameter      : chr "any;rel"
#>  $ n              :'data.frame': 60 obs. of  3 variables:
#>  $ order          : num [1:60] 1008 1009 2019 2021 2022 ...
#>  $ group          : chr [1:3] "Placebo" "Xanomeline Low Dose" "Total"
#>  $ reference_group: num 2
#>  $ parameter_order: Factor w/ 2 levels "any","rel": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ components     : chr "par"
#>  $ prop           :'data.frame': 60 obs. of  3 variables:
#>  $ diff           :'data.frame': 60 obs. of  1 variable:
#>  $ n_pop          :'data.frame': 1 obs. of  3 variables:
#>  $ name           : chr [1:60] "Atrioventricular block second degree" "Bundle branch block left" "Eye allergy" "Eye pruritus" ...
#>  $ soc_name       : chr [1:60] "CARDIAC DISORDERS" "CARDIAC DISORDERS" "EYE DISORDERS" "EYE DISORDERS" ...
#>  $ ci_lower       :'data.frame': 60 obs. of  1 variable:
#>  $ ci_upper       :'data.frame': 60 obs. of  1 variable:
#>  $ p              :'data.frame': 60 obs. of  1 variable:
#>  $ ae_listing     :'data.frame': 151 obs. of  15 variables:
```
