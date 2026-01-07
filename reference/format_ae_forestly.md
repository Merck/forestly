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
  diff_label = "Treatment <- Favor -> Placebo",
  col_header = NULL,
  fig_header = NULL,
  show_ae_parameter = FALSE
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

- diff_label:

  x-axis label for risk difference.

- col_header:

  Column header for risk difference table columns. If NULL (default),
  uses "Risk Difference (%)  
  vs. Reference Group".

- fig_header:

  Column header for risk difference figure. If NULL (default), uses
  "Risk Difference (%) + 95% CI  
  vs. Reference Group".

- show_ae_parameter:

  A boolean value to display AE parameter column.

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
  prepare_ae_forestly() |>
  format_ae_forestly()
#> Warning: In observation level data, force group variable 'TRTA' be a factor
#> Warning: In observation level data, force group variable 'TRTA' be a factor
#> List of 25
#>  $ meta                   :List of 7
#>  $ population             : chr "apat"
#>  $ observation            : chr "safety"
#>  $ parameter              : chr "any;rel"
#>  $ n                      :'data.frame': 60 obs. of  3 variables:
#>  $ order                  : num [1:60] 1008 1009 2019 2021 2022 ...
#>  $ group                  : chr [1:3] "Placebo" "Xanomeline Low Dose" "Total"
#>  $ reference_group        : num 2
#>  $ parameter_order        : Factor w/ 2 levels "any","rel": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ prop                   :'data.frame': 60 obs. of  3 variables:
#>  $ diff                   :'data.frame': 60 obs. of  1 variable:
#>  $ n_pop                  :'data.frame': 1 obs. of  3 variables:
#>  $ name                   : chr [1:60] "Atrioventricular block second degree" "Bundle branch block left" "Eye allergy" "Eye pruritus" ...
#>  $ soc_name               : chr [1:60] "CARDIAC DISORDERS" "CARDIAC DISORDERS" "EYE DISORDERS" "EYE DISORDERS" ...
#>  $ ci_lower               :'data.frame': 60 obs. of  1 variable:
#>  $ ci_upper               :'data.frame': 60 obs. of  1 variable:
#>  $ p                      :'data.frame': 60 obs. of  1 variable:
#>  $ ae_listing             :'data.frame': 151 obs. of  15 variables:
#>  $ tbl                    :'data.frame': 60 obs. of  14 variables:
#>  $ reactable_columns      :List of 14
#>  $ reactable_columns_group:List of 3
#>  $ display                : chr [1:4] "n" "prop" "fig_prop" "fig_diff"
#>  $ fig_prop_color         : chr [1:2] "#00857C" "#66203A"
#>  $ fig_diff_color         : chr "#00857C"
#>  $ hidden_column          : chr [1:6] "parameter" "diff_1" "lower_1" "upper_1" ...
```
