# Create metadata for interactive forest plot

Create metadata for interactive forest plot

## Usage

``` r
meta_forestly(
  dataset_adsl,
  dataset_adae,
  population_term = "apat",
  observation_term = "safety",
  parameter_term = "any;rel",
  population_subset,
  observation_subset,
  treatment_group = "TRTA"
)
```

## Arguments

- dataset_adsl:

  ADSL source dataset.

- dataset_adae:

  ADAE source dataset.

- population_term:

  A character value of population term name.

- observation_term:

  A character value of observation term name.

- parameter_term:

  A character value of parameter term name.

- population_subset:

  An unquoted condition for selecting the populations from ADSL dataset.

- observation_subset:

  An unquoted condition for selecting the observations from ADAE
  dataset.

- treatment_group:

  A character value of treatment group name.

## Value

A metalite object.

## Examples

``` r
meta_forestly(
  forestly_adsl,
  forestly_adae,
  population_term = "apat",
  observation_term = "safety",
  parameter_term = "any;rel"
)
#> ADaM metadata: 
#>    .$data_population     Population data with 170 subjects 
#>    .$data_observation    Observation data with 736 records 
#>    .$plan    Analysis plan with 1 plans 
#> 
#> 
#>   Analysis population type:
#>     name        id  group var label
#> 1 'apat' 'USUBJID' 'TRTA'        ''
#> 
#> 
#>   Analysis observation type:
#>       name        id  group var label
#> 1 'safety' 'USUBJID' 'TRTA'        ''
#> 
#> 
#>   Analysis parameter type:
#>    name                         label                     subset
#> 1 'any'          'any adverse events'                           
#> 2 'rel' 'drug-related adverse events' toupper(AREL) == 'RELATED'
#> 
#> 
#>   Analysis function:
#>            name                     label
#> 1 'ae_forestly' 'Interactive forest plot'
#> 
```
