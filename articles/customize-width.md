# Adjusting Column Widths for Optimal Layout

``` r
library(forestly)
library(metalite)
```

The interactive AE forest plots include AE-specific tables presenting
numerical values for AE proportions, differences, and confidence
intervals (CI), alongside their visualizations. Each type of information
is displayed in a separate column. In this vignette, we demonstrate how
to customize the column widths in these tables.

## Step 1: build your metadata

Building interactive AE forest plots starts with constructing the
metadata. The detailed procedure for building metadata is covered in the
vignette [Generate Interactive AE Forest Plots with Drill Down to AE
Listing](https://merck.github.io/forestly/articles/forestly.html).
Therefore, in this vignette, we will skip those details and directly use
the metadata created there.

``` r
adsl <- forestly_adsl
adae <- forestly_adae

adsl$TRTA <- factor(forestly_adsl$TRT01A,
  levels = c("Xanomeline Low Dose", "Placebo"),
  labels = c("Low Dose", "Placebo")
)
adae$TRTA <- factor(forestly_adae$TRTA,
  levels = c("Xanomeline Low Dose", "Placebo"),
  labels = c("Low Dose", "Placebo")
)

meta <- meta_adam(population = adsl, observation = adae) |>
  define_plan(plan = plan(
    analysis = "ae_forestly",
    population = "apat",
    observation = "apat",
    parameter = "any;drug-related"
  )) |>
  define_analysis(name = "ae_forestly", label = "Interactive Forest Plot") |>
  define_population(
    name = "apat", group = "TRTA", id = "USUBJID",
    subset = SAFFL == "Y", label = "All Patient as Treated"
  ) |>
  define_observation(
    name = "apat", group = "TRTA",
    subset = SAFFL == "Y", label = "All Patient as Treated"
  ) |>
  define_parameter(
    name = "any",
    subset = NULL,
    label = "Any AEs",
    var = "AEDECOD", soc = "AEBODSYS"
  ) |>
  define_parameter(
    name = "drug-related",
    subset = toupper(AREL) == "RELATED",
    label = "Drug-related AEs",
    var = "AEDECOD", soc = "AEBODSYS"
  ) |>
  meta_build()
```

## Step 2: adjusting column widths

Users can control the column widths using the following arguments:

- `width_term = ...`: Sets the width (in pixels) of the column
  displaying the AE preferred term.
- `width_fig = ...`: Sets the width (in pixels) of the column displaying
  the AE proportions and their differences visually.
- `width_n = ...`: Sets the width (in pixels) of the columns reporting
  the numerical counts of subjects with the AE term (the “N” columns).
- `width_prop = ...`: Sets the width (in pixels) of the columns
  reporting the numerical AE proportions (the “(%)” columns).
- `width_diff = ...`: Sets the width (in pixels) of the columns
  reporting the numerical AE proportion differences.
- `footer_space = ...`: Sets the vertical space (in pixels) between the
  legend and the bottom of the AE-specific table.

In the example below, we modify the column widths to better suit the
display. Users are encouraged to adjust these widths to optimize the
table layout for their preferred screen size and readability.

``` r
meta |>
  prepare_ae_forestly() |>
  format_ae_forestly(
    width_term = 5, # width of the column of AE PT term
    # widths of the column of subject with AE counts
    width_n = 2,
    # widths of the column of AE proportions
    width_prop = 2,
    # widths of plotting the AE proportions and differences
    width_fig = 150,
    # vertical space between legend and the table
    footer_space = 100
  ) |>
  ae_forestly()
```

AE Criteria

Incidence (%) in One or More Treatment Groups

Show/Hide SOC column
