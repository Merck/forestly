# Display Only SOC in the AE-Specific Tables

``` r
library(forestly)
library(metalite)
```

The interactive AE forest plots include AE-specific tables that present
both numerical and visual summaries for each AE SOC/PT terms. In this
vignette, we demonstrate how to display only SOC terms in the table.

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

## Step 2: display only SOC

Users can control the display of SOC and/or PT terms using the
`components` = … argument in the
[`prepare_ae_forestly()`](https://merck.github.io/forestly/reference/prepare_ae_forestly.md).

In the example below, we display only SOC terms in the forest plot, so
the “Adverse Events” column contains only SOC terms in the table.

``` r
meta |>
  prepare_ae_forestly(components = "soc") |>
  format_ae_forestly() |>
  ae_forestly()
```

AE Criteria

Incidence (%) in One or More Treatment Groups

Show/Hide SOC column
