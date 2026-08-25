# forestly

## Installation

The easiest way to get forestly is to install from CRAN:

``` r

install.packages("forestly")
```

Alternatively, to use a new feature or get a bug fix, you can install
the development version of forestly from GitHub:

``` r

# install.packages("remotes")
remotes::install_github("Merck/forestly")
```

## Overview

The forestly package creates interactive forest plots for clinical trial
analysis & reporting.

- Safety analysis
  - Specific adverse events analysis
- Efficacy analysis (future work)
  - Subgroup analysis

We assume ADaM datasets are ready for analysis and leverage
[metalite](https://merck.github.io/metalite/) data structure to define
inputs and outputs.

## Workflow

The general workflow is:

1.  Define input metadata from ADaM datasets with `metalite`.
2.  [`prepare_ae_forestly()`](https://merck.github.io/forestly/reference/prepare_ae_forestly.md)
    prepares datasets for interactive forest plot.
3.  [`format_ae_forestly()`](https://merck.github.io/forestly/reference/format_ae_forestly.md)
    formats output layout.
4.  [`ae_forestly()`](https://merck.github.io/forestly/reference/ae_forestly.md)
    generates an interactive forest plot.

Here is a quick example

``` r

library("forestly")

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
  parameter = "any;rel;ser"
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
  metalite::define_parameter(
    name = "rel",
    term1 = "Drug-Related",
    term2 = "",
    subset = AEREL %in% c("POSSIBLE", "PROBABLE"),
    var = "AEDECOD",
    soc = "AEBODSYS",
    label = "Drug-related AEs"
  ) |>
  metalite::define_parameter(
    name = "ser",
    term1 = "Serious",
    term2 = "",
    subset = AESER == "Y",
    var = "AEDECOD",
    soc = "AEBODSYS",
    label = "Serious AEs"
  ) |>
  metalite::define_analysis(
    name = "ae_forestly",
    label = "Interactive forest plot"
  ) |>
  metalite::meta_build()

meta |>
  prepare_ae_forestly(parameter = "any;rel;ser") |>
  format_ae_forestly() |>
  ae_forestly()
```

## Interactive features

The interactive features for safety analysis include:

- Select different AE criteria.
- Filter by incidence of AE in one or more groups.
- Reveal information by hovering the mouse over a data point.
- Search bars to find subjects with selected adverse events (AEs).
- Sort value by clicking the column header.
- Drill-down listing by clicking $`\blacktriangleright`$.

## References

- Paper: [2023 PHUSE US
  Connect](https://phuse.s3.eu-central-1.amazonaws.com/Archive/2023/Connect/US/Florida/PAP_DV07.pdf)
- Talk: [2021 R/Pharma
  Conference](https://www.youtube.com/watch?v=HICBeSqD6kI)
