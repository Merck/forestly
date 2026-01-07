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

1.  [`meta_forestly()`](https://merck.github.io/forestly/reference/meta_forestly.md)
    constructs input metadata for treatment analysis from ADaM datasets.
2.  [`prepare_ae_forestly()`](https://merck.github.io/forestly/reference/prepare_ae_forestly.md)
    prepares datasets for interactive forest plot.
3.  [`format_ae_forestly()`](https://merck.github.io/forestly/reference/format_ae_forestly.md)
    formats output layout.
4.  [`ae_forestly()`](https://merck.github.io/forestly/reference/ae_forestly.md)
    generates an interactive forest plot.

Here is a quick example

``` r
library("forestly")

meta_forestly(
  dataset_adsl = forestly_adsl,
  dataset_adae = forestly_adae,
  parameter_term = "any;rel;ser",
  population_subset = SAFFL == "Y",
  observation_subset = SAFFL == "Y"
) |>
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
- Drill-down listing by clicking $▸$.

## References

- Paper: [2023 PHUSE US
  Connect](https://phuse.s3.eu-central-1.amazonaws.com/Archive/2023/Connect/US/Florida/PAP_DV07.pdf)
- Talk: [2021 R/Pharma
  Conference](https://rinpharma.com/publication/rinpharma_206/)
