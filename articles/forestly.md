# Generate Interactive AE Forest Plots with Drill Down to AE Listing

## Introduction

The `forestly` R package creates interactive forest plots for clinical
trial analysis and reporting. The interactive forest plots include: -
AE-specific tables along with the AE proportion difference and its
confidence interval visualized. - AE listing tables corresponding to a
selected AE.

Through its interactive features, users can: - **Search** for the AE PT
term or SOC term of interest. - **Filter** AEs with an incidence greater
than a specified percentage in one or more arms. - **Sort** AEs from the
smallest proportion or proportion difference to the largest. -
**Switch** between different AE-specific tables. - **Drill down** to
listings for subjects with the selected AE.

## Procedure to generate interactive forest plot

### Step 1: read the ADAM data

In the example presented in this vignette, we utilize the ADAM data from
the `forestly` R package. We further factorize the actual arm variable,
`TRTA`, to enable the `forestly` R package to identify which arm serves
as the reference group.

``` r
library(metalite)
library(forestly)

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
```

### Step 2: build the metadata

We use the [metalite](https://merck.github.io/metalite/) R package to
generate a minimal metadata for the generation of the interactive forest
plot.

To begin, we input the `adsl` data as the population data and the `adae`
data as the observation data.

``` r
meta <- meta_adam(population = adsl, observation = adae)
```

Without loss of generality, our plan is to generate the following four
AE-specific tables:

- Any AE (we give it a short name/nickname as `"any"`)
- Drug-related AE (we give it a short name/nickname as `"drug-related"`)
- Serious AE (we give it a short name/nickname as `"serious"`)
- Drug-related serious AE (we give it a short name/nickname as
  `"drug-related-serious"`) However, users are allowed to add more
  AE-specific tables as needed. For the above 4 AE specific tables, we
  take the APaT for both observation and observation.

``` r
meta <- meta |>
  define_plan(plan = plan(
    analysis = "ae_forestly",
    population = "apat",
    observation = "apat",
    parameter = "any;drug-related;serious;drug-related-serious"
  ))
```

Following the definition of the analysis plan, we further specify the
details of `analysis = "ae_forestly"` by providing its label for
display.

``` r
meta <- meta |>
  define_analysis(name = "ae_forestly", label = "Interactive Forest Plot")
```

We then define the related population (`population = "apat"`) and
observation (`observation = "apat"`) by specifying their grouping
variable (`group = ...`), subject ID (`id = ...`), population flag
(`subset = ...`) and label (`label = ...`). If users wish to use
different population flags, grouping variables, or labels, they can make
changes as needed.

``` r
meta <- meta |>
  define_population(
    name = "apat", group = "TRTA", id = "USUBJID",
    subset = SAFFL == "Y", label = "All Patient as Treated"
  ) |>
  define_observation(
    name = "apat", group = "TRTA",
    subset = SAFFL == "Y", label = "All Patient as Treated"
  )
```

Next, we specify the details of
`parameter = "any;drug-related;serious;drug-related-serious"`. For any
AEs (`"any"`), their is no filter applied.

``` r
meta <- meta |>
  define_parameter(
    name = "any",
    subset = NULL,
    label = "Any AEs",
    var = "AEDECOD", soc = "AEBODSYS"
  )
```

For drug related AEs (`"drug-related"`), its filter is
`AEREL %in% c("PROBABLE", "POSSIBLE")`.

``` r
meta <- meta |>
  define_parameter(
    name = "drug-related",
    subset = toupper(AREL) == "RELATED",
    label = "Drug-related AEs",
    var = "AEDECOD", soc = "AEBODSYS"
  )
```

Similarly, we define serious AE and drug related serious AEs.

``` r
meta <- meta |>
  define_parameter(
    name = "serious",
    subset = AESER == "Y",
    label = "Serious AEs",
    var = "AEDECOD", soc = "AEBODSYS"
  ) |>
  define_parameter(
    name = "drug-related-serious",
    subset = AESER == "Y" & AEREL %in% c("PROBABLE", "POSSIBLE"),
    label = "Drug-related serious AEs",
    var = "AEDECOD", soc = "AEBODSYS"
  )
```

Finally, we build the metadata by running the
[`meta_build()`](https://merck.github.io/metalite/reference/meta_build.html)
function.

``` r
meta <- meta |> meta_build()
```

### Step 3: generate the interative AE forest plot

With the `meta` built as described above, users can generate the
interactive AE forest seamlessly through a few simple function calls
connected with the pipe operator (`|>`). The
[`prepare_ae_forestly()`](https://merck.github.io/forestly/reference/prepare_ae_forestly.md)
prepares datasets (i.e., calculate all to-be-reported numbers). The
[`format_ae_forestly()`](https://merck.github.io/forestly/reference/format_ae_forestly.md)
takes care of formating, and the
[`ae_forestly()`](https://merck.github.io/forestly/reference/ae_forestly.md)
function realize the interactivity.

``` r
meta |>
  prepare_ae_forestly() |>
  format_ae_forestly() |>
  ae_forestly()
```

AE Criteria

Incidence (%) in One or More Treatment Groups

Show/Hide SOC column
