# Display interactive forest plot

Display interactive forest plot

## Usage

``` r
ae_forestly(
  outdata,
  display_soc_toggle = TRUE,
  display_diff_toggle = FALSE,
  filter = c("prop", "n"),
  filter_label = NULL,
  filter_range = NULL,
  ae_label = NULL,
  width = 1400,
  max_page = NULL,
  dowload_button = FALSE
)
```

## Arguments

- outdata:

  An `outdata` object created by
  [`format_ae_forestly()`](https://merck.github.io/forestly/reference/format_ae_forestly.md).

- display_soc_toggle:

  A boolean value to display SOC toggle button.

- display_diff_toggle:

  A boolean value to display risk difference toggle button.

- filter:

  A character value of the filter variable. If NULL, the slider bar will
  not be displayed.

- filter_label:

  A character value of the label for slider bar.

- filter_range:

  A numeric vector of length 2 for the range of the slider bar.

- ae_label:

  A character value of the label for criteria. If NULL (default), the
  range is automatically calculated from the data. If only one value is
  provided, it will be used as the maximum and minimum will be 0.

- width:

  A numeric value of width of the table in pixels.

- max_page:

  A numeric value of max page number shown in the table.

- dowload_button:

  A logical value to display download button.

## Value

An AE forest plot saved as a `shiny.tag.list` object.

## Searching and filtering

The interactive table has a search box for each column (and, in the
expandable detail listing, a table-wide search box). In addition to a
plain substring match, the search terms understand two extra styles.

**Negation with `!`.** Prefix a term with an exclamation mark to
*exclude* matching rows instead of keeping them:

- `Rash` — keep rows whose value contains "Rash".

- `!Rash` — keep rows whose value does *not* contain "Rash".

**Expressions.** If the term mentions the letter `x` (which stands for
the value of the cell being searched), it is evaluated as a small
expression and the row is kept when the expression is true. `x` behaves
like the value in that column, so numeric columns can be compared with
numbers and text columns with quoted text. Common patterns:

|  |  |
|----|----|
| **Type this in the search box** | **Keeps rows where** |
| `x > 5` | the value is greater than 5 |
| `x >= 5` | the value is 5 or more |
| `x < 65` | the value is less than 65 |
| `x >= 18 && x <= 65` | the value is between 18 and 65 (inclusive) |
| `x == 0` | the value equals 0 |
| `x === "Rash"` | the value is exactly "Rash" |
| `x !== "Rash"` | the value is anything except exactly "Rash" |
| `x.includes("itch")` | the text contains "itch" |
| `!x.includes("itch")` | the text does not contain "itch" |
| `x.startsWith("Application")` | the text starts with "Application" |
| `x.endsWith("itis")` | the text ends with "itis" |
| `x === "M" || x === "F"` | the value is either "M" or "F" |

Notes for the expression style:

- Wrap text values in quotes (`"Rash"`); numbers need no quotes (`5`).

- Use `&&` for "and", `||` for "or", and `!` in front of a condition for
  "not".

- Comparisons use doubled symbols: `===` (equal), `!==` (not equal),
  together with `>`, `>=`, `<`, `<=`.

- Matching is case-sensitive in the expression style, so `x === "m"`
  will not match "M". Use the plain substring style (which ignores case)
  when case does not matter.

- A term that mentions `x` but is not a valid expression is treated as
  an ordinary substring search, so everyday searches keep working.

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

if (interactive()) {
  meta |>
    prepare_ae_forestly(parameter = "any") |>
    format_ae_forestly() |>
    ae_forestly()
}
```
