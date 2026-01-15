# Display interactive forest plot

Display interactive forest plot

## Usage

``` r
ae_forestly(
  outdata,
  display_soc_toggle = TRUE,
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

- filter:

  A character value of the filter variable.

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

## Examples

``` r
adsl <- forestly_adsl[1:100, ]
adae <- forestly_adae[1:100, ]
if (interactive()) {
  meta_forestly(
    dataset_adsl = adsl,
    dataset_adae = adae,
  ) |>
    prepare_ae_forestly() |>
    format_ae_forestly() |>
    ae_forestly()
}
```
