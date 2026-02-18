# Changelog

## forestly 0.1.4

CRAN release: 2026-02-18

### New features

- Add `download_button` argument to
  [`ae_forestly()`](https://merck.github.io/forestly/reference/ae_forestly.md)
  to control whether the CSV download button is shown
  ([\#109](https://github.com/Merck/forestly/issues/109)).
- Add `ae_label` argument to
  [`ae_forestly()`](https://merck.github.io/forestly/reference/ae_forestly.md)
  to customize the label of the AE filter
  ([\#110](https://github.com/Merck/forestly/issues/110)).
- Add `display_diff_toggle` argument to
  [`ae_forestly()`](https://merck.github.io/forestly/reference/ae_forestly.md)
  to control whether the toggle button for displaying the “diff” column
  is shown ([\#114](https://github.com/Merck/forestly/issues/114)).
- Add new vignettes: vignette(“customize-display-only-soc”) and
  vignette(“customize-toggle-buttons”)
  ([\#108](https://github.com/Merck/forestly/issues/108),
  [\#114](https://github.com/Merck/forestly/issues/114)).

### Improvements

- Add a tool tip to the risk-difference column so that the full text is
  displayed ([\#117](https://github.com/Merck/forestly/issues/117)).

## forestly 0.1.3

CRAN release: 2025-09-10

### New features

- [`rtf_static_forestly()`](https://merck.github.io/forestly/reference/rtf_static_forestly.md)
  is now available for generating static forest plots in RTF format
  ([\#60](https://github.com/Merck/forestly/issues/60)).
- Add data source script `data-raw/forestly_adae.R`
  ([\#72](https://github.com/Merck/forestly/issues/72)).
- Add `prop_range` and `diff_range` arguments to
  [`format_ae_forestly()`](https://merck.github.io/forestly/reference/format_ae_forestly.md)
  to allow control over the x-axis limits for AE proportion and risk
  difference plots
  ([\#80](https://github.com/Merck/forestly/issues/80)).
- Add `col_header` and `fig_header` arguments to
  [`format_ae_forestly()`](https://merck.github.io/forestly/reference/format_ae_forestly.md)
  to enable customizable column and figure headers for risk difference
  outputs ([\#96](https://github.com/Merck/forestly/issues/96)).
- Add `filter_range` argument to
  [`ae_forestly()`](https://merck.github.io/forestly/reference/ae_forestly.md)
  for flexible slider range customization
  ([\#97](https://github.com/Merck/forestly/issues/97)).

### Bug fixes

- Fix bug to correctly pass the `digits` to the display format of each
  column in
  [`format_ae_forestly()`](https://merck.github.io/forestly/reference/format_ae_forestly.md)
  to make the number of displayed decimal places for proportions and
  risk differences user-configurable
  ([\#90](https://github.com/Merck/forestly/issues/90)).
- Fix bug so that the “n” and “(%)” columns in the output table are
  displayed only when specified in the `display` of
  [`format_ae_forestly()`](https://merck.github.io/forestly/reference/format_ae_forestly.md)
  ([\#95](https://github.com/Merck/forestly/issues/95))

### Improvements

- Update `format_ae_listing()` to consistently use list-style indexing
  (res\[\[“column”\]\]) instead of dollar-sign notation (res\$column)
  for accessing data frame columns
  ([\#71](https://github.com/Merck/forestly/issues/71)).
- Reorganize `pkgdown` site structure
  ([\#81](https://github.com/Merck/forestly/issues/81),
  [\#82](https://github.com/Merck/forestly/issues/82),
  [\#85](https://github.com/Merck/forestly/issues/85))
- Update `format_ae_listing()` to replace direct use of
  [`tools::toTitleCase()`](https://rdrr.io/r/tools/toTitleCase.html)
  with new helper function `titelcase()` for the case where SEX in the
  input data is factorized
  ([\#92](https://github.com/Merck/forestly/issues/92)).
- Update
  [`format_ae_forestly()`](https://merck.github.io/forestly/reference/format_ae_forestly.md)
  to ensure consistency in the order of treatment groups across tables
  and figures ([\#94](https://github.com/Merck/forestly/issues/94)).

## forestly 0.1.2

CRAN release: 2025-01-10

- Fix a bug that prevents reactable from rendering under reactR \>=
  0.6.0 ([\#67](https://github.com/Merck/forestly/issues/67)).
- Add a new argument `filter_label` to
  [`ae_forestly()`](https://merck.github.io/forestly/reference/ae_forestly.md)
  for controlling slider bar label
  ([\#68](https://github.com/Merck/forestly/issues/68)).

## forestly 0.1.1

CRAN release: 2024-07-08

- Add button to display SOC column for subset.
- Refine the default value of
  [`meta_forestly()`](https://merck.github.io/forestly/reference/meta_forestly.md).
- Update `format_ae_listing()` to be better aligned with oncology AEs
  reporting.
- Enhance functionality related to drill-down listing.
- Inherit paramter_term from all parameters of a metadata by default
  ([\#20](https://github.com/Merck/forestly/issues/20)).
- Allow user to specify treatment group
  ([\#15](https://github.com/Merck/forestly/issues/15)).
- Add warning when using tibble as input
  ([\#17](https://github.com/Merck/forestly/issues/17)).
- Fix bug when there is no record of a selected AE category
  ([\#9](https://github.com/Merck/forestly/issues/9)).
- Fix bug when there are multiple plots displayed on one page
  ([\#12](https://github.com/Merck/forestly/issues/12)).
- Update dataset `forestly_adae` since a variable was a factor which is
  unexpected.

## forestly 0.1.0

CRAN release: 2023-07-19

- Initial version.
- Added a `NEWS.md` file to track changes to the package.
