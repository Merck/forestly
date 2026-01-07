# Claude Code Assistant Instructions

## Project Overview

This is the forestly R package, which creates interactive forest plots
for clinical trial data analysis. The package is built on top of
metalite and metalite.ae packages and uses reactable for interactive
tables with Plotly.js for interactive visualizations.

## Development Guidelines

### Testing

- load project using `devtools::load_all()`
- Run tests using: `devtools::test()`
- Run specific test files using: `devtools::test(filter = "filename")`
- Before running tests, ensure required packages are installed
  (metalite, metalite.ae, reactable, reactR)

### Code Style

- Follow tidyverse style guide
- Use `|>` pipe operator (R 4.1+)
- Functions should handle both character vectors and factors robustly

### Key Functions

- [`ae_forestly()`](https://merck.github.io/forestly/reference/ae_forestly.md):
  Main function to create interactive forest plots
- [`format_ae_forestly()`](https://merck.github.io/forestly/reference/format_ae_forestly.md):
  Formats AE data and configures Plotly visualizations
- `format_ae_listing()`: Formats AE listing data
- `sparkline_point_js()`: Generates JavaScript/Plotly code for
  interactive sparkline plots
- `propercase()`: Converts strings to proper case (handles factors)
- `titlecase()`: Converts strings to title case using tools::toTitleCase
  (handles factors)

### Before Committing

- Run linting: Check for any linting issues in the IDE
- Run tests: `devtools::test()` to ensure all tests pass
- Check documentation: `devtools::document()` if roxygen comments are
  updated

### Branch Strategy

- Main branch: `main`
- Feature branches: Use descriptive names like `fix-factor-handling` or
  `add-new-feature`
- Always create pull requests for merging into main

### Common Commands

``` r
# Load all functions for development
devtools::load_all()

# Run all tests
devtools::test()

# Check package
devtools::check()

# Build documentation
devtools::document()
```

## Package Dependencies

- metalite
- metalite.ae
- reactable
- reactR
- plotly (via JavaScript integration)
- brew (for template processing)
- tools (base R)

## Testing Data

The package includes test data in `data/`: - forestly_adae.rda -
forestly_adae_3grp.rda - forestly_adsl.rda - forestly_adsl_3grp.rda

## Notes for Future Development

- The `ae_listing.R` file contains functions that handle factor inputs,
  which was a recent fix
- Test files should use `devtools::load_all()` or source the R files
  directly for testing
- The package uses testthat for unit testing framework
- Interactive plots use Plotly.js via JavaScript templates in `inst/js/`
- The y-axis ordering in `format_ae_forestly.R` affects the visual
  display in interactive plots
- When debugging interactive plot issues, check both R code and
  JavaScript template files
