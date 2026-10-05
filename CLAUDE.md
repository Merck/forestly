# Claude Code Assistant Instructions

## Project Overview
This is the forestly R package, which creates interactive forest plots for clinical trial data analysis. The package is built on top of metalite and metalite.ae packages and uses the `lt` package (yihui/lt) for interactive tables with inline SVG figures (dot plots and error bars) drawn client-side.

## Development Guidelines

### Testing
- load project using `devtools::load_all()`
- Run tests using: `devtools::test()`
- Run specific test files using: `devtools::test(filter = "filename")`
- Before running tests, ensure required packages are installed (metalite, metalite.ae, lt (>= 0.4.21))

### Code Style
- Follow tidyverse style guide
- Use `|>` pipe operator (R 4.1+)
- Functions should handle both character vectors and factors robustly

### Key Functions
- `ae_forestly()`: Main function to create interactive forest plots (layers `lt::lt_interactive()`, the detail drill-down, and the forestly controls on top of the structural table)
- `format_ae_forestly()`: Formats AE data and computes figure metadata (ranges, colors, headers, widths) for the inline dot plot / error bar
- `format_lt_forestly()`: Builds the structural `lt` table (interaction-free; in `R/lt_table.R`)
- `format_ae_listing()`: Formats AE listing data
- `propercase()`: Converts strings to proper case (handles factors)
- `titlecase()`: Converts strings to title case using tools::toTitleCase (handles factors)

### Before Committing
- Run linting: Check for any linting issues in the IDE
- Run tests: `devtools::test()` to ensure all tests pass
- Check documentation: `devtools::document()` if roxygen comments are updated

### Branch Strategy
- Main branch: `main`
- Feature branches: Use descriptive names like `fix-factor-handling` or `add-new-feature`
- Always create pull requests for merging into main

### Common Commands
```r
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
- lt (>= 0.4.21) — interactive tables + inline SVG figures
- htmltools
- ggplot2 (static forest/table panels)
- xfun
- tools (base R)

## Testing Data
The package includes test data in `data/`:
- forestly_adae.rda
- forestly_adae_3grp.rda
- forestly_adsl.rda
- forestly_adsl_3grp.rda

## Notes for Future Development
- The `ae_listing.R` file contains functions that handle factor inputs, which was a recent fix
- Test files should use `devtools::load_all()` or source the R files directly for testing
- The package uses testthat for unit testing framework
- The main interactive table is a single `lt` table: `format_lt_forestly()` builds the structure (per-arm columns, spanners, `lt_dotplot` proportion figure, `lt_errorbar` risk-difference figure, hidden helper columns), and `ae_forestly()` makes it interactive and adds the drill-down + forestly controls
- the AE-criteria dropdown and incidence slider are lt typed filters, declared via `lt_interactive(filter = ...)` in `ae_forestly()` (e.g. `filter = list(parameter = list(type = "select", ...), hide_prop = list(type = "range", ...))`) and bound to the hidden helper columns (`parameter`, `hide_prop`/`hide_n`); lt renders them automatically (funnel under a visible column's header, chip in the control bar for a hidden column). The only forestly-owned widget left is the CSV download in `inst/js/forestly-widgets.js`, which reads the table's current view via lt's `el._lt.view()`
- When debugging interactive plot issues, check both the R code (`R/lt_table.R`, `R/ae_forestly.R`) and the `lt` package JS runtime (`../lt/inst/www/lt-interactive.js`, `lt-plot.js`)