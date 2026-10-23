# Claude Code Assistant Instructions

forestly creates interactive forest plots for clinical trial AE data, on top of metalite / metalite.ae. The interactive table is an [`lt`](https://github.com/yihui/lt) table with inline SVG figures (dot plot, error bar) drawn client-side.

## Architecture

- `ae_forestly()` (`R/ae_forestly.R`): layers `lt::lt_interactive()` on the structural table, adds the drill-down and CSV download.
- `format_ae_forestly()`: formats AE data and computes figure metadata (ranges, colors, headers, widths).
- `format_lt_forestly()` (`R/lt_table.R`): builds the interaction-free `lt` table: per-arm columns, spanners, `lt_dotplot` proportion figure, `lt_errorbar` risk-difference figure, hidden helper columns.
- `format_ae_listing()` (`R/ae_listing.R`): drill-down listings; handles factor inputs.
- All controls are lt's own. The AE-criteria dropdown and incidence slider are typed filters declared via `lt_interactive(filter = ...)` and bound to the hidden `parameter` and `hide_prop`/`hide_n` columns; the column menu is `lt_interactive(hide = ...)`; CSV is `lt_interactive(download = ...)`.
- forestly ships no widget JavaScript. `inst/css/forestly-widgets.css` only carries cell-truncation styles.

## Test data

`data/forestly_adae*.rda`, `forestly_adsl*.rda` (2- and 3-arm).
