# forestly 0.1.2

- Fix a bug that prevents reactable from rendering under reactR >= 0.6.0 (#67).
- Add a new argument `filter_label` to `ae_forestly()` for controlling slider bar label (#68).

# forestly 0.1.1

- Add button to display SOC column for subset.
- Refine the default value of `meta_forestly()`.
- Update `format_ae_listing()` to be better aligned with oncology AEs reporting.
- Enhance functionality related to drill-down listing.
- Inherit paramter_term from all parameters of a metadata by default (#20).
- Allow user to specify treatment group (#15).
- Add warning when using tibble as input (#17).
- Fix bug when there is no record of a selected AE category (#9).
- Fix bug when there are multiple plots displayed on one page (#12).
- Update dataset `forestly_adae` since a variable was a factor
  which is unexpected.

# forestly 0.1.0

- Initial version.
- Added a `NEWS.md` file to track changes to the package.
