# Helper functions used by test-ae_forestly.R

test_ae_forestly <- function() {
  meta_ae_test() |>
    prepare_ae_forestly(
      population = "apat",
      observation = "wk12",
      parameter = "any;rel;ser"
    ) |>
    format_ae_forestly(display = c("n", "prop", "fig_prop"))
}
