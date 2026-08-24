# Helper functions used by test-format_ae_forestly.R

test_format_ae_forestly <- function() {
  prepare_ae_forestly(
    meta_ae_test(),
    population = "apat",
    observation = "wk12",
    parameter = "any;rel;ser"
  )
}
