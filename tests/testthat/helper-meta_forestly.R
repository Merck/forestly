# Helper functions used by test-independent-testing-meta_forestly.R

test_meta_forestly_1 <- function() {
  adsl <- r2rtf::r2rtf_adsl |> dplyr::mutate(TRTA = TRT01A)
  adae <- r2rtf::r2rtf_adae

  meta_forestly(
    adsl,
    adae,
    population_term = "apat",
    observation_term = "wk12",
    parameter_term = "any;rel;ser"
  )
}

test_meta_forestly_2 <- function() {
  adsl <- r2rtf::r2rtf_adsl |> dplyr::mutate(TRTA = TRT01A)
  adae <- r2rtf::r2rtf_adae

  meta_forestly(
    adsl, adae,
    population_term = "trt",
    population_subset = TRTFL == "Y",
    observation_term = "trtem",
    observation_subset = TRTEMFL == "Y" & TRTFL == "Y",
    parameter_term = "any;g35"
  )
}
