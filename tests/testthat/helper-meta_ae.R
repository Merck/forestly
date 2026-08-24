meta_ae_test <- function() {
  adsl <- r2rtf::r2rtf_adsl
  adsl$TRTA <- factor(
    adsl$TRT01A,
    levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
    labels = c("Placebo", "Low Dose", "High Dose")
  )

  adae <- r2rtf::r2rtf_adae
  adae$TRTA <- factor(
    adae$TRTA,
    levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
    labels = c("Placebo", "Low Dose", "High Dose")
  )
  adae$AEACN <- sample(
    x = c(
      "DOSE NOT CHANGED", "DRUG INTERRUPTED", "DRUG WITHDRAWN",
      "NOT APPLICABLE", "UNKNOWN"
    ),
    size = length(adae$USUBJID),
    prob = c(0.7, 0.1, 0.05, 0.1, 0.05),
    replace = TRUE
  )

  analysis_plan <- metalite::plan(
    analysis = "ae_forestly",
    population = "apat",
    observation = "wk12",
    parameter = "any;rel;ser"
  )

  metalite::meta_adam(observation = adae, population = adsl) |>
    metalite::define_plan(plan = analysis_plan) |>
    metalite::define_population(
      name = "apat",
      var = c("USUBJID", "SAFFL", "TRTA", "SITEID", "SEX", "RACE", "AGE"),
      group = "TRTA",
      subset = quote(SAFFL == "Y"),
      label = "All Participants as Treated"
    ) |>
    metalite::define_observation(
      name = "wk12",
      var = c(
        "USUBJID", "SAFFL", "TRTA", "SITEID", "SEX", "RACE", "AGE",
        "ASTDY", "AEDECOD", "AEBODSYS", "AESEV", "AESER", "AEREL",
        "AEACN", "AEOUT", "ADURN", "ADURU"
      ),
      group = "TRTA",
      subset = quote(SAFFL == "Y"),
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
    metalite::define_parameter(
      name = "rel",
      term1 = "Drug-Related",
      term2 = "",
      subset = quote(AEREL %in% c("POSSIBLE", "PROBABLE")),
      var = "AEDECOD",
      soc = "AEBODSYS",
      label = "Drug-related AEs"
    ) |>
    metalite::define_parameter(
      name = "ser",
      term1 = "Serious",
      term2 = "",
      subset = quote(AESER == "Y"),
      var = "AEDECOD",
      soc = "AEBODSYS",
      label = "Serious AEs"
    ) |>
    metalite::define_analysis(
      name = "ae_forestly",
      title = "Interactive Forest Plot"
    ) |>
    metalite::meta_build()
}