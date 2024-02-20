# meta <-
#   adsl <- r2rtf::r2rtf_adsl
#   adsl$TRTA <- adsl$TRT01A
#   adsl$TRTA <- factor(adsl$TRTA,
#                       levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
#   )
#
#   adae <- r2rtf::r2rtf_adae
#   adae$TRTA <- factor(adae$TRTA,
#                       levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
#   )
#
#   plan <- plan(
#     analysis = "ae_summary", population = "apat",
#     observation = c("wk12", "wk24"), parameter = "any;rel;ser"
#   ) |>
#     add_plan(
#       analysis = "ae_specific", population = "apat",
#       observation = c("wk12", "wk24"),
#       parameter = c("any", "aeosi", "rel", "ser")
#     )
#
#   meta_adam(
#     population = adsl,
#     observation = adae
#   ) |>
#     define_plan(plan = plan) |>
#     define_population(
#       name = "apat",
#       group = "TRTA",
#       subset = quote(SAFFL == "Y")
#     ) |>
#     define_observation(
#       name = "wk12",
#       group = "TRTA",
#       subset = quote(SAFFL == "Y"),
#       label = "Weeks 0 to 12"
#     ) |>
#     define_observation(
#       name = "wk24",
#       group = "TRTA",
#       subset = quote(AOCC01FL == "Y"), # just for demo, another flag shall be used.
#       label = "Weeks 0 to 24"
#     ) |>
#     define_parameter(
#       name = "rel",
#       subset = quote(AEREL %in% c("POSSIBLE", "PROBABLE"))
#     ) |>
#     define_parameter(
#       name = "aeosi",
#       subset = quote(AEOSI == "Y"),
#       var = "AEDECOD",
#       soc = "AEBODSYS",
#       term1 = "",
#       term2 = "of special interest",
#       label = "adverse events of special interest"
#     ) |>
#     define_analysis(
#       name = "ae_summary",
#       title = "Summary of Adverse Events"
#     ) |>
#     meta_build()




# test_that("output is a list wihch contains dataframe: 'prop', 'diff', 'n_pop', 'ci_lower', 'ci_upper', 'p', 'ae_listing'", {
# ae_df <- prepare_ae_forestly(meta_example(), "apat", "wk12", "rel", c("soc", "par"), NULL, c('SEX', 'RACE', 'AGE'))
# expect_true("diff" %in% names(ae_df))
# expect_true("prop" %in% names(ae_df))
# expect_true("n_pop" %in% names(ae_df))
# expect_true("ci_lower" %in% names(ae_df))
# expect_true("ci_upper" %in% names(ae_df))
# expect_true("p" %in% names(ae_df))
# expect_true("ae_listing" %in% names(ae_df))
# expect_snapshot_output(ae_df)
# })
