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

test_that("prepare_ae_forestly() retains a parameter with one AE record", {
  meta <- meta_ae_test()
  meta$data_observation$AESER <- "N"
  meta$data_observation$AESER[1] <- "Y"

  outdata <- prepare_ae_forestly(
    meta,
    population = "apat",
    observation = "wk12",
    parameter = "any;rel;ser"
  )

  # The single serious AE must survive into the forest-plot inference rows.
  ser_rows <- as.character(outdata$parameter_order) == "ser"
  expect_true(any(ser_rows))

  # Exactly one retained ser row is a specific-AE row (non-missing SOC name),
  # i.e. the fix keeps the low-frequency event row rather than a summary row.
  expect_equal(sum(ser_rows & !is.na(outdata$soc_name)), 1)

  # The retained inference row corresponds to the same AE term listed in the
  # drill-down listing for the ser parameter.
  ser_name <- outdata$name[ser_rows & !is.na(outdata$soc_name)]
  ser_listing <- outdata$ae_listing[outdata$ae_listing$param == "ser", ]
  expect_equal(nrow(ser_listing), 1)
  expect_equal(
    tolower(ser_name),
    unique(tolower(ser_listing$Adverse_Event))
  )
})
