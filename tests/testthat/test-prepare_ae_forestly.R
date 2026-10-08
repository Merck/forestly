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

test_that("prepare_ae_forestly() retains a specific AE with missing SOC", {
	meta <- meta_ae_test()
	meta$data_observation$AESER <- "N"
	meta$data_observation$AESER[1] <- "Y"
	meta$data_observation$AEBODSYS[1] <- NA_character_

	outdata <- prepare_ae_forestly(
		meta,
		population = "apat",
		observation = "wk12",
		parameter = "ser"
	)

	expect_equal(as.character(outdata$name), as.character(outdata$ae_listing$Adverse_Event))
	expect_true(is.na(outdata$soc_name))
	expect_equal(as.character(outdata$parameter_order), "ser")
})

test_that("ae_listing_placebo must be a single non-missing logical value", {
  invalid <- list(NULL, logical(), NA, c(TRUE, FALSE), 0, 1, "FALSE", list(FALSE))
  for (value in invalid) {
    expect_error(
      prepare_ae_forestly(NULL, ae_listing_placebo = value),
      "ae_listing_placebo must be a single non-missing logical value.",
      fixed = TRUE
    )
  }
})

test_that("ae_listing_placebo filters listings without changing analysis results", {
  meta <- meta_ae_test()
  for (unique_records in c(FALSE, TRUE)) {
    args <- list(
      meta = meta, parameter = "any;rel;ser", components = c("soc", "par"),
      ae_listing_unique = unique_records
    )
    all_groups <- do.call(prepare_ae_forestly, args)
    explicit_true <- do.call(prepare_ae_forestly, c(args, list(ae_listing_placebo = TRUE)))
    active_only <- do.call(prepare_ae_forestly, c(args, list(ae_listing_placebo = FALSE)))

    expect_equal(explicit_true, all_groups)
    expect_setequal(as.character(unique(all_groups$ae_listing$Treatment_Group)),
      c("Placebo", "Low Dose", "High Dose"))
    expected <- all_groups$ae_listing[
      all_groups$ae_listing$Treatment_Group != "Placebo", , drop = FALSE
    ]
    expect_equal(active_only$ae_listing, expected)
    expect_setequal(unique(active_only$ae_listing$param), c("any", "rel", "ser"))
    expect_setequal(as.character(unique(active_only$ae_listing$Treatment_Group)),
      c("Low Dose", "High Dose"))

    fields <- setdiff(names(all_groups), "ae_listing")
    expect_equal(active_only[fields], all_groups[fields])
    expect_equal(format_ae_forestly(active_only)$tbl, format_ae_forestly(all_groups)$tbl)
  }
})

test_that("ae_listing_placebo uses the reference group with customized labels", {
  meta <- meta_ae_test()
  for (dataset in c("data_population", "data_observation")) {
    meta[[dataset]]$TRTA <- factor(meta[[dataset]]$TRTA,
      levels = c("Low Dose", "Placebo", "High Dose"),
      labels = c("Treatment A", "Control Arm", "Treatment B")
    )
  }
  outdata <- prepare_ae_forestly(meta, parameter = "any",
    reference_group = 2, ae_listing_placebo = FALSE)
  expect_equal(outdata$group[outdata$reference_group], "Control Arm")
  expect_setequal(as.character(unique(outdata$ae_listing$Treatment_Group)),
    c("Treatment A", "Treatment B"))

  # An active reference group is excluded instead of guessing from its label.
  outdata <- prepare_ae_forestly(meta, parameter = "any",
    reference_group = 1, ae_listing_placebo = FALSE)
  expect_setequal(as.character(unique(outdata$ae_listing$Treatment_Group)),
    c("Control Arm", "Treatment B"))
})

test_that("ae_listing_placebo uses the default second reference group for two arms", {
  meta <- meta_ae_test()
  for (dataset in c("data_population", "data_observation")) {
    meta[[dataset]] <- meta[[dataset]][meta[[dataset]]$TRTA != "High Dose", ]
    meta[[dataset]]$TRTA <- factor(meta[[dataset]]$TRTA,
      levels = c("Low Dose", "Placebo"))
  }
  implicit <- prepare_ae_forestly(meta, parameter = "any", ae_listing_placebo = FALSE)
  explicit <- prepare_ae_forestly(meta, parameter = "any",
    reference_group = 2, ae_listing_placebo = FALSE)
  expect_equal(implicit, explicit)
  expect_equal(implicit$reference_group, 2)
  expect_equal(unique(as.character(implicit$ae_listing$Treatment_Group)), "Low Dose")
})

test_that("ae_listing_placebo supports character treatment variables", {
  meta <- meta_ae_test()
  meta$data_population$TRTA <- as.character(meta$data_population$TRTA)
  meta$data_observation$TRTA <- as.character(meta$data_observation$TRTA)
  # metalite.ae converts character treatment variables to alphabetically ordered factors.
  expect_warning(
    expect_warning(
      outdata <- prepare_ae_forestly(meta, parameter = "any",
        reference_group = 3, ae_listing_placebo = FALSE),
      "In population level data, force group variable"
    ),
    "In observation level data, force group variable"
  )
  expect_equal(outdata$group[outdata$reference_group], "Placebo")
  expect_setequal(unique(outdata$ae_listing$Treatment_Group), c("Low Dose", "High Dose"))
})

test_that("ae_listing_placebo retains placebo-only terms even with missing SOC", {
  meta <- meta_ae_test()
  meta$data_observation$AESER <- "N"
  i <- which(meta$data_observation$TRTA == "Placebo")[1]
  meta$data_observation$AESER[i] <- "Y"
  meta$data_observation$AEBODSYS[i] <- NA_character_

  all_groups <- prepare_ae_forestly(meta, parameter = "ser")
  active_only <- prepare_ae_forestly(meta, parameter = "ser", ae_listing_placebo = FALSE)
  expect_equal(nrow(all_groups$ae_listing), 1)
  expect_equal(nrow(active_only$ae_listing), 0)
  expect_equal(names(active_only$ae_listing), names(all_groups$ae_listing))
  expect_equal(length(active_only$name), 1)
  fields <- setdiff(names(all_groups), "ae_listing")
  expect_equal(active_only[fields], all_groups[fields])
})
