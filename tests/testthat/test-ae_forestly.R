test_that("ae_forestly(): default setting can be executed without error", {
  outdata <- test_ae_forestly()
  html <- outdata |> ae_forestly()

  html_text <- as.character(html)
  expect_true(grepl("Incidence (%) in One or More Treatment Groups", html_text, fixed = TRUE))
})

test_that("ae_forestly(): test filter option", {
  outdata <- test_ae_forestly()
  html <- outdata |> ae_forestly(filter = c("n"))

  expect_true(grepl(
    "Number of AE in One or More Treatment Groups",
    as.character(html), fixed = TRUE
  ))
})

test_that("ae_forestly(): AE-criteria dropdown is labelled", {
  outdata <- test_ae_forestly()
  html_text <- as.character(ae_forestly(outdata))

  expect_true(grepl("AE Criteria", html_text, fixed = TRUE))
})

test_that("ae_forestly(): drill-down carries a treatment-group picker", {
  outdata <- test_ae_forestly()
  html_text <- as.character(ae_forestly(outdata))

  # The picker offers the listing's own arms, not aggregate forest columns
  # ("Total").
  expect_true(grepl('groups = ["Placebo","Low Dose","High Dose"]', html_text, fixed = TRUE))
  expect_false(grepl('"Total"', regmatches(
    html_text, regexpr("groups = \\[[^]]*\\]", html_text)
  ), fixed = TRUE))
})

test_that("ae_forestly(): no incidence filter when filter is NULL", {
  outdata <- test_ae_forestly()
  html_text <- as.character(ae_forestly(outdata, filter = NULL))

  # the incidence slider (identified by its label) is dropped
  expect_false(grepl("One or More Treatment Groups", html_text, fixed = TRUE))
})

test_that("ae_forestly(): both diff-toggle settings render without error", {
  outdata <- meta_ae_test() |>
    prepare_ae_forestly(
      population = "apat",
      observation = "wk12",
      parameter = "any;rel;ser"
    ) |>
    format_ae_forestly(display = c("n", "prop", "fig_prop", "fig_diff"))

  expect_s3_class(ae_forestly(outdata, display_diff_toggle = FALSE), "shiny.tag.list")
  expect_s3_class(ae_forestly(outdata, display_diff_toggle = TRUE), "shiny.tag.list")
})
