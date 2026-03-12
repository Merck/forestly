test_that("ae_forestly(): default setting can be executed without error", {
  outdata <- test_ae_forestly()
  html <- outdata |> ae_forestly()
  html <- html[[length(html)]]

  expect_equal(html$name, "div")
  expect_equal(html$attribs$class, "container-fluid crosstalk-bscols")
  expect_true(grepl("width:1400px", html$children[[1]], fixed = TRUE))
  expect_true(grepl("Incidence (%) in One or More Treatment Groups", html$children[[1]], fixed = TRUE))
})

test_that("ae_forestly(): test filter and width option", {
  outdata <- test_ae_forestly()
  html <- outdata |> ae_forestly(filter = c("n"), width = 1500)
  html <- html[[length(html)]]

  expect_true(grepl("width:1500px", html$children[[1]], fixed = TRUE))
  expect_true(grepl("Number of AE in One or More Treatment Groups", html$children[[1]], fixed = TRUE))
})

test_that("ae_forestly(): toggle risk difference button is hidden by default", {
  outdata <- metalite.ae::meta_ae_example() |>
    prepare_ae_forestly(
      population = "apat",
      observation = "wk12",
      parameter = "any;rel;ser"
    ) |>
    format_ae_forestly(display = c("n", "prop", "fig_prop", "fig_diff", "diff"))

  html <- outdata |> ae_forestly(display_diff_toggle = FALSE)
  html_text <- as.character(html)

  expect_false(grepl("Show/Hide Risk Difference", html_text, fixed = TRUE))
})

test_that("ae_forestly(): toggle risk difference button can be enabled", {
  outdata <- metalite.ae::meta_ae_example() |>
    prepare_ae_forestly(
      population = "apat",
      observation = "wk12",
      parameter = "any;rel;ser"
    ) |>
    format_ae_forestly(display = c("n", "prop", "fig_prop", "fig_diff", "diff"))

  html <- outdata |> ae_forestly(display_diff_toggle = TRUE)
  html_text <- as.character(html)

  expect_true(grepl("Show/Hide Risk Difference", html_text, fixed = TRUE))
  expect_true(grepl("control_diff", html_text, fixed = TRUE))
})
