library(metalite)
library(metalite.ae)

tmp <- metalite.ae::meta_ae_example() |>
  prepare_ae_forestly(
    population = "apat",
    observation = "wk12",
    parameter = "any;rel;ser"
  )

outdata <- tmp |> format_ae_forestly(display = c("n", "prop", "fig_prop"))

test_that("ae_forestly(): default setting can be executed without error", {
  html <- outdata |> ae_forestly()
  html <- html[[length(html)]]

  testthat::expect_equal(html$name, "div")
  testthat::expect_equal(html$attribs$class, "container-fluid crosstalk-bscols")
  testthat::expect_true(grepl("width:1400px", html$children[[1]], fixed = TRUE))
  testthat::expect_true(grepl("Incidence (%) in One or More Treatment Groups", html$children[[1]], fixed = TRUE))
})

test_that("ae_forestly(): test filter and width option", {
  html <- outdata |> ae_forestly(filter = c("n"), width = 1500)
  html <- html[[length(html)]]

  testthat::expect_true(grepl("width:1500px", html$children[[1]], fixed = TRUE))
  testthat::expect_true(grepl("Number of AE in One or More Treatment Groups", html$children[[1]], fixed = TRUE))
})
