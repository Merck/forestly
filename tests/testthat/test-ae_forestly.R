test_that("ae_forestly(): default setting can be executed without error", {
  outdata <- test_ae_forestly()
  html <- outdata |> ae_forestly()
  container <- html[[length(html)]]

  # The output is a browsable tagList whose last element is the forestly
  # container div holding the controls and the lt table.
  expect_equal(container$name, "div")
  expect_equal(container$attribs$class, "forestly-ae")
  expect_true(grepl("width:1400px", container$attribs$style, fixed = TRUE))

  html_text <- as.character(html)
  expect_true(grepl("Incidence (%) in One or More Treatment Groups", html_text, fixed = TRUE))
})

test_that("ae_forestly(): test filter and width option", {
  outdata <- test_ae_forestly()
  html <- outdata |> ae_forestly(filter = c("n"), width = 1500)
  container <- html[[length(html)]]

  expect_true(grepl("width:1500px", container$attribs$style, fixed = TRUE))
  expect_true(grepl(
    "Number of AE in One or More Treatment Groups",
    as.character(html), fixed = TRUE
  ))
})

test_that("ae_forestly(): forestly controls (dropdown + slider) are emitted", {
  outdata <- test_ae_forestly()
  html_text <- as.character(ae_forestly(outdata))

  # The parameter dropdown and the incidence range slider are forestly-owned
  # widgets that drive the lt table through its el._lt.filter() contract.
  expect_true(grepl("forestly-controls", html_text, fixed = TRUE))
  expect_true(grepl("forestly-param", html_text, fixed = TRUE))
  expect_true(grepl("forestly-slider", html_text, fixed = TRUE))
  # The slider targets the hidden incidence helper column.
  expect_true(grepl("data-col=\"hide_prop\"", html_text, fixed = TRUE))
})

test_that("ae_forestly(): no slider when filter is NULL", {
  outdata <- test_ae_forestly()
  html_text <- as.character(ae_forestly(outdata, filter = NULL))

  expect_false(grepl("forestly-slider", html_text, fixed = TRUE))
})

test_that("ae_forestly(): download button is opt-in", {
  outdata <- test_ae_forestly()
  expect_false(grepl("forestly-download", as.character(ae_forestly(outdata)), fixed = TRUE))
  expect_true(grepl(
    "forestly-download",
    as.character(ae_forestly(outdata, dowload_button = TRUE)), fixed = TRUE
  ))
})

test_that("ae_forestly(): drill-down listings render lazily via lt", {
  outdata <- test_ae_forestly()
  html <- outdata |> ae_forestly()
  html_text <- as.character(html)

  # Detail listings are lightweight `lt` interactive tables rendered on expand
  # (see #158/#168), not eager per-row nested tables. The listing is embedded
  # once as a shared record store captured in the detail callback's closure;
  # each row carries only the record indices it needs (see #147).
  expect_true(grepl("const store =", html_text, fixed = TRUE))
  expect_true(grepl("store.records", html_text, fixed = TRUE))
  expect_true(grepl("store.index", html_text, fixed = TRUE))

  # The lt runtime, interactivity extension, and inline-plot extension are
  # bundled as HTML dependencies.
  deps <- htmltools::findDependencies(html)
  dep_names <- vapply(deps, function(d) d$name, character(1))
  expect_true("lt" %in% dep_names)
  expect_true("forestly-widgets" %in% dep_names)

  lt_dep <- Filter(function(d) identical(d$name, "lt"), deps)[[1]]
  expect_true("lt-interactive.js" %in% unlist(lt_dep$script))
  expect_true("lt-plot.js" %in% unlist(lt_dep$script))
})

test_that("ae_forestly(): forestly-widgets dependency ships the controller script", {
  js_path <- system.file("js", "forestly-widgets.js", package = "forestly")
  expect_true(file.exists(js_path))
  js <- paste(readLines(js_path, warn = FALSE), collapse = "\n")

  # The widgets drive the table through lt's el._lt.filter() contract.
  expect_true(grepl("_lt", js, fixed = TRUE))
  expect_true(grepl("filter", js, fixed = TRUE))
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
