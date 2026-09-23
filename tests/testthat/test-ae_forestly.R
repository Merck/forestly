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

test_that("ae_forestly(): detail table embeds custom search method", {
  outdata <- test_ae_forestly()
  html <- outdata |> ae_forestly()
  html_text <- as.character(html)

  # The custom search / filter methods reference the shared globals defined in
  # inst/js/search-filter.js (deduplicated to keep the widget small), rather
  # than inlining the function body into every nested table.
  expect_true(grepl("searchMethod", html_text, fixed = TRUE))
  expect_true(grepl("window.__forestly_filter_table", html_text, fixed = TRUE))
  expect_true(grepl("window.__forestly_filter_column", html_text, fixed = TRUE))
})

test_that("search-filter.js defines the shared globals and search logic", {
  js_path <- system.file("js", "search-filter.js", package = "forestly")
  expect_true(file.exists(js_path))
  js <- paste(readLines(js_path, warn = FALSE), collapse = "\n")

  # Both globals referenced by search_filter_js() are defined here.
  expect_true(grepl("window.__forestly_filter_column", js, fixed = TRUE))
  expect_true(grepl("window.__forestly_filter_table", js, fixed = TRUE))
  # Substring `!` negation and JS expression modes live in the shared body.
  expect_true(grepl("var negate = v.charAt(0) === '!'", js, fixed = TRUE))
  expect_true(grepl("new Function('x'", js, fixed = TRUE))
})

test_that("ae_forestly(): toggle risk difference button is hidden by default", {
  outdata <- meta_ae_test() |>
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
  outdata <- meta_ae_test() |>
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
