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

test_that("ae_forestly(): main table uses the shared column filter global", {
  outdata <- test_ae_forestly()
  html <- outdata |> ae_forestly()
  html_text <- as.character(html)

  # The main table's per-column filter references the shared global defined in
  # inst/js/search-filter.js (deduplicated to keep the widget small), rather
  # than inlining the function body into every column.
  expect_true(grepl("window.__forestly_filter_column", html_text, fixed = TRUE))
})

test_that("ae_forestly(): drill-down listings render lazily via lt", {
  outdata <- test_ae_forestly()
  html <- outdata |> ae_forestly()
  html_text <- as.character(html)

  # Detail listings are lightweight `lt` interactive tables rendered on expand
  # (see #158), not eager per-row nested reactables. The specs are embedded once
  # in a closure captured by the `details` renderer and drawn with LT.render().
  expect_true(grepl("const store =", html_text, fixed = TRUE))
  expect_true(grepl("window.LT.render", html_text, fixed = TRUE))
  # The listing is embedded once as a shared record store; each row carries only
  # the indices it needs and the client gathers its slice on expand (see #147),
  # rather than shipping a full data slice per row.
  expect_true(grepl("store.records", html_text, fixed = TRUE))
  expect_true(grepl("store.index", html_text, fixed = TRUE))
  # The lt interactivity extension is bundled as an HTML dependency.
  deps <- htmltools::findDependencies(html)
  lt_dep <- Filter(function(d) identical(d$name, "lt"), deps)
  expect_true(length(lt_dep) > 0)
  expect_true("lt-interactive.js" %in% lt_dep[[1]]$script)
})

test_that("ae_forestly(): excluded placebo records are not embedded in the widget", {
  meta <- meta_ae_test()
  meta$data_observation$Listing_Record <- ifelse(
    meta$data_observation$TRTA == "Placebo", "PLACEBO-LISTING-RECORD", "ACTIVE-LISTING-RECORD"
  )
  outdata <- prepare_ae_forestly(meta, parameter = "any", ae_listing_placebo = FALSE,
    ae_listing_display = c("USUBJID", "Listing_Record")) |>
    format_ae_forestly()
  html_text <- as.character(ae_forestly(outdata))

  expect_false(grepl("PLACEBO-LISTING-RECORD", html_text, fixed = TRUE))
  expect_true(grepl("ACTIVE-LISTING-RECORD", html_text, fixed = TRUE))
  expect_true(grepl("window.LT.render", html_text, fixed = TRUE))
})

test_that("ae_forestly(): terms with no eligible records have no drill-down", {
  meta <- meta_ae_test()
  meta$data_observation$AESER <- "N"
  i <- which(meta$data_observation$TRTA == "Placebo")[1]
  meta$data_observation$AESER[i] <- "Y"
  meta$data_observation$Listing_Record <- "OTHER-LISTING-RECORD"
  meta$data_observation$Listing_Record[i] <- "PLACEBO-ONLY-SERIOUS-RECORD"
  outdata <- prepare_ae_forestly(meta, parameter = "any;ser",
    ae_listing_placebo = FALSE, ae_listing_display = c("USUBJID", "Listing_Record")) |>
    format_ae_forestly()
  expect_true(any(outdata$tbl$parameter == "ser"))
  html_text <- as.character(ae_forestly(outdata))
  expect_false(grepl("PLACEBO-ONLY-SERIOUS-RECORD", html_text, fixed = TRUE))
  expect_true(grepl("if (!enc || !enc.length) return null;", html_text, fixed = TRUE))

  # Also allow an entirely empty shared record store, keeping its column schema.
  empty <- prepare_ae_forestly(meta, parameter = "ser", ae_listing_placebo = FALSE,
    ae_listing_display = c("USUBJID", "Listing_Record")) |>
    format_ae_forestly()
  expect_equal(nrow(empty$ae_listing), 0)
  expect_true(nrow(empty$tbl) > 0)
  expect_no_error(ae_forestly(empty))
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
