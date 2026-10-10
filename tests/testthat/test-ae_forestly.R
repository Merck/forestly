test_that("ae_forestly(): default setting can be executed without error", {
  outdata <- test_ae_forestly()
  html <- outdata |> ae_forestly()

  html_text <- as.character(html)
  # The forestly-ae class (scoping cell styles and sizing the container) is set
  # on lt's own `.lt-wrap` via lt_wrap(), serialized in the spec.
  expect_true(grepl('"wrap":{"class":"forestly-ae"}', html_text, fixed = TRUE))
  expect_true(grepl("Incidence (%) in One or More Treatment Groups", html_text, fixed = TRUE))
})

test_that("ae_forestly(): numeric width fixes the container width", {
  outdata <- test_ae_forestly()
  html_text <- as.character(outdata |> ae_forestly(width = 800))
  # numeric width becomes a px container width (still capped at the viewport)
  expect_true(grepl('"style":"width:800px"', html_text, fixed = TRUE))
})

test_that("ae_forestly(): test filter option", {
  outdata <- test_ae_forestly()
  html <- outdata |> ae_forestly(filter = c("n"))

  expect_true(grepl(
    "Number of AE in One or More Treatment Groups",
    as.character(html), fixed = TRUE
  ))
})

test_that("ae_forestly(): AE-criteria dropdown and incidence slider are lt typed filters", {
  outdata <- test_ae_forestly()
  html_text <- as.character(ae_forestly(outdata))

  # The parameter dropdown and incidence range slider are lt typed filters bound
  # to the hidden parameter / incidence columns, serialized into the lt spec --
  # not separate forestly DOM widgets.
  expect_true(grepl('"type":"select"', html_text, fixed = TRUE))
  expect_true(grepl('"type":"range"', html_text, fixed = TRUE))
  expect_true(grepl("AE Criteria", html_text, fixed = TRUE))
  # the old external control widgets are gone
  expect_false(grepl("forestly-param", html_text, fixed = TRUE))
  expect_false(grepl("forestly-slider", html_text, fixed = TRUE))
})

test_that("ae_forestly(): drill-down carries a treatment-group picker", {
  outdata <- test_ae_forestly()
  html_text <- as.character(ae_forestly(outdata))

  # The detail callback filters the listing by a `selected` set of arms, driven
  # by an lt control-bar picker (LT.ui popover + checklist on el._lt.bar) that
  # busts open details via el._lt.resetDetail. The onMount guard keys off the
  # callback's identity so it wires only this table.
  expect_true(grepl("LT.ui.popover(", html_text, fixed = TRUE))
  expect_true(grepl("LT.ui.checklist(", html_text, fixed = TRUE))
  expect_true(grepl("tbl._lt.resetDetail()", html_text, fixed = TRUE))
  expect_true(grepl("spec.interactive?.detail !== build", html_text, fixed = TRUE))
  # the funnel is wrapped in a labelled chip via lt's reusable LT.ui.chip, which
  # also hosts the popover's click so the whole chip (not just the funnel) opens it
  expect_true(grepl('label = "Treatment group"', html_text, fixed = TRUE))
  expect_true(grepl("LT.ui.chip(doc, label)", html_text, fixed = TRUE))
  expect_true(grepl("null, chip.el)", html_text, fixed = TRUE))
  # the picker offers the listing's arms, not aggregate forest columns ("Total")
  expect_true(grepl('groups = ["Placebo","Low Dose","High Dose"]', html_text, fixed = TRUE))
  expect_false(grepl('"Total"', regmatches(
    html_text, regexpr("groups = \\[[^]]*\\]", html_text)
  ), fixed = TRUE))
})

test_that("ae_forestly(): no incidence filter when filter is NULL", {
  outdata <- test_ae_forestly()
  html_text <- as.character(ae_forestly(outdata, filter = NULL))

  # the incidence range filter is dropped; the parameter dropdown stays
  expect_false(grepl('"type":"range"', html_text, fixed = TRUE))
  expect_true(grepl('"type":"select"', html_text, fixed = TRUE))
})

test_that("ae_forestly(): download button is opt-in via lt's download control", {
  outdata <- test_ae_forestly()
  # No bespoke forestly download control; the button is lt's own, requested
  # through the lt_interactive(download =) spec field.
  expect_false(grepl('"download"', as.character(ae_forestly(outdata)), fixed = TRUE))
  expect_true(grepl(
    '"download":"ae-forest.csv"',
    as.character(ae_forestly(outdata, download_button = TRUE)), fixed = TRUE
  ))
})

test_that("ae_forestly(): drill-down listings render lazily via lt", {
  outdata <- test_ae_forestly()
  html <- outdata |> ae_forestly()
  html_text <- as.character(html)

  # Detail listings are lightweight `lt` interactive tables rendered on expand
  # (see #158/#168), not eager per-row nested tables. The listing is embedded
  # once as a shared record store captured in the detail callback's closure and
  # built lazily on first expand (see #190); a row carries no record pointers --
  # the client builds a param+term/SOC lookup on first expand and resolves each
  # row by its own parameter/name (see #147). Each distinct record is stored once
  # and `members` maps each parameter to its record indices (dedupes the ~3x
  # cross-parameter overlap in the stacked listing).
  expect_true(grepl("getStore = () =>", html_text, fixed = TRUE))
  expect_true(grepl("s.records", html_text, fixed = TRUE))
  expect_true(grepl("s.members", html_text, fixed = TRUE))

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

test_that("ae_forestly(): forestly-widgets dependency ships styles only, no JS", {
  html <- test_ae_forestly() |> ae_forestly()
  deps <- htmltools::findDependencies(html)
  dep <- Filter(function(d) identical(d$name, "forestly-widgets"), deps)[[1]]

  # All controls are lt's own now; forestly ships only the table's CSS.
  expect_true("css/forestly-widgets.css" %in% unlist(dep$stylesheet))
  expect_null(dep$script)
  expect_false(file.exists(system.file("js", "forestly-widgets.js", package = "forestly")))
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
