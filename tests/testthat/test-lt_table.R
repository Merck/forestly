# format_lt_forestly() turns an outdata object into the structural lt table;
# ae_forestly() layers interactivity on top. These guard the column/menu wiring.

helper_lt_outdata <- function() {
  meta_ae_test() |>
    prepare_ae_forestly(
      population = "apat",
      observation = "wk12",
      parameter = "any;rel;ser"
    ) |>
    format_ae_forestly(display = c("n", "prop", "fig_prop", "fig_diff"))
}

test_that("format_lt_forestly(): returns an lt_tbl and a hide-menu spec", {
  built <- format_lt_forestly(helper_lt_outdata())
  expect_s3_class(built$x, "lt_tbl")
  expect_true("hide_menu" %in% names(built))
})

test_that("format_lt_forestly(): SOC toggle offers or excludes the SOC column", {
  out <- helper_lt_outdata()

  # SOC toggle -> SOC column offered in the menu (starts hidden).
  expect_true("soc_name" %in% format_lt_forestly(out, display_soc_toggle = TRUE)$hide_menu)

  # Excluded: not in the menu, and nowhere in the rendered table.
  expect_false("soc_name" %in% format_lt_forestly(out, display_soc_toggle = FALSE)$hide_menu)
  html <- paste(as.character(ae_forestly(out, display_soc_toggle = FALSE)), collapse = "")
  expect_false(grepl("soc_name", html, fixed = TRUE))
})

test_that("format_lt_forestly(): diff columns are offered in the show/hide menu", {
  out <- helper_lt_outdata()
  built <- format_lt_forestly(out)

  # diff columns are named after the comparison arms (diff_<k>).
  expect_true(all(out$diff_name %in% built$hide_menu))
})
