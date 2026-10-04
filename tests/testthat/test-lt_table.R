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

test_that("format_lt_forestly(): SOC toggle controls the hide menu vs. hard hide", {
  out <- helper_lt_outdata()

  # No toggles -> no column-visibility menu at all.
  built_none <- format_lt_forestly(out, display_soc_toggle = FALSE, display_diff_toggle = FALSE)
  expect_null(built_none$hide_menu)

  # SOC toggle -> SOC column offered in the menu (starts hidden).
  built_soc <- format_lt_forestly(out, display_soc_toggle = TRUE, display_diff_toggle = FALSE)
  expect_true("soc_name" %in% built_soc$hide_menu)
})

test_that("format_lt_forestly(): diff toggle adds the numeric diff columns to the menu", {
  out <- helper_lt_outdata()
  built <- format_lt_forestly(out, display_soc_toggle = TRUE, display_diff_toggle = TRUE)

  # diff columns are named after the comparison arms (diff_<k>).
  diff_cols <- out$diff_name
  expect_true(all(diff_cols %in% built$hide_menu))
})
