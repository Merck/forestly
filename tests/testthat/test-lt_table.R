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

test_that("ae_forestly(): figures appear only when listed in `display`", {
  m <- meta_ae_test() |>
    prepare_ae_forestly(population = "apat", observation = "wk12", parameter = "any;rel;ser")
  has_figs <- function(display) {
    html <- paste(as.character(ae_forestly(format_ae_forestly(m, display = display))), collapse = "")
    c(prop = grepl("AE Proportion (%)", html, fixed = TRUE),
      diff = grepl("Risk Difference", html, fixed = TRUE))
  }
  expect_equal(has_figs(c("n", "prop", "fig_prop", "fig_diff")), c(prop = TRUE, diff = TRUE))
  expect_equal(has_figs(c("n", "prop", "fig_prop")), c(prop = TRUE, diff = FALSE))
  expect_equal(has_figs(c("n", "prop", "fig_diff")), c(prop = FALSE, diff = TRUE))
  expect_equal(has_figs(c("n", "prop")), c(prop = FALSE, diff = FALSE))
})

test_that("format_lt_forestly(): n and (%) columns follow `display`", {
  m <- meta_ae_test() |>
    prepare_ae_forestly(population = "apat", observation = "wk12", parameter = "any;rel;ser")
  shown <- function(display) format_lt_forestly(format_ae_forestly(m, display = display))$range_cols

  expect_false(any(grepl("^(n|prop)_", shown("fig_prop"))))
  expect_true(length(shown("n")) > 0 && all(grepl("^n_", shown("n"))))
  expect_true(any(grepl("^prop_", shown("prop"))))
})
