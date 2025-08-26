test_that("Set `display` to ('n', 'prop', 'diff') then one has an additional risk difference column", {
  out <- test_format_ae_forestly()
  ae_frm <- format_ae_forestly(
    out,
    display = c("n", "prop", "diff"),
    digits = 1,
    width_term = 200,
    width_fig = 300,
    width_n = 40,
    width_prop = 60,
    width_diff = 80,
    footer_space = 90,
    color = NULL,
    diff_label = "Treatment <- Favor -> Placebo",
    show_ae_parameter = FALSE
  )

  # expect_named(ae_frm, c("n", "prop", "diff"))
  expect_named(ae_frm)
  expect_true("n" %in% names(ae_frm))
  expect_true("prop" %in% names(ae_frm))
  expect_true("diff" %in% names(ae_frm))
})

test_that("Set `display` to ('n', 'prop', 'total') then one has total column", {
  out <- test_format_ae_forestly()
  ae_frm <- format_ae_forestly(
    out,
    display = c("n", "prop", "total"),
    digits = 1,
    width_term = 200,
    width_fig = 300,
    width_n = 40,
    width_prop = 60,
    width_diff = 80,
    footer_space = 90,
    color = NULL,
    diff_label = "Treatment <- Favor -> Placebo",
    show_ae_parameter = FALSE
  )

  # expect_named(ae_frm, c("n", "prop", "total"))
  expect_named(ae_frm)
  expect_true("n" %in% names(ae_frm))
  expect_true("prop" %in% names(ae_frm))
  expect_false("total" %in% names(ae_frm))
})

test_that("Set `display` to ('diff', 'total') without ('n', 'prop') columns", {
  out <- test_format_ae_forestly()
  ae_frm <- format_ae_forestly(
    out,
    display = c("diff", "total"),
    digits = 1,
    width_term = 200,
    width_fig = 300,
    width_n = 40,
    width_prop = 60,
    width_diff = 80,
    footer_space = 90,
    color = NULL,
    diff_label = "Treatment <- Favor -> Placebo",
    show_ae_parameter = FALSE
  )

  # expect_named(ae_frm, c("n", "prop", "total"))
  expect_named(ae_frm)
  expect_true("diff" %in% names(ae_frm))
  expect_false("total" %in% names(ae_frm))
})

test_that("1. Set `display` to ('n', 'prop', 'total', 'diff') and change column width using argument
           2. Change `diff_label` to 'MK-xxxx <- Favor -> Placebo' and 'footer_space' to change location of footer", {
  out <- test_format_ae_forestly()
  ae_frm <- format_ae_forestly(
    out,
    display = c("n", "prop", "total", "diff", "fig_diff", "fig_prop"),
    digits = 1,
    width_term = 200,
    width_fig = 300,
    width_n = 40,
    width_prop = 60,
    width_diff = 80,
    footer_space = 90,
    color = NULL,
    diff_label = "MK-XXXX <- Favor -> Placebo",
    show_ae_parameter = FALSE
  )

  expect_equal(ae_frm$reactable_columns$diff_fig$width, 300)
  expect_equal(ae_frm$reactable_columns$prop_fig$width, 300)
  expect_equal(ae_frm$reactable_columns$n_1$minWidth, 40)
  expect_equal(ae_frm$reactable_columns$n_3$minWidth, 40)
  expect_equal(ae_frm$reactable_columns$prop_1$minWidth, 60)
  expect_equal(ae_frm$reactable_columns$prop_2$minWidth, 60)
  expect_equal(ae_frm$reactable_columns$prop_4$minWidth, 60)
})

test_that("Set `show` to TRUE then display column 'Type' and change color for tratment group", {
  out <- test_format_ae_forestly()
  ae_frm <- format_ae_forestly(
    out,
    display = c("n", "prop", "total", "diff"),
    digits = 1,
    width_term = 200,
    width_fig = 300,
    width_n = 40,
    width_prop = 60,
    width_diff = 80,
    footer_space = 90,
    color = c("BLACK", "BLUE", "YELLOW", "PINK"),
    diff_label = "Treatment <- Favor -> Placebo",
    show_ae_parameter = TRUE
  )

  expect_equal(ae_frm$reactable_columns$parameter$header, "Type")
})

test_that("Add variable name not in n, prop, total, diff causes error", {
  out <- test_format_ae_forestly()
  expect_error(
    format_ae_forestly(
      out,
      display = c("ci_1", "ci_2"),
      digits = 1,
      width_term = 200,
      width_fig = 300,
      width_n = 40,
      width_prop = 60,
      width_diff = 80,
      footer_space = 90,
      color = NULL,
      diff_label = "Treatment <- Favor -> Placebo",
      show_ae_parameter = FALSE
    )
  )
})
