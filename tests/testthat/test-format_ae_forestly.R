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
    diff_label = "Treatment <- Favor -> Placebo"
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
    diff_label = "Treatment <- Favor -> Placebo"
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
    diff_label = "Treatment <- Favor -> Placebo"
  )

  # expect_named(ae_frm, c("n", "prop", "total"))
  expect_named(ae_frm)
  expect_true("diff" %in% ae_frm$display[1])
  expect_true("total" %in% ae_frm$display[2])
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
    diff_label = "MK-XXXX <- Favor -> Placebo"
  )

  # Column widths are carried as metadata (px) for ae_forestly() to feed into
  # lt_width(); the reactable column specs are gone.
  expect_equal(ae_frm$widths$fig, 300)
  expect_equal(ae_frm$widths$term, 200)
  expect_equal(ae_frm$widths$n, 40)
  expect_equal(ae_frm$widths$prop, 60)
  expect_equal(ae_frm$widths$diff, 80)
  expect_equal(ae_frm$widths$footer, 90)
})

test_that("Parameter helper column is carried in tbl for the client to filter on", {
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
    diff_label = "Treatment <- Favor -> Placebo"
  )

  # The parameter column stays in tbl (hidden by lt_hide() later) so the
  # parameter dropdown can filter rows on it client-side.
  expect_true("parameter" %in% names(ae_frm$tbl))
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
