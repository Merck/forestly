# Regression tests for the panel plot builders in R/plot.R.
#
# plot_dot(), plot_errorbar() and table_panel() have no other automated
# coverage, so these tests snapshot the computed ggplot2 layer data (the fully
# resolved x/y positions and aesthetics) for a fixed input. They lock the
# current behavior so the shared-setup deduplication can be verified as a no-op.

# Fixed fixture exercising the multi-group and missing-value paths. "Gamma" has
# NA risk-difference/CI values, which drives the wide-format row-count + nudge
# logic that differs between the long (plot_dot) and wide (plot_errorbar,
# table_panel) builders.
plot_fixture <- function() {
  data.frame(
    name = c("Alpha", "Beta", "Gamma"),
    n_1 = c(10L, 5L, 0L),
    n_2 = c(8L, 3L, 1L),
    prop_1 = c(20.5, 10.2, 0.0),
    prop_2 = c(16.0, 6.1, 2.0),
    diff_1 = c(4.5, 4.1, NA_real_),
    lower_1 = c(-1.2, -0.5, NA_real_),
    upper_1 = c(10.1, 8.7, NA_real_),
    stringsAsFactors = FALSE
  )
}

# Reduce a built plot to a stable, comparable digest: per layer, the resolved
# positional and aesthetic columns, with numerics rounded to avoid noise.
layer_digest <- function(g) {
  built <- ggplot2::ggplot_build(g)
  lapply(built$data, function(d) {
    keep <- intersect(
      c(
        "x", "y", "xmin", "xmax", "ymin", "ymax",
        "colour", "fill", "shape", "label", "alpha", "size",
        "PANEL", "group"
      ),
      names(d)
    )
    d <- d[, keep, drop = FALSE]
    is_num <- vapply(d, is.numeric, logical(1))
    d[is_num] <- lapply(d[is_num], round, digits = 6)
    d
  })
}

test_that("plot_dot() layer data is stable", {
  g <- plot_dot(
    plot_fixture(),
    prop_cols = c("prop_1", "prop_2"),
    y_var = "name",
    label = c("Treatment", "Placebo")
  )
  expect_snapshot_value(layer_digest(g), style = "json2")
})

test_that("plot_errorbar() layer data is stable", {
  g <- plot_errorbar(
    plot_fixture(),
    ci_cols = c("diff_1", "lower_1", "upper_1"),
    y_var = "name",
    label = c("Treatment", "Placebo")
  )
  expect_snapshot_value(layer_digest(g), style = "json2")
})

test_that("table_panel() layer data is stable (text_format_by = column)", {
  g <- table_panel(
    plot_fixture(),
    n_cols = c("n_1", "n_2"),
    prop_cols = c("prop_1", "prop_2"),
    y_var = "name"
  )
  expect_snapshot_value(layer_digest(g), style = "json2")
})

test_that("table_panel() layer data is stable (text_format_by = group)", {
  g <- table_panel(
    plot_fixture(),
    n_cols = c("n_1", "n_2"),
    prop_cols = c("prop_1", "prop_2"),
    y_var = "name",
    text_format_by = "group",
    text_color = c("#00857C", "#66203A")
  )
  expect_snapshot_value(layer_digest(g), style = "json2")
})
