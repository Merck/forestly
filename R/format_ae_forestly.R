# Copyright (c) 2023 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
# All rights reserved.
#
# This file is part of the forestly program.
#
# forestly is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Format outdata for interactive forest plot
#'
#' @param outdata An `outdata` object created by [prepare_ae_forestly()].
#' @param display A character vector of measurement to be displayed.
#'   - `n`: Number of subjects with AE.
#'   - `prop`: Proportion of subjects with AE.
#'   - `total`: Total columns.
#'   - `diff`: Risk difference.
#' @param digits A value of digits to be displayed for proportion and
#'   risk difference.
#' @param width_term Width in px for AE term column.
#' @param width_fig Width in px for proportion and risk difference figure.
#' @param width_n Width in px for "N" columns.
#' @param width_prop Width in px for "(%)" columns.
#' @param width_diff Width in px for risk difference columns.
#' @param footer_space Space in px for footer to display legend.
#' @param color A vector of colors for analysis groups.
#'   Default value supports up to 4 groups.
#' @param diff_label x-axis label for risk difference.
#' @param show_ae_parameter A boolean value to display AE parameter column.
#'
#' @return An `outdata` object.
#'
#' @export
#'
#' @examples
#' adsl <- forestly_adsl[1:100,]
#' adae <- forestly_adae[1:100,]
#' meta_forestly(
#'   dataset_adsl = adsl,
#'   dataset_adae = adae,
#'   population_term = "apat",
#'   observation_term = "wk12",
#'   parameter = "any;rel"
#' ) |>
#'   prepare_ae_forestly()|>
#'   format_ae_forestly()
format_ae_forestly <- function(
    outdata,
    display = c("n", "prop", "fig_prop", "fig_diff"),
    digits = 1,
    width_term = 200,
    width_fig = 320,
    width_n = 40,
    width_prop = 60,
    width_diff = 80,
    footer_space = 90,
    color = NULL,
    diff_label = "Treatment <- Favor -> Placebo",
    show_ae_parameter = FALSE) {
  display <- tolower(display)

  display <- match.arg(
    display,
    c("n", "prop", "total", "diff", "fig_prop", "fig_diff"),
    several.ok = TRUE
  )

  display_total <- "total" %in% display
  display_diff <- "diff" %in% display

  # Define Variables
  index_reference <- outdata$reference_group
  index_total <- length(outdata$group)
  index_diff <- as.numeric(gsub("diff_", "", names(outdata$diff), fixed = TRUE))

  n_group_total <- index_total
  n_group <- index_total - 1
  n_group1 <- n_group - 1
  m_group <- ifelse(display_total, n_group_total, n_group)

  name_n <- names(outdata$n)[1:m_group]
  name_prop <- names(outdata$prop)[1:m_group]

  # Input checking
  if (is.null(color)) {
    if (n_group <= 2) {
      color <- c("#00857C", "#66203A")
    } else {
      if (n_group1 > 3) stop("Please define color to display groups")
      color <- c("#66203A", rev(c("#00857C", "#6ECEB2", "#BFED33")[1:n_group1]))
    }
  }

  if (length(color) < n_group) {
    stop("Please define more color to display groups")
  }

  # Define table data
  tbl <- data.frame(
    parameter = outdata$parameter_order,
    name = outdata$name,
    prop_fig = NA,
    diff_fig = NA,
    outdata$n[, 1:m_group],
    round(outdata$prop[, 1:m_group], digits = digits),
    round(outdata$diff, digits = digits),
    round(outdata$ci_lower, digits = digits),
    round(outdata$ci_upper, digits = digits),
    hide_prop = apply(outdata$prop[, 1:n_group], 1, max, na.rm = TRUE),
    hide_n = apply(outdata$n[, 1:n_group], 1, max, na.rm = TRUE)
  )

  if (!show_ae_parameter) tbl <- tbl[, c(2:ncol(tbl), 1)]

  rownames(tbl) <- NULL

  # JavaScript for plotly figures ----
  tbl_prop <- outdata$prop[, 1:n_group]
  y <- rep(NA, n_group)
  y[outdata$reference_group] <- mean(1:n_group1)
  y[-outdata$reference_group] <- 1:n_group1

  # Calculate the range of the forest plot
  fig_prop_range <- round(range(tbl_prop, na.rm = TRUE) + c(-2, 2))
  fig_prop_color <- color[1:n_group]

  # Function to create proportion of subjects figure
  js_prop_fig_cell <- sparkline_point_js(
    tbl = tbl,
    type = "cell",
    x = names(tbl_prop),
    y = y,
    xlim = fig_prop_range,
    color = fig_prop_color,
    width = width_fig,
    height = 30,
    text = paste0("x[", 1:n_group - 1, "]"),
    margin = c(0, 0, 0, 0, 0)
  )

  # Function to create Axis
  js_prop_fig_footer <- sparkline_point_js(
    tbl = data.frame(x = 1),
    x = rep("x", n_group),
    y = -1,
    type = "footer",
    xlab = "",
    xlim = fig_prop_range,
    height = footer_space,
    width = width_fig,
    color = fig_prop_color,
    legend = TRUE,
    legend_label = outdata$group[1:n_group],
    legend_title = "",
    legend_position = -0.8,
    legend_type = "point",
    margin = c(footer_space - 20, 0, 0, 0, 0)
  )

  # Function to create proportion difference figure
  tbl_diff <- data.frame(outdata$diff, outdata$ci_lower, outdata$ci_upper)
  fig_diff_range <- round(range(tbl_diff, na.rm = TRUE) + c(-2, 2))
  fig_diff_color <- fig_prop_color[index_diff]

  iter <- 1:ncol(outdata$diff) - 1
  text <- glue::glue("x[{iter}] + '(' + x_lower[{iter}] + ', ' + x_upper[{iter}] + ')'")
  js_diff_fig_cell <- sparkline_point_js(
    tbl = tbl,
    type = "cell",
    x = names(outdata$diff),
    x_lower = names(outdata$ci_lower),
    x_upper = names(outdata$ci_upper),
    y = 1:ncol(outdata$diff),
    xlim = fig_diff_range,
    color = fig_diff_color,
    width = width_fig,
    text = text,
    margin = c(0, 20, 0, 0, 0)
  )

  # Function to create Axis
  js_diff_fig_footer <- sparkline_point_js(
    tbl = data.frame(x = 1),
    x = "x",
    y = -1,
    type = "footer",
    xlab = diff_label,
    xlim = fig_diff_range,
    height = footer_space,
    width = width_fig,
    legend = FALSE,
    margin = c(footer_space - 20, 20, 0, 0, 0)
  )

  # Column Group information ----
  columnGroups <- list()
  for (i in 1:m_group) {
    columnGroups[[i]] <- reactable::colGroup(
      name = paste0(outdata$group[i], "<br> (N=", outdata$n_pop[i], ")"),
      html = TRUE,
      columns = c(name_n[i], name_prop[i])
    )
  }
  columnGroups[[m_group + 1]] <- reactable::colGroup(
    name = "Risk Difference (%) <br> vs. Placebo",
    html = TRUE,
    columns = names(outdata$diff)
  )

  # Column Definition ----

  # Format variables for group
  col_var <- list(
    parameter = reactable::colDef(
      header = "Type",
      show = show_ae_parameter
    ),
    name = reactable::colDef(
      header = "Adverse Events",
      minWidth = width_term, align = "right"
    )
  )

  # n column format
  col_n <- lapply(name_n, function(x) {
    reactable::colDef(
      header = "n", defaultSortOrder = "desc",
      minWidth = width_n, align = "center"
    )
  })
  names(col_n) <- name_n

  # prop column format
  col_prop <- lapply(name_prop, function(x) {
    reactable::colDef(
      header = "(%)", defaultSortOrder = "desc",
      minWidth = width_prop, align = "center",
      format = reactable::colFormat(
        prefix = "(",
        digits = 1,
        suffix = ")"
      )
    )
  })
  names(col_prop) <- name_prop

  # Define diff column
  diff_name <- c(names(outdata$diff))
  col_diff <- lapply(
    diff_name,
    function(x) {
      i <- as.numeric(gsub("diff_", "", x, fixed = TRUE))
      reactable::colDef(
        header = outdata$group[i],
        minWidth = width_diff,
        show = display_diff,
        format = reactable::colFormat(digits = 1)
      )
    }
  )
  names(col_diff) <- diff_name

  # Define ci columns
  ci_name <- c(names(outdata$ci_lower), names(outdata$ci_upper))
  col_ci <- lapply(
    ci_name,
    function(x) {
      reactable::colDef(show = FALSE)
    }
  )
  names(col_ci) <- ci_name

  # proportion format
  col_prop_fig <- list(prop_fig = reactable::colDef(
    header = "AE Proportion (%)",
    width = ifelse("fig_prop" %in% display, width_fig, 0),
    align = "center",
    cell = reactable::JS(js_prop_fig_cell),
    footer = reactable::JS(js_prop_fig_footer),
    html = TRUE,
    style = "font-size: 0px; padding: 0px; margin: 0px;",
    footerStyle = "font-size: 0px; padding: 0px; margin: 0px;"
  ))

  # difference format
  col_diff_fig <- list(diff_fig = reactable::colDef(
    header = "Risk Difference (%) + 95% CI <br> vs. Placebo",
    defaultSortOrder = "desc",
    width = ifelse("fig_diff" %in% display, width_fig, 0),
    align = "center",
    cell = reactable::JS(js_diff_fig_cell),
    footer = reactable::JS(js_diff_fig_footer),
    html = TRUE,
    style = "font-size: 0px; padding: 0px; margin: 0px;",
    footerStyle = "font-size: 0px; padding: 0px; margin: 0px;"
  ))

  # Format variables for slider bar
  col_sider <- list(
    hide_prop = reactable::colDef(show = FALSE),
    hide_n = reactable::colDef(show = FALSE)
  )

  # Combine column definition
  columns <- c(
    col_var, col_n, col_prop,
    col_diff, col_ci, col_sider,
    col_prop_fig, col_diff_fig
  )

  # Create outdata
  outdata$tbl <- tbl
  outdata$reactable_columns <- columns
  outdata$reactable_columns_group <- columnGroups
  outdata$display <- display

  outdata
}
