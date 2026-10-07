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
#' @param digits A number of digits after decimal point to be displayed for proportion and
#'   risk difference.
#' @param width_term Width in px for AE term column.
#' @param width_fig Width in px for proportion and risk difference figure.
#' @param width_n Width in px for "N" columns.
#' @param width_prop Width in px for "(%)" columns.
#' @param width_diff Width in px for risk difference columns.
#' @param footer_space Space in px for footer to display legend.
#' @param prop_range A vector of lower and upper limit of x-axis
#'   for proportion figure.
#' @param diff_range A vector of lower and upper limit of x-axis
#'   for risk difference figure.
#' @param color A vector of colors for analysis groups.
#'   Default value supports up to 4 groups.
#' @param ae_col_header Column header for adverse events item columns.
#'   If NULL (default) and "par" specified in `components` from `prepare_ae_forestly()`, uses "Adverse Event".
#'   If NULL and "soc" specified in `components` from `prepare_ae_forestly()`, uses "System Organ Class" for "soc".
#' @param diff_label x-axis label for risk difference.
#' @param diff_col_header Column header for risk difference table columns.
#'   If NULL (default), uses "Risk Difference (%) <br> vs. Reference Group".
#' @param diff_fig_header Column header for risk difference figure.
#'   If NULL (default), uses "Risk Difference (%) + 95% CI <br> vs. Reference Group".
#'
#' @return An `outdata` object.
#'
#' @export
#'
#' @examples
#' adsl <- forestly_adsl
#' adae <- forestly_adae
#' adsl$TRTA <- factor(
#'   adsl$TRT01A,
#'   levels = c("Xanomeline Low Dose", "Placebo"),
#'   labels = c("Low Dose", "Placebo")
#' )
#' adae$TRTA <- factor(
#'   adae$TRTA,
#'   levels = c("Xanomeline Low Dose", "Placebo"),
#'   labels = c("Low Dose", "Placebo")
#' )
#'
#' analysis_plan <- metalite::plan(
#'   analysis = "ae_forestly",
#'   population = "apat",
#'   observation = "wk12",
#'   parameter = "any"
#' )
#' meta <- metalite::meta_adam(population = adsl, observation = adae) |>
#'   metalite::define_plan(plan = analysis_plan) |>
#'   metalite::define_population(
#'     name = "apat",
#'     var = c("USUBJID", "SAFFL", "TRTA", "SITEID", "SEX", "RACE", "AGE"),
#'     group = "TRTA",
#'     subset = SAFFL == "Y",
#'     label = "All Participants as Treated"
#'   ) |>
#'   metalite::define_observation(
#'     name = "wk12",
#'     var = c(
#'       "USUBJID", "SAFFL", "TRTA", "SITEID", "SEX", "RACE", "AGE",
#'       "ASTDY", "AEDECOD", "AEBODSYS", "AESER", "AEREL", "AEACN",
#'       "AEOUT", "ADURN", "ADURU"
#'     ),
#'     group = "TRTA",
#'     subset = SAFFL == "Y",
#'     label = "Weeks 0 to 12"
#'   ) |>
#'   metalite::define_parameter(
#'     name = "any",
#'     term1 = "",
#'     term2 = "",
#'     var = "AEDECOD",
#'     soc = "AEBODSYS",
#'     label = "All AEs"
#'   ) |>
#'   metalite::define_analysis(
#'     name = "ae_forestly",
#'     label = "Interactive forest plot"
#'   ) |>
#'   metalite::meta_build()
#'
#' meta |>
#'   prepare_ae_forestly(parameter = "any") |>
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
    prop_range = NULL,
    diff_range = NULL,
    color = NULL,
    ae_col_header = NULL,
    diff_label = "Treatment <- Favor -> Placebo",
    diff_col_header = NULL,
    diff_fig_header = NULL) {
  display <- tolower(display)

  display <- match.arg(
    display,
    c("n", "prop", "total", "diff", "fig_prop", "fig_diff"),
    several.ok = TRUE
  )

  display_n <- "n" %in% display
  display_prop <- "prop" %in% display
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

  # Get reference group name for headers
  reference_name <- outdata$group[index_reference]

  # Set default headers if not provided
  if (is.null(ae_col_header)) {
    if ("par" %in% outdata$components) {
      ae_col_header <- "Adverse Event"
    } else if ("soc" %in% outdata$components) {
      ae_col_header <- "System Organ Class"
    }
  }

  if (is.null(diff_col_header)) {
    diff_col_header <- paste0("Risk Difference (%) <br> vs. ", reference_name)
  }

  if (is.null(diff_fig_header)) {
    diff_fig_header <- paste0("Risk Difference (%) + 95% CI <br> vs. ", reference_name)
  }

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

  # Aggregate the finite values of x (vector/matrix/data.frame) with f; when
  # none are finite, return `empty` rather than f()'s warning + -Inf/Inf on an
  # all-NA input (e.g. a term with no events, or an uncomputable risk diff).
  agg_finite <- function(x, f, empty) {
    x <- unlist(x, use.names = FALSE)
    x <- x[is.finite(x)]
    if (length(x)) f(x) else empty
  }

  # Per-arm proportions and counts carried into the figures (n_group arms, i.e.
  # excluding any Total column that m_group would add).
  prop_grp <- outdata$prop[, 1:n_group]
  n_grp <- outdata$n[, 1:n_group]

  # Define table data
  tbl <- data.frame(
    parameter = outdata$parameter_order,
    name = outdata$name,
    soc_name = outdata$soc_name,
    prop_fig = NA,
    diff_fig = NA,
    outdata$n[, 1:m_group],
    round(outdata$prop[, 1:m_group], digits = digits),
    round(outdata$diff, digits = digits),
    round(outdata$ci_lower, digits = digits),
    round(outdata$ci_upper, digits = digits),
    hide_prop = round(apply(prop_grp, 1, agg_finite, max, 0), digits + 2),
    hide_n = apply(n_grp, 1, agg_finite, max, 0)
  )

  rownames(tbl) <- NULL

  # Shared x-axis ranges for the inline figures ----
  # Computed once across every row so the proportion dot plot and the risk
  # difference error-bar cells are comparable from row to row. `ae_forestly()`
  # feeds these to `lt_dotplot(limits=)` / `lt_errorbar(limits=)`.
  tbl_prop <- prop_grp
  if (is.null(prop_range)) {
    fig_prop_range <- round(agg_finite(tbl_prop, range, c(0, 0)) + c(-2, 2))
  } else {
    rng <- agg_finite(tbl_prop, range, c(0, 0))
    if (prop_range[1] > rng[1] | prop_range[2] < rng[2]) {
      warning("There are data points outside the specified range for proportion.")
    }
    fig_prop_range <- prop_range
  }
  fig_prop_color <- color[1:n_group]

  tbl_diff <- data.frame(outdata$diff, outdata$ci_lower, outdata$ci_upper)
  if (is.null(diff_range)) {
    fig_diff_range <- round(agg_finite(tbl_diff, range, c(0, 0)) + c(-2, 2))
  } else {
    rng <- agg_finite(tbl_diff, range, c(0, 0))
    if (diff_range[1] > rng[1] | diff_range[2] < rng[2]) {
      warning("There are data points outside the specified range for difference.")
    }
    fig_diff_range <- diff_range
  }
  fig_diff_color <- fig_prop_color[index_diff]

  # Create outdata ----
  # `tbl` is the single source of truth for the interactive table; everything
  # else here is metadata `ae_forestly()` needs to turn `tbl` into an `lt`
  # table (ranges, colors, headers, per-arm column groupings, widths). No
  # reactable/plotly column specs are produced any more.
  outdata$tbl <- tbl
  outdata$display <- display
  outdata$digits <- digits
  outdata$fig_prop_range <- fig_prop_range
  outdata$fig_diff_range <- fig_diff_range
  outdata$fig_prop_color <- fig_prop_color
  outdata$fig_diff_color <- fig_diff_color
  outdata$diff_label <- diff_label
  outdata$ae_col_header <- ae_col_header
  outdata$diff_col_header <- diff_col_header
  outdata$diff_fig_header <- diff_fig_header
  outdata$n_group <- n_group
  outdata$m_group <- m_group
  outdata$name_n <- name_n
  outdata$name_prop <- name_prop
  outdata$diff_name <- names(outdata$diff)
  outdata$ci_lower_name <- names(outdata$ci_lower)
  outdata$ci_upper_name <- names(outdata$ci_upper)
  outdata$index_diff <- index_diff
  outdata$widths <- list(
    term = width_term, fig = width_fig, n = width_n,
    prop = width_prop, diff = width_diff, footer = footer_space
  )

  outdata
}
