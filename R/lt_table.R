# Copyright (c) 2026 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
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

#' Build the main AE forest `lt` table (structure only, no interactivity)
#'
#' Turns `format_ae_forestly()`'s `outdata$tbl` into an `lt` table: per-arm
#' n/(%) columns under treatment-group spanners, an inline proportion dot plot
#' (`lt_dotplot`, one staggered dot per arm), an inline risk-difference
#' error-bar (`lt_errorbar`, one point-and-CI per comparison), optional numeric
#' difference columns, and hidden helper columns (`parameter`, `hide_prop`,
#' `hide_n`, CI bounds) that stay in the spec so the client-side widgets and the
#' drill-down detail can read them.
#'
#' Interactivity (`lt_interactive()`, detail, widgets) is layered on by
#' [ae_forestly()]; this builder is deliberately interaction-free so it can be
#' tested in isolation.
#'
#' @param outdata An `outdata` object from [format_ae_forestly()].
#' @param display_soc_toggle Offer the SOC column via the column show/hide menu
#'   (starts hidden) instead of dropping it outright.
#' @param display_diff_toggle Offer the numeric risk-difference columns via the
#'   column show/hide menu.
#'
#' @return A list with `x` (an `lt_tbl`) and `hide_menu` (the argument to pass to
#'   `lt_interactive(hide=)`: `NULL`, `TRUE`, or a character vector of columns to
#'   start hidden).
#'
#' @noRd
format_lt_forestly <- function(outdata,
                               display_soc_toggle = TRUE,
                               display_diff_toggle = FALSE) {
  tbl <- outdata$tbl
  digits <- outdata$digits %||% 1
  m <- outdata$m_group # arms shown as n/(%) columns (incl. Total if displayed)
  ng <- outdata$n_group # arms carried into the proportion dot plot

  name_n <- outdata$name_n
  name_prop <- outdata$name_prop
  group <- outdata$group
  w <- outdata$widths

  diff_name <- outdata$diff_name
  lo_name <- outdata$ci_lower_name
  up_name <- outdata$ci_upper_name
  nd <- length(diff_name)
  diff_shown <- "diff" %in% outdata$display

  # Each figure draws into its own dedicated column via `lt_*(into=)`: lt ships
  # only the column name (an `add_col` op) and the runtime materializes the
  # column, so no duplicated value array travels in the JSON payload. Every
  # proportion / diff stays visible as text (`hide = FALSE` below) and is read by
  # reference, and the CI bounds come from the hidden `lo_name`/`up_name`.
  prop_cols <- paste0("prop_", seq_len(ng))
  pfig <- "prop_fig"                               # proportion dot-plot target
  dfig <- if (nd > 0) "diff_fig" else NULL         # risk-difference error-bar target

  # Column order of the real data: name, SOC, per-arm n/(%), numeric diffs,
  # hidden helpers last. The two figure columns are synthesized by the plots and
  # positioned after SOC by lt_move() below, so the figures lead the numeric
  # columns as in the forest-plot convention.
  arm_cols <- as.vector(rbind(name_n, name_prop)) # n_1, prop_1, n_2, prop_2, ...
  visible <- c("name", "soc_name", arm_cols, diff_name)
  hidden_tail <- c("parameter", "hide_prop", "hide_n", lo_name, up_name)
  tbl <- tbl[, c(visible, hidden_tail), drop = FALSE]

  # The parameter dropdown filters on the stored value, so ship a plain string
  # (not a factor code) the client can compare against the option text.
  tbl$parameter <- as.character(tbl$parameter)

  x <- lt::lt(tbl, auto_format = FALSE, auto_label = FALSE)

  # Column labels: arm n/(%), numeric diff header per comparison arm.
  labels <- stats::setNames(
    as.list(c(outdata$ae_col_header, "SOC Name")), c("name", "soc_name")
  )
  for (nm in name_n) labels[[nm]] <- "n"
  for (nm in name_prop) labels[[nm]] <- "(%)"
  for (k in seq_len(nd)) labels[[diff_name[k]]] <- group[outdata$index_diff[k]]
  x <- lt::lt_label(x, labels)

  # Numeric formatting: proportions wrapped as "(x.x)", diffs to `digits`.
  x <- lt::lt_format(x, name_prop, decimals = digits, prefix = "(", suffix = ")")
  if (nd > 0) x <- lt::lt_format(x, diff_name, decimals = digits)

  # Per-arm treatment-group spanners "<Group><br>(N=...)" over each n/(%) pair.
  for (i in seq_len(m)) {
    header <- sprintf(
      "<span title=\"%s (N=%s)\">%s<br>(N=%s)</span>",
      group[i], outdata$n_pop[i], group[i], outdata$n_pop[i]
    )
    x <- lt::lt_spanner(x, I(header), c(name_n[i], name_prop[i]))
  }
  if (diff_shown && nd > 0) {
    x <- lt::lt_spanner(x, I(outdata$diff_col_header), diff_name)
  }

  # Inline proportion dot plot: one dot per arm on a shared scale, footer legend
  # keyed by arm color. Dots are staggered onto separate tracks so near-equal
  # per-arm proportions stay distinguishable (as in the original forest plot).
  x <- lt::lt_dotplot(
    x, prop_cols,
    into = pfig,
    color = outdata$fig_prop_color,
    labels = group[seq_len(ng)],
    limits = outdata$fig_prop_range,
    width = w$fig,
    stagger = TRUE,
    axis = TRUE,
    hide = FALSE # the per-arm proportions stay visible as numeric columns
  )
  x <- lt::lt_label(x, stats::setNames(list("AE Proportion (%)"), pfig))

  # Inline risk-difference error-bar: one point + 95% CI per comparison, stacked,
  # shared scale, zero reference line, favor-direction axis label. Color + legend
  # only when 2+ arms stack (keys color -> arm); a single comparison draws
  # monochrome (no legend), since the "vs. <ref>" spanner already names it.
  eb_series <- lapply(seq_len(nd), function(k) {
    c(diff_name[k], lo_name[k], up_name[k]) # value, lower, upper
  })
  eb_color <- if (nd > 1) outdata$fig_diff_color else FALSE
  eb_labels <- if (nd > 1) group[outdata$index_diff] else NULL
  if (nd > 0) {
    x <- do.call(lt::lt_errorbar, c(
      list(x), eb_series,
      list(
        into = dfig,
        ref = 0,
        color = eb_color,
        labels = eb_labels,
        limits = outdata$fig_diff_range,
        width = w$fig,
        axis = outdata$diff_label,
        hide = FALSE # CI bounds stay hidden via fully_hidden; keep the diffs
      )
    ))
    x <- lt::lt_label(x, stats::setNames(list(I(outdata$diff_fig_header)), dfig))
  }

  # Place the synthesized figure columns right after SOC (before the numeric
  # columns); the plots appended them last via `into=`.
  x <- lt::lt_move(x, c(pfig, dfig), after = "soc_name")

  # Column widths (px). The synthesized figure columns carry the figure width.
  widths <- stats::setNames(
    as.list(c(
      paste0(w$term, "px"), paste0(w$term, "px"),
      rep(c(paste0(w$n, "px"), paste0(w$prop, "px")), m),
      paste0(w$fig, "px")
    )),
    c("name", "soc_name", arm_cols, pfig)
  )
  for (nm in diff_name) widths[[nm]] <- paste0(w$diff, "px")
  if (nd > 0) widths[[dfig]] <- paste0(w$fig, "px")
  x <- do.call(lt::lt_width, c(list(x), widths))

  # Decide show/hide handling for SOC and numeric diff columns.
  fully_hidden <- c("parameter", "hide_prop", "hide_n", lo_name, up_name)
  menu_hidden <- character(0)

  if (display_soc_toggle) {
    menu_hidden <- c(menu_hidden, "soc_name")
  } else {
    fully_hidden <- c(fully_hidden, "soc_name")
  }

  if (nd > 0 && !diff_shown) {
    if (display_diff_toggle) {
      menu_hidden <- c(menu_hidden, diff_name)
    } else {
      fully_hidden <- c(fully_hidden, diff_name)
    }
  }

  x <- lt::lt_hide(x, unique(fully_hidden))

  hide_menu <- if (display_soc_toggle || display_diff_toggle) {
    if (length(menu_hidden)) menu_hidden else TRUE
  } else {
    NULL
  }

  list(x = x, hide_menu = hide_menu)
}
