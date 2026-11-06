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
#' @param display_soc_toggle Include the SOC column in the table, offered via the
#'   column show/hide menu (starts hidden). If `FALSE`, the column is left out of
#'   the table data altogether.
#'
#' @return A list with `x` (an `lt_tbl`) and `hide_menu` (the argument to pass to
#'   `lt_interactive(hide=)`: `TRUE`, or a character vector of columns to start
#'   hidden).
#'
#' @noRd
format_lt_forestly <- function(outdata,
                               display_soc_toggle = TRUE) {
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
  display <- outdata$display
  diff_shown <- "diff" %in% display

  # Each figure draws into its own dedicated column via `lt_*(into=)`: lt ships
  # only the column name (an `add_col` op) and the runtime materializes the
  # column, so no duplicated value array is saved in the HTML. Every
  # proportion / diff stays visible as text (`hide = FALSE` below) and is read by
  # reference, and the CI bounds come from the hidden `lo_name`/`up_name`.
  prop_cols <- paste0("prop_", seq_len(ng))
  # A figure is built only when listed in `display`; the CI bounds are carried
  # only for the error bar.
  pfig <- if ("fig_prop" %in% display) "prop_fig"                # dot-plot target
  dfig <- if (nd > 0 && "fig_diff" %in% display) "diff_fig"      # error-bar target
  ci_cols <- if (!is.null(dfig)) c(lo_name, up_name)

  # Column order of the real data: name, SOC, per-arm n/(%), numeric diffs,
  # hidden helpers last. The two figure columns are synthesized by the plots and
  # positioned after SOC by lt_move() below, so the figures lead the numeric
  # columns as in the forest-plot convention.
  #
  # `n` / `prop` shown only when listed in `display`. The per-arm proportions are
  # still carried, hidden, when the dot plot reads them; otherwise they, and any
  # unshown column, are left out of the data.
  show_n <- if ("n" %in% display) name_n
  show_prop <- if ("prop" %in% display) name_prop
  prop_hidden <- if (!is.null(pfig)) setdiff(prop_cols, show_prop)
  arm_cols <- intersect( # n_1, prop_1, n_2, prop_2, ...
    as.vector(rbind(name_n, name_prop)), c(show_n, show_prop, prop_hidden)
  )
  shown_arm_cols <- setdiff(arm_cols, prop_hidden)
  visible <- c("name", if (display_soc_toggle) "soc_name", arm_cols, diff_name)
  hidden_tail <- c("parameter", "hide_prop", "hide_n", ci_cols)
  tbl <- tbl[, c(visible, hidden_tail), drop = FALSE]

  # The parameter dropdown filters on the stored value, so ship a plain string
  # (not a factor code) the client can compare against the option text.
  tbl$parameter <- as.character(tbl$parameter)

  x <- lt::lt(tbl, auto_format = FALSE, auto_label = FALSE)

  # Column labels: arm n/(%), numeric diff header per comparison arm.
  labels <- list(name = outdata$ae_col_header)
  if (display_soc_toggle) labels$soc_name <- "SOC"
  for (nm in show_n) labels[[nm]] <- "n"
  for (nm in show_prop) labels[[nm]] <- "(%)"
  for (k in seq_len(nd)) labels[[diff_name[k]]] <- group[outdata$index_diff[k]]
  x <- lt::lt_label(x, labels)

  # Numeric formatting: proportions wrapped as "(x.x)", diffs to `digits`.
  if (length(show_prop)) {
    x <- lt::lt_format(x, show_prop, decimals = digits, prefix = "(", suffix = ")")
  }
  if (nd > 0) x <- lt::lt_format(x, diff_name, decimals = digits)

  # Per-arm treatment-group spanners "<Group><br>(N=...)" over each n/(%) pair.
  for (i in seq_len(m)) {
    header <- sprintf(
      "<span title=\"%s (N=%s)\">%s<br>(N=%s)</span>",
      group[i], outdata$n_pop[i], group[i], outdata$n_pop[i]
    )
    arm_i <- intersect(c(name_n[i], name_prop[i]), shown_arm_cols)
    if (length(arm_i)) x <- lt::lt_spanner(x, I(header), arm_i)
  }
  if (diff_shown && nd > 0) {
    x <- lt::lt_spanner(x, I(outdata$diff_col_header), diff_name)
  }

  # Inline proportion dot plot: one dot per arm on a shared scale, footer legend
  # keyed by arm color. Dots are staggered onto separate tracks so near-equal
  # per-arm proportions stay distinguishable (as in the original forest plot).
  if (!is.null(pfig)) {
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
    x <- lt::lt_label(x, list(pfig, "AE Proportion (%)"))
  }

  # Inline risk-difference error-bar: one point + 95% CI per comparison, stacked,
  # shared scale, zero reference line, favor-direction axis label. Color + legend
  # only when 2+ arms stack (keys color -> arm); a single comparison draws
  # monochrome (no legend), since the "vs. <ref>" spanner already names it.
  eb_series <- lapply(seq_len(nd), function(k) {
    c(diff_name[k], lo_name[k], up_name[k]) # value, lower, upper
  })
  eb_color <- if (nd > 1) outdata$fig_diff_color else FALSE
  eb_labels <- if (nd > 1) group[outdata$index_diff] else NULL
  if (!is.null(dfig)) {
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
    x <- lt::lt_label(x, list(dfig, I(outdata$diff_fig_header)))
  }

  # Place the synthesized figure columns right after SOC, or the term column when
  # SOC is excluded (before the numeric columns); the plots appended them last
  # via `into=`.
  figs <- c(pfig, dfig)
  if (length(figs)) x <- lt::lt_move(x, figs, after = if (display_soc_toggle) "soc_name" else "name")

  # Column widths (px). The synthesized figure columns carry the figure width.
  width_cols <- c("name", if (display_soc_toggle) "soc_name", arm_cols, pfig, diff_name, dfig)
  width_px <- c(
    rep(w$term, 1 + display_soc_toggle), ifelse(arm_cols %in% name_n, w$n, w$prop),
    rep(w$fig, length(pfig)), rep(w$diff, nd), rep(w$fig, length(dfig))
  )
  x <- lt::lt_width(x, list(width_cols, paste0(width_px, "px")))

  # Numeric diff columns not shown at start are offered in the show/hide menu
  # (SOC is either in the menu or excluded from the data above).
  fully_hidden <- c("parameter", "hide_prop", "hide_n", ci_cols, prop_hidden)
  menu_hidden <- character(0)

  if (display_soc_toggle) menu_hidden <- "soc_name"

  if (nd > 0 && !diff_shown) menu_hidden <- c(menu_hidden, diff_name)

  x <- lt::lt_hide(x, unique(fully_hidden))

  hide_menu <- if (length(menu_hidden)) menu_hidden else TRUE

  # Numeric columns shown at start (n, (%), difference) for ae_forestly() sliders.
  range_cols <- setdiff(c(shown_arm_cols, diff_name), unique(c(fully_hidden, menu_hidden)))

  list(x = x, hide_menu = hide_menu, range_cols = range_cols)
}
