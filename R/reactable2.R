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

#' Update default value for reactable
#'
#' This function is used to create a data table from tabular data with sorting
#' and pagination by default.
#' The reactable configuration follows
#' <https://glin.github.io/reactable/reference/reactable.html>.
#' The data table is an HTML widget that can be used in R Markdown documents
#' and Shiny applications, or viewed from an R console.
#'
#' @param data A data frame or matrix to obtain variables.
#' @param resizable A character string to define the criteria to
#'   enable columns resizing.
#' @param filterable A character string to define the criteria to
#'   enable columns filtering.
#' @param searchable A character string to define the criteria to
#'   enable global table searching.
#' @param defaultPageSize A character string to define the Page size options
#'   for the table.
#' @param borderless  A character string to remove inner borders from table.
#' @param striped A character string to add zebra-striping to table rows.
#' @param highlight A character string to highlight table rows on hover.
#' @param fullWidth Stretch the table to fill the full width of its container?
#'   Defaults to `TRUE`.
#' @param width Width of the table in pixels. Defaults to `"auto"` for
#'   automatic sizing.
#' @param theme Theme options for the table.
#'   The default value is `reactableTheme(cellPadding = "0px 8px")`:
#'   no padding between two cells (ensure no break line between reference line).
#' @param col_def An alternative argument for `defaultColDef`.
#' @param label A logical value to display label as a hover text.
#' @param download A logical value to display download button.
#' @param soc_toggle A logical value to display SOC toggle button.
#' @param diff_toggle A logical value to display risk difference toggle button.
#' @param hidden_item Vector for hidden columns.
#' @param diff_columns Character vector of risk difference column names.
#' @param ... Additional arguments passed to [reactable::reactable()].
#' @inheritParams reactable::reactable
#'
#' @return An interactive data table.
#'
#' @noRd
#'
#' @examples
#' reactable2(iris)
reactable2 <- function(
    data,
    resizable = TRUE,
    filterable = TRUE,
    searchable = TRUE,
    defaultPageSize = 10,
    showPageSizeOptions = TRUE,
    borderless = TRUE,
    striped = TRUE,
    highlight = TRUE,
    fullWidth = TRUE,
    width = 1200,
    theme = reactable::reactableTheme(cellPadding = "0px 8px"),
    label = TRUE,
    wrap = FALSE,
    download = TRUE,
    col_def = NULL,
    soc_toggle = TRUE,
    diff_toggle = FALSE,
    hidden_item = NULL,
    diff_columns = NULL,
    ...) {
  # Display variable label as hover text
  if (label & is.null(col_def)) {
    label <- get_label(data)

    col_header <- function(value, name) {
      htmltools::div(title = as.character(label[value]), value)
    }

    col_def <- reactable::colDef(header = col_header)
  }

  element_id <- basename(tempfile())

  tbl <- reactable::reactable(
    data = data,
    resizable = resizable,
    filterable = filterable,
    searchable = searchable,
    defaultPageSize = defaultPageSize,
    # defaultColDef = col_def, # This breaks rendering under reactR >= 0.6.0
    showPageSizeOptions = showPageSizeOptions,
    borderless = borderless,
    striped = striped,
    highlight = highlight,
    fullWidth = fullWidth,
    width = width,
    theme = theme,
    wrap = wrap,
    elementId = element_id,
    ...
  )

  buttons <- list()

  if (soc_toggle) {
    on_click_soc <- paste0(
      "function control_soc(hidden_columns) {",
      "  if (hidden_columns.includes('soc_name')) {",
      "    Reactable.setHiddenColumns('", element_id, "', prevColumns => {
                             return prevColumns.filter(col => col !== 'soc_name')})",
      "  } else {",
      "    Reactable.setHiddenColumns('", element_id, "', prevColumns => {
                             return [...prevColumns, 'soc_name']})",
      "  }",
      "}",
      "control_soc(Reactable.getState('", element_id, "').hiddenColumns);"
    )

    buttons <- c(buttons, list(
      htmltools::tags$button(
        "Show/Hide SOC column",
        onclick = on_click_soc
      )
    ))
  }

  if (diff_toggle && !is.null(diff_columns) && length(diff_columns) > 0) {
    diff_cols_js <- paste0("['", paste(diff_columns, collapse = "', '"), "']")
    on_click_diff <- paste0(
      "function control_diff(hidden_columns) {",
      "  const diffCols = ", diff_cols_js, ";",
      "  const allDiffHidden = diffCols.every(col => hidden_columns.includes(col));",
      "  if (allDiffHidden) {",
      "    Reactable.setHiddenColumns('", element_id, "', prevColumns => {
                             return prevColumns.filter(col => !diffCols.includes(col))})",
      "  } else {",
      "    Reactable.setHiddenColumns('", element_id, "', prevColumns => {
                             return [...new Set([...prevColumns, ...diffCols])]})",
      "  }",
      "}",
      "control_diff(Reactable.getState('", element_id, "').hiddenColumns);"
    )

    buttons <- c(buttons, list(
      htmltools::tags$button(
        "Show/Hide Risk Difference Value",
        onclick = on_click_diff
      )
    ))
  }



  if (length(buttons) > 0) {
    tbl <- htmltools::tagList(
      buttons,
      tbl
    )
  }

  if (download) {
    on_click <- paste0("Reactable.downloadDataCSV('", element_id, "')")

    htmltools::browsable(
      htmltools::tagList(
        htmltools::tags$button("Download as CSV", onclick = on_click),
        tbl
      )
    )
  } else {
    tbl
  }
}

get_label <- function(data) {
  label <- vapply(data, function(x) {
    if (is.null(attr(x, "label"))) {
      return(NA_character_)
    } else {
      attr(x, "label")
    }
  }, FUN.VALUE = character(1))

  ifelse(is.na(label), names(data), label)
}

assign_label <- function(data, var = names(data), label = names(data)) {
  # Input checking
  stopifnot(length(var) == length(label))
  stopifnot(!any(duplicated(var)))

  # Check missing label
  name <- names(data)
  diff <- setdiff(name, var)

  if (length(diff) > 0) {
    message(
      "missing variables set label as itself\n",
      paste(diff, collapse = "\n")
    )

    var <- c(var, diff)
    label <- c(label, diff)
  }

  # Assign label
  for (i in seq(name)) {
    attr(data[[i]], "label") <- label[names(data[i]) == var]
  }

  data
}
