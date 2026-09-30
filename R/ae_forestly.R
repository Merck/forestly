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

#' Display interactive forest plot
#'
#' @param outdata An `outdata` object created by [format_ae_forestly()].
#' @param display_soc_toggle A boolean value to display SOC toggle button.
#' @param display_diff_toggle A boolean value to display risk difference toggle button.
#' @param filter A character value of the filter variable. If NULL, the slider bar will not be displayed.
#' @param filter_label A character value of the label for slider bar.
#' @param filter_range A numeric vector of length 2 for the range of the slider bar.
#' @param ae_label A character value of the label for criteria.
#'   If NULL (default), the range is automatically calculated from the data.
#'   If only one value is provided, it will be used as the maximum and minimum will be 0.
#' @param width A numeric value of width of the table in pixels.
#' @param max_page A numeric value of max page number shown in the table.
#' @param dowload_button A logical value to display download button.
#'
#' @section Searching and filtering:
#' The interactive table has a search box for each column (and, in the
#' expandable detail listing, a table-wide search box). In addition to a
#' plain substring match, the search terms understand two extra styles.
#'
#' **Negation with `!`.** Prefix a term with an exclamation mark to *exclude*
#' matching rows instead of keeping them:
#'
#' \itemize{
#'   \item `Rash` --- keep rows whose value contains "Rash".
#'   \item `!Rash` --- keep rows whose value does *not* contain "Rash".
#' }
#'
#' **Expressions.** If the term mentions the letter `x` (which stands for the
#' value of the cell being searched), it is evaluated as a small expression and
#' the row is kept when the expression is true. `x` behaves like the value in
#' that column, so numeric columns can be compared with numbers and text
#' columns with quoted text. Common patterns:
#'
#' \tabular{ll}{
#'   \strong{Type this in the search box} \tab \strong{Keeps rows where} \cr
#'   `x > 5`                       \tab the value is greater than 5 \cr
#'   `x >= 5`                      \tab the value is 5 or more \cr
#'   `x < 65`                      \tab the value is less than 65 \cr
#'   `x >= 18 && x <= 65`          \tab the value is between 18 and 65 (inclusive) \cr
#'   `x == 0`                      \tab the value equals 0 \cr
#'   `x === "Rash"`                \tab the value is exactly "Rash" \cr
#'   `x !== "Rash"`                \tab the value is anything except exactly "Rash" \cr
#'   `x.includes("itch")`          \tab the text contains "itch" \cr
#'   `!x.includes("itch")`         \tab the text does not contain "itch" \cr
#'   `x.startsWith("Application")` \tab the text starts with "Application" \cr
#'   `x.endsWith("itis")`          \tab the text ends with "itis" \cr
#'   `x === "M" || x === "F"`      \tab the value is either "M" or "F" \cr
#' }
#'
#' Notes for the expression style:
#' \itemize{
#'   \item Wrap text values in quotes (`"Rash"`); numbers need no quotes (`5`).
#'   \item Use `&&` for "and", `||` for "or", and `!` in front of a condition
#'     for "not".
#'   \item Comparisons use doubled symbols: `===` (equal), `!==` (not equal),
#'     together with `>`, `>=`, `<`, `<=`.
#'   \item Matching is case-sensitive in the expression style, so `x === "m"`
#'     will not match "M". Use the plain substring style (which ignores case)
#'     when case does not matter.
#'   \item A term that mentions `x` but is not a valid expression is treated as
#'     an ordinary substring search, so everyday searches keep working.
#' }
#'
#' @return An AE forest plot saved as a `shiny.tag.list` object.
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
#' if (interactive()) {
#'   meta |>
#'     prepare_ae_forestly(parameter = "any") |>
#'     format_ae_forestly() |>
#'     ae_forestly()
#' }
ae_forestly <- function(outdata,
                        display_soc_toggle = TRUE,
                        display_diff_toggle = FALSE,
                        filter = c("prop", "n"),
                        filter_label = NULL,
                        filter_range = NULL,
                        ae_label = NULL,
                        width = 1400,
                        max_page = NULL,
                        dowload_button = FALSE) {
  # Set filter parameter
  if (!is.null(filter)) {
    display_filter = TRUE
    filter <- match.arg(filter, c("prop", "n"))
  } else {
    display_filter = FALSE
  }

  # Handle filter_range parameter
  if (display_filter) {
    if (!is.null(filter_range)) {
      # User provided filter_range
      if (length(filter_range) == 1) {
        # If only one value provided, use it as max with min=0
        filter_range <- c(0, filter_range[1])
      } else if (length(filter_range) == 2) {
        # Use as provided
        filter_range <- filter_range
      } else {
        stop("filter_range must be NULL, a single numeric value, or a numeric vector of length 2")
      }
    } else {
      # Auto-detect range from data
      if (filter == "prop") {
        # For proportion, get max from hide_prop column
        max_val <- max(outdata$tbl$hide_prop, na.rm = TRUE)
        # Round up to nearest 10 for better UX
        max_val <- ceiling(max_val / 10) * 10
        # Ensure at least 100 for proportion
        filter_range <- c(0, max(100, max_val))
      } else if (filter == "n") {
        # For count, get max from hide_n column
        max_val <- max(outdata$tbl$hide_n, na.rm = TRUE)
        # Round up to nearest 10 (or 5 if max is small)
        if (max_val <= 20) {
          max_val <- ceiling(max_val / 5) * 5
        } else {
          max_val <- ceiling(max_val / 10) * 10
        }
        filter_range <- c(0, max_val)
      }
    }
  }

  if (is.null(filter_label)) {
    filter_label <- ifelse(filter == "prop",
                           "Incidence (%) in One or More Treatment Groups",
                           "Number of AE in One or More Treatment Groups"
    )
  }

  # `max_page` controls the maximum page number displayed in the interactive forest table.
  # By default (`NULL`), it will display the counts that round up to the nearest hundred.
  if (is.null(max_page)) {
    max_page <- if (max(attr(outdata$tbl$name, "n")) <= 100) c(10, 25, 50, 100) else c(10, 25, 50, 100, ceiling(max(attr(outdata$tbl$name, "n")) / 100) * 100)
  } else {
    max_page <- if (max_page <= 100) c(10, 25, 50, 100) else c(10, 25, 50, 100, max_page)
  }

  parameters <- unlist(strsplit(outdata$parameter, ";"))
  par_label <- vapply(parameters,
                      function(x) metalite::collect_adam_mapping(outdata$meta, x)$label,
                      FUN.VALUE = character(1)
  )

  for (par in parameters[(!(parameters %in% unique(outdata$parameter_order)))]) {
    outdata$tbl <-
      rbind(outdata$tbl, NA)
    outdata$tbl$name <- ifelse(is.na(outdata$tbl$name), "No data to display", outdata$tbl$name)
    outdata$tbl$parameter <-
      factor(
        ifelse(is.na(outdata$tbl$parameter), par, as.character(outdata$tbl$parameter)),
        levels(outdata$parameter_order)
      )
  }

  outdata$tbl$parameter <- factor(
    outdata$tbl$parameter,
    levels = parameters,
    labels = par_label
  )

  outdata$ae_listing$param <- factor(
    outdata$ae_listing$param,
    levels = parameters,
    labels = par_label
  )

  tbl <- crosstalk::SharedData$new(outdata$tbl)
  # Set default to be the first item
  default_param <- as.character(unique(outdata$tbl$parameter)[1])

  random_id <- paste0("filter_ae_", basename(tempfile("")), "|", default_param)

  if (is.null(ae_label)) {
    ae_label <- "AE Criteria"
  }

  filter_ae <- crosstalk::filter_select(
    id = random_id,
    label = ae_label,
    sharedData = tbl,
    group = ~parameter,
    multiple = FALSE
  )

  # Make a select list
  # Make a slider bar of the incidence percentage
  if (display_filter) {
    if (filter == "prop") {
      filter_subject <- crosstalk::filter_slider(
        id = "filter_subject",
        label = filter_label,
        sharedData = tbl,
        column = ~hide_prop, # whose values will be used for this slider
        step = 1, # specifies interval between each select-able value on the slider
        width = 250, # width of the slider control
        min = filter_range[1], # the leftmost value of the slider
        max = filter_range[2] # the rightmost value of the slider
      )
    }

    if (filter == "n") {
      filter_subject <- crosstalk::filter_slider(
        id = "filter_subject",
        label = filter_label,
        sharedData = tbl,
        column = ~hide_n,
        step = 1,
        width = 250,
        min = filter_range[1], # the leftmost value of the slider
        max = filter_range[2] # the rightmost value of the slider
      )
    }

    # Set the slider attributes to match our filter_range
    filter_subject$children[[2]]$attribs$`data-from` <- filter_range[1]
    filter_subject$children[[2]]$attribs$`data-to` <- filter_range[2]
    filter_subject$children[[2]]$attribs$`data-max` <- filter_range[2]
  } else {
    filter_subject <- NULL
  }

  diff_cols <- c(
    names(outdata$diff)
  )

  all_diff_cols <- c(diff_cols, "diff_fig")
  displayed_diff_cols <- intersect(all_diff_cols, c(
    if ("diff" %in% outdata$display) diff_cols else NULL,
    if ("fig_diff" %in% outdata$display) "diff_fig" else NULL
  ))

  hidden_cols <- outdata$hidden_column
  if (display_diff_toggle) {
    hidden_cols <- setdiff(hidden_cols, displayed_diff_cols)
  }

  # Lazy client-side drill-down listings. Instead of emitting a full nested
  # reactable per row (which duplicates scaffolding and inflated the widget past
  # a gigabyte, see #147/#158), we render a lightweight `lt` interactive table
  # on demand when a row is expanded. The columns, labels, number formatting and
  # interactive options are identical for every row, so we build that `lt` spec
  # skeleton once and embed only a per-row data slice; the client merges the two
  # and renders with LT.render(). This avoids materializing ~one spec per row
  # (thousands of them) and keeps the payload to the listing data alone.
  ae_listing <- outdata$ae_listing
  ae_listing_event_upper <- toupper(ae_listing$Adverse_Event)
  ae_listing_soc_upper <- toupper(ae_listing$SOC_Name)
  ae_listing_param <- ae_listing$param

  detail_cols <- names(ae_listing)[!(names(ae_listing) %in% c("param", "SOC_Name"))]
  listing_label <- get_label(ae_listing)
  detail_labels <- unname(listing_label[match(detail_cols, names(listing_label))])
  detail_label_map <- stats::setNames(
    ifelse(is.na(detail_labels), detail_cols, detail_labels), detail_cols
  )
  # Only numeric columns need decimal formatting; keeping the raw numeric values
  # in the spec lets lt's search/filter evaluate `x`-expressions numerically.
  numeric_detail_cols <- detail_cols[
    vapply(ae_listing[detail_cols], is.numeric, logical(1))
  ]

  tbl_name <- outdata$tbl$name
  tbl_parameter <- outdata$tbl$parameter

  # Shared spec skeleton, built once from an empty (zero-row) slice: it carries
  # the columns, labels, formatting and interactive options that every row's
  # listing shares. Only `spec$data` differs per row, and `lt`'s `data` is just
  # the sliced data frame, so per-row work is a plain subset (no lt pipeline).
  skeleton_df <- ae_listing[0, detail_cols, drop = FALSE]
  row.names(skeleton_df) <- NULL
  x <- lt::lt(skeleton_df)
  x <- lt::lt_label(x, detail_label_map)
  if (length(numeric_detail_cols)) {
    x <- lt::lt_format(x, numeric_detail_cols, decimals = 1)
  }
  detail_tpl <- lt::lt_spec(lt::lt_interactive(
    x, sort = TRUE, search = FALSE, filter = TRUE, resize = TRUE
  ))

  # Per-row listing data. Rather than embed a data slice per row -- which copies
  # every listing record into each row it matches (its AE-term row *and* its SOC
  # row, so a record ships two or more times) and dominated the widget on large
  # trials -- we embed the listing columns *once* and, per row, only the 0-based
  # indices of the records it needs. The client gathers those indices out of the
  # shared record store to rebuild the slice. Matches are looked up from a
  # precomputed parameter+term -> row-index map, so building each index vector is
  # O(matches) instead of a full scan of the listing per row.
  row_idx <- seq_len(nrow(ae_listing))
  key_event <- paste(ae_listing_param, ae_listing_event_upper, sep = "\r")
  key_soc <- paste(ae_listing_param, ae_listing_soc_upper, sep = "\r")
  map_event <- split(row_idx, key_event)
  map_soc <- split(row_idx, key_soc)

  build_detail_index <- function(i) {
    key <- paste(tbl_parameter[i], toupper(tbl_name[i]), sep = "\r")
    idx <- c(map_event[[key]], map_soc[[key]])
    if (length(idx)) idx <- sort.int(unique(idx))
    idx - 1L # 0-based for the client-side gather
  }
  detail_index <- lapply(seq_along(tbl_name), build_detail_index)

  # The full listing serialized once (columnar: `{column: [values]}`), shared by
  # every expanded row. Numeric columns stay raw so lt's formatting/filtering can
  # act on the values; the skeleton spec carries the decimal formatting.
  detail_records <- ae_listing[detail_cols]
  row.names(detail_records) <- NULL

  # Embed the skeleton and per-row data once under a widget-unique global; the
  # `details` callback rebuilds a row's spec by merging the skeleton with its
  # data slice and renders it lazily on expand. Guard the payload against a
  # literal `</script>` closing the inline block early.
  specs_var <- paste0(
    "__forestly_ae_specs_",
    gsub("[^A-Za-z0-9]", "", basename(tempfile("")))
  )
  specs_json <- gsub(
    "</(script)", "<\\\\/\\1",
    xfun::tojson(list(
      tpl = detail_tpl,
      columns = detail_cols,
      records = detail_records,
      index = detail_index
    )),
    perl = TRUE, ignore.case = TRUE
  )
  specs_script <- htmltools::tags$script(htmltools::HTML(
    paste0("window.", specs_var, "=", specs_json, ";")
  ))

  # Client-side detail renderer. reactable renders a table-level `details`
  # string as a React text node (it is escaped, not parsed as HTML), so we
  # return a real element instead: an empty container whose `ref` callback fires
  # on mount, rebuilds the row's data slice by gathering the row's record
  # indices out of the shared columnar store, merges it into the shared skeleton
  # and renders the lt table with LT.render() (which triggers the interactive
  # plugin). `React` is a global from reactR's dependency. `rowInfo.index`
  # (0-based, stable across sort/filter) matches the `index` array; a length-1
  # index vector serializes as a bare number, so coerce to an array.
  detail_js <- reactable::JS(paste0(
    "function(rowInfo) {\n",
    "  var store = window.", specs_var, ";\n",
    "  if (!store || !store.index) return null;\n",
    "  var idx = store.index[rowInfo.index];\n",
    "  if (idx == null) return null;\n",
    "  if (!Array.isArray(idx)) idx = [idx];\n",
    "  var recs = store.records, cols = store.columns, data = {};\n",
    "  for (var c = 0; c < cols.length; c++) {\n",
    "    var name = cols[c], col = recs[name], out = new Array(idx.length);\n",
    "    for (var j = 0; j < idx.length; j++) out[j] = col[idx[j]];\n",
    "    data[name] = out;\n",
    "  }\n",
    "  var spec = Object.assign({}, store.tpl, {data: data});\n",
    "  return window.React.createElement('div', {\n",
    "    className: 'forestly-ae-drilldown',\n",
    "    ref: function(el) {\n",
    "      if (el && !el.dataset.ltDone && window.LT) {\n",
    "        el.dataset.ltDone = '1';\n",
    "        window.LT.render(el, spec);\n",
    "      }\n",
    "    }\n",
    "  });\n",
    "}"
  ))

  p_reactable <- reactable2(
    tbl,
    columns = outdata$reactable_columns,
    columnGroups = outdata$reactable_columns_group,
    hidden_item = paste0("'", hidden_cols, "'", collapse = ", "),
    soc_toggle = display_soc_toggle,
    diff_toggle = display_diff_toggle,
    diff_columns = displayed_diff_cols,
    width = width,
    download = dowload_button,
    searchable = FALSE,
    details = detail_js,
    pageSizeOptions = max_page,

    # Default sort variable
    defaultSorted = c("parameter", names(outdata$diff)),
    defaultSortOrder = "desc"
  )

  p <- suppressWarnings(
    crosstalk::bscols(
      # Width of the select list and reactable
      widths = c(3, 9, 12, 0),
      filter_ae,
      filter_subject,
      p_reactable
    )
  )

  # Assemble html file
  offline <- TRUE

  htmltools::browsable(
    htmltools::tagList(
      html_dependency_filter_crosstalk(),
      html_dependency_search_filter(),
      reactR::html_dependency_react(offline),
      html_dependency_plotly(offline),
      html_dependency_react_plotly(offline),
      lt::lt_dependency(interactive = TRUE),
      html_dependency_ae_drilldown(),
      specs_script,
      p
    )
  )
}
