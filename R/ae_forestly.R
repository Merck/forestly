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

  if (is.null(ae_label)) {
    ae_label <- "AE Criteria"
  }

  # ---- Drill-down detail (native lt row detail) ----
  # The listing is embedded once; each table row carries only the 0-based record
  # indices it needs (contiguous runs or delta-encoded), and lt's detail callback
  # assembles that row's listing on expand. This keeps the widget small -- the
  # listing is never duplicated per row (see #147/#158).
  ae_listing <- outdata$ae_listing
  ae_listing_event_upper <- toupper(ae_listing$Adverse_Event)
  ae_listing_soc_upper <- toupper(ae_listing$SOC_Name)
  ae_listing_param <- ae_listing$param

  # Group each drill-down's records together (param, then SOC, then term) so its
  # index is a contiguous block the run-length encoding below can collapse.
  ord <- order(ae_listing_param, ae_listing_soc_upper, ae_listing_event_upper)
  ae_listing <- ae_listing[ord, , drop = FALSE]
  ae_listing_event_upper <- ae_listing_event_upper[ord]
  ae_listing_soc_upper <- ae_listing_soc_upper[ord]
  ae_listing_param <- ae_listing_param[ord]

  detail_cols <- names(ae_listing)[!(names(ae_listing) %in% c("param", "SOC_Name"))]
  listing_label <- get_label(ae_listing)
  detail_labels <- unname(listing_label[match(detail_cols, names(listing_label))])
  detail_label_map <- stats::setNames(
    ifelse(is.na(detail_labels), detail_cols, detail_labels), detail_cols
  )
  # Numeric columns ship rounded (see `detail_records`), so lt needs no lt_format().
  detail_decimals <- 1L

  tbl_name <- outdata$tbl$name
  tbl_parameter <- outdata$tbl$parameter

  # Spec skeleton, built once from a zero-row slice: columns, labels and
  # interactive options shared by every row. Only `spec$data` differs per row.
  skeleton_df <- ae_listing[0, detail_cols, drop = FALSE]
  row.names(skeleton_df) <- NULL
  skeleton <- lt::lt(skeleton_df, auto_format = FALSE, auto_label = FALSE)
  skeleton <- lt::lt_label(skeleton, detail_label_map)
  detail_tpl <- lt::lt_spec(lt::lt_interactive(
    skeleton, sort = TRUE, search = FALSE, filter = TRUE, resize = TRUE
  ))

  # Per row, embed only the 0-based record indices it needs (not a data slice,
  # which would ship each record once per matching row); the client gathers them
  # from the shared store. Precomputed param+term/param+SOC -> row-index maps make
  # each lookup O(matches) instead of a full scan.
  row_idx <- seq_len(nrow(ae_listing))
  key_event <- paste(ae_listing_param, ae_listing_event_upper, sep = "\r")
  key_soc <- paste(ae_listing_param, ae_listing_soc_upper, sep = "\r")
  map_event <- split(row_idx, key_event)
  map_soc <- split(row_idx, key_soc)

  tbl_keys <- paste(tbl_parameter, toupper(tbl_name), sep = "\r")
  # `index` dominates the payload, so encode it tightly (one compact `js()` blob):
  #   * contiguous run (common after the sort) -> `[start, -count]`; the negative
  #     second element is the run marker, since deltas are always positive.
  #   * otherwise -> delta-encode the sorted 0-based array (`[first, gap, ...]`).
  # Dict-encoding can't help (indices are distinct per row). Client reverses both.
  index_arrays <- vapply(tbl_keys, function(key) {
    idx <- c(map_event[[key]], map_soc[[key]])
    if (!length(idx)) return("[]")
    idx <- sort.int(unique(idx)) - 1L
    n <- length(idx)
    if (n >= 2L && idx[n] - idx[1] + 1L == n) {
      paste0("[", idx[1], ",", -n, "]")
    } else {
      paste0("[", paste0(c(idx[1], diff(idx)), collapse = ","), "]")
    }
  }, character(1))
  detail_index <- xfun::js(paste0("[", paste0(index_arrays, collapse = ","), "]"))

  # The listing serialized once, shared by every row. Columns repeat values across
  # rows, so xfun::tojson(dict=) dictionary-encodes them (uniques once + a code per
  # row) when shorter. Rounding numerics trims unseen digits and boosts repeats.
  detail_records <- lapply(ae_listing[detail_cols], function(x) {
    if (is.numeric(x)) round(x, detail_decimals) else as.factor(x)
  })
  names(detail_records) <- detail_cols

  # dict < 1 gates near-unique columns out at the cheap unique() check instead of
  # serializing them twice (codes + plain) only to discard the encoding.
  specs_json <- xfun::tojson(list(
    tpl = detail_tpl,
    records = detail_records,
    index = detail_index
  ), dict = 0.5, pretty = FALSE)

  # lt calls the detail callback as (rawRow, index1Based, displayRow) and renders
  # the returned spec via LT.render (so the listing can itself be interactive).
  # The IIFE captures the store in a closure (one copy, no global). A 2-element
  # index entry with a negative second value is a contiguous run [start, -count];
  # otherwise it is delta-encoded ([first, gap, ...]) and recovered with a running
  # sum. Returning null leaves a row with no listing un-expandable.
  detail_cb <- xfun::js(sprintf(
    "(() => {
  const store = %s;
  return (row, index) => {
    const enc = store.index[index - 1];
    if (!enc || !enc.length) return null;
    let abs;
    if (enc.length === 2 && enc[1] < 0) {
      const start = enc[0], count = -enc[1];
      abs = Array.from({ length: count }, (_, k) => start + k);
    } else {
      let acc = 0;
      abs = enc.map((d) => (acc += d));
    }
    return {
      ...store.tpl,
      data: Object.fromEntries(
        Object.entries(store.records).map(([k, col]) => [k, abs.map((i) => col[i])])
      )
    };
  };
})()", specs_json))

  # ---- Build the interactive lt table ----
  built <- format_lt_forestly(
    outdata,
    display_soc_toggle = display_soc_toggle,
    display_diff_toggle = display_diff_toggle
  )
  x <- lt::lt_interactive(
    built$x,
    sort = TRUE,
    search = FALSE,
    filter = TRUE,
    pager = max_page,
    resize = TRUE,
    hide = built$hide_menu,
    detail = detail_cb
  )

  # ---- forestly-owned controls (drive the table via el._lt.filter) ----
  param_levels <- levels(outdata$tbl$parameter)
  param_select <- htmltools::tags$label(
    class = "forestly-param", ae_label,
    htmltools::tags$select(
      lapply(param_levels, function(p) htmltools::tags$option(p))
    )
  )

  if (display_filter) {
    slider_col <- if (filter == "prop") "hide_prop" else "hide_n"
    lo <- filter_range[1]
    hi <- filter_range[2]
    slider <- htmltools::div(
      class = "forestly-slider", `data-col` = slider_col,
      htmltools::tags$label(filter_label),
      htmltools::div(
        class = "forestly-slider-track",
        htmltools::tags$input(
          type = "range", class = "lo",
          min = lo, max = hi, value = lo, step = 1
        ),
        htmltools::tags$input(
          type = "range", class = "hi",
          min = lo, max = hi, value = hi, step = 1
        )
      ),
      htmltools::div(
        class = "forestly-slider-out",
        htmltools::tags$span(class = "lo-out", lo),
        htmltools::HTML("&ndash;"),
        htmltools::tags$span(class = "hi-out", hi)
      )
    )
  } else {
    slider <- NULL
  }

  download_btn <- if (dowload_button) {
    htmltools::tags$button(class = "forestly-download", "Download as CSV")
  } else {
    NULL
  }

  controls <- htmltools::div(
    class = "forestly-controls",
    param_select, slider, download_btn
  )

  container <- htmltools::div(
    class = "forestly-ae",
    style = htmltools::css(width = paste0(width, "px"), `max-width` = "100%"),
    controls,
    htmltools::div(
      class = "forestly-table",
      style = "overflow-x: auto;",
      htmltools::HTML(format(x, assets = FALSE))
    )
  )

  # ---- Assemble: lt runtime (interactive + plot) + forestly widgets ----
  htmltools::browsable(
    htmltools::tagList(
      lt::lt_dependency(interactive = TRUE, plot = TRUE),
      html_dependency_forestly_widgets(),
      html_dependency_ae_drilldown(),
      container
    )
  )
}
