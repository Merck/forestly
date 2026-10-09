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
#' @param max_page A numeric value of max page number shown in the table.
#' @param download_button A logical value to display download button.
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
                        max_page = NULL,
                        download_button = FALSE) {
  # Set filter parameter
  if (!is.null(filter)) {
    display_filter = TRUE
    filter <- match.arg(filter, c("prop", "n"))
  } else {
    display_filter = FALSE
  }

  # Handle filter_range parameter. The slider's bounds and step are left to lt,
  # which derives nice, data-bracketing values from the bound column itself (see
  # the `filter` argument below). A user-supplied filter_range overrides min/max.
  if (display_filter && !is.null(filter_range)) {
    if (length(filter_range) == 1) {
      # a single value is the max, with min = 0
      filter_range <- c(0, filter_range[1])
    } else if (length(filter_range) != 2) {
      stop("filter_range must be NULL, a single numeric value, or a numeric vector of length 2")
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

  # Shrink every embedded JSON payload (the listing store below and the main
  # table serialized by lt's `format()`): all go through xfun::tojson(), which
  # reads these options -- dictionary-encode repetitive columns, drop whitespace.
  # Scoped so the user's session options are left untouched.
  old_opt <- options(xfun.tojson.dict = 0.5, xfun.tojson.pretty = FALSE)
  on.exit(options(old_opt), add = TRUE)

  # ---- Drill-down detail (native lt row detail) ----
  # The listing is embedded once and shared by every row; lt's detail callback
  # assembles a row's listing on expand by looking it up in a client-side map
  # (see below), so records need no particular order and carry no per-row index
  # (see #147/#158). The listing is therefore used as-is -- no sort.
  ae_listing <- outdata$ae_listing
  ae_listing_param <- ae_listing$param
  listing_label <- get_label(ae_listing)

  detail_cols <- names(ae_listing)[!(names(ae_listing) %in% c("param", "SOC_Name"))]
  detail_labels <- unname(listing_label[match(detail_cols, names(listing_label))])
  detail_label_map <- stats::setNames(
    ifelse(is.na(detail_labels), detail_cols, detail_labels), detail_cols
  )
  # Numeric columns ship rounded (see `detail_records`), so lt needs no lt_format().
  detail_decimals <- 1L

  # Spec skeleton, built once from a zero-row slice: columns, labels and
  # interactive options shared by every row. Only `spec$data` differs per row.
  skeleton_df <- ae_listing[0, detail_cols, drop = FALSE]
  row.names(skeleton_df) <- NULL
  skeleton <- lt::lt(skeleton_df, auto_format = FALSE, auto_label = FALSE)
  skeleton <- lt::lt_label(skeleton, detail_label_map)
  detail_tpl <- lt::lt_spec(lt::lt_interactive(
    skeleton, sort = TRUE, search = FALSE, filter = TRUE, resize = TRUE
  ))

  # The listing is serialized once and shared by every row; a row embeds no record
  # pointers at all (a precomputed per-row index dwarfed the payload). Instead the
  # client builds a `parameter + upper(term-or-SOC) -> record indices` map once on
  # first expand and looks each row up by its own `parameter`/`name`. This drops
  # the whole index and no longer relies on records being contiguous in the store.
  #
  # Columns repeat values across rows, so xfun::tojson(dict=) dictionary-encodes
  # them (uniques once + a code per row) when shorter. Rounding numerics trims
  # unseen digits and boosts repeats.
  #
  # The per-parameter listings overlap heavily: an AE meeting several criteria
  # (e.g. "any", "serious" and "drug-related") is one physical record that the
  # stacked listing repeats once per matching parameter -- ~3x its distinct rows
  # on a typical plan. So each distinct record is stored once (`records`, plus its
  # match-key `soc`), and `members` lists, per parameter, the indices of the
  # records in it. The client keys each record by its event and SOC (`Adverse_Event`
  # is in `records`), so a term row and a SOC row both resolve. Deduping shrinks
  # the store ~55% -- both the dictionaries and the per-row code arrays scale with
  # the (now 3x smaller) row count.
  rec_key <- do.call(paste, c(ae_listing[c(detail_cols, "SOC_Name")], sep = "\r"))
  u_first <- !duplicated(rec_key)
  u_row <- which(u_first)
  # 0-based index of each listing row's distinct record (first-appearance order).
  ref <- as.integer(factor(rec_key, levels = rec_key[u_first])) - 1L

  detail_records <- lapply(ae_listing[detail_cols], function(x) {
    x <- x[u_row]
    if (is.numeric(x)) round(x, detail_decimals) else as.factor(x)
  })
  names(detail_records) <- detail_cols

  specs_json <- xfun::tojson(list(
    tpl = detail_tpl,
    records = detail_records,
    soc = as.factor(ae_listing$SOC_Name[u_row]),
    members = lapply(split(ref, ae_listing_param), as.integer)
  ))

  # Treatment groups the control-bar picker (below) offers, in display order:
  # the arms that actually appear in the listing, so aggregate forest columns
  # (e.g. "Total") that are never a per-subject `Treatment_Group` are left out.
  listing_groups <- intersect(outdata$group, ae_listing$Treatment_Group)
  group_json <- xfun::tojson(listing_groups)
  group_label_json <- '"Treatment group"'

  # lt calls the detail callback as (rawRow, index1Based, displayRow) and renders
  # the returned spec via LT.render (so the listing can itself be interactive).
  # The IIFE captures the store in a closure (one copy, no global). On first expand
  # it lazily builds a `parameter + "\r" + upper(term or SOC) -> record indices`
  # map (memoized), keying each record by both its event and its SOC so a term row
  # and a SOC row both resolve. Each expand then looks the row up by its own
  # `parameter`/`name`. Returning null leaves a row with no listing un-expandable.
  #
  # On mount it also drops a treatment-group picker into the table's control bar
  # (lt's reusable LT.ui popover + checklist, attached via el._lt.bar): checking
  # groups drives the `selected` set the callback filters each listing by, and
  # el._lt.resetDetail re-renders any open details through it. The onMount guard
  # keys off the callback's identity, so it wires only this table — not a detail
  # sub-table (no `detail`), nor another forestly table on the same page.
  detail_cb <- xfun::js(sprintf(
    "(() => {
  // The listing payload is dict-encoded JS (`[codes].map(i => [dict][i])`), not
  // plain JSON, so it can't ride as a deferred <script type=application/json>.
  // Instead build it lazily on first expand: V8 only pre-parses this closure
  // body at load, so the tens-of-MB of array literals compile and allocate off
  // the initial-paint path rather than when the spec's IIFE runs (#190).
  let store = null;
  const getStore = () => store || (store = %s);
  const groups = %s, selected = new Set(groups);
  let map = null;                       // (param + '\\r' + UPPER term/soc) -> [record idx]
  const lookup = () => {
    if (map) return map;
    const s = getStore();
    map = new Map();
    // `records`/`soc` hold one entry per distinct record; `members[param]` lists
    // the record indices in that parameter. Key each by its event and SOC so a
    // term row and a SOC row both resolve.
    const ev = s.records.Adverse_Event, so = s.soc, mem = s.members;
    const add = (k, r) => { const a = map.get(k); a ? a.push(r) : map.set(k, [r]); };
    for (const p of Object.keys(mem)) {
      const refs = mem[p];
      for (let j = 0; j < refs.length; j++) {
        const r = refs[j];
        add(p + '\\r' + ev[r].toUpperCase(), r);
        add(p + '\\r' + so[r].toUpperCase(), r);
      }
    }
    return map;
  };
  const build = (row) => {
    const key = row.parameter + '\\r' + String(row.name == null ? '' : row.name).toUpperCase();
    let abs = lookup().get(key);
    if (!abs || !abs.length) return null;
    const s = getStore();
    abs = [...new Set(abs)].sort((a, b) => a - b);   // dedupe: a term may equal its SOC
    const grp = s.records.Treatment_Group;
    if (grp) abs = abs.filter((i) => selected.has(grp[i]));
    return {
      ...s.tpl,
      data: Object.fromEntries(
        Object.entries(s.records).map(([k, col]) => [k, abs.map((i) => col[i])])
      )
    };
  };
  LT.onMount.push((tbl, spec) => {
    if (spec.interactive?.detail !== build || !tbl._lt || !tbl._lt.chips) return;
    const doc = tbl.ownerDocument, label = %s;
    // Build the chip first so the popover can host its click on the whole chip
    // (clicking the label text opens it, like lt's own typed-filter chips), then
    // drop the returned funnel wrap into the chip and add it to the bar's chips.
    const chip = LT.ui.chip(doc, label);
    const pop = LT.ui.popover(doc, label, (panel) => {
      const cl = LT.ui.checklist(doc, groups.map((g) => ({ value: g, label: g })),
        (vals) => {
          selected.clear();
          vals.forEach((v) => selected.add(v));
          tbl._lt.resetDetail();
        });
      panel.append(...cl.el);
    }, null, chip.el);
    chip.el.append(pop);
    tbl._lt.chips.append(chip.el);
  });
  return build;
})()", specs_json, group_json, group_label_json))

  # ---- Build the interactive lt table ----
  built <- format_lt_forestly(
    outdata,
    display_soc_toggle = display_soc_toggle,
    display_diff_toggle = display_diff_toggle
  )
  # forestly-ae on lt's container scopes cell styles and sizes the table (see
  # inst/css/forestly-widgets.css); per-column widths from format_lt_forestly() kept.
  built$x <- lt::lt_class(built$x, "forestly-ae")

  # The AE-criteria dropdown and the incidence slider are lt typed filters. They
  # bind to the hidden `parameter` and `hide_prop`/`hide_n` columns, so lt renders
  # each as a chip in the table's control bar; the visible term column keeps a
  # plain filter box. All read columns that travel with the table, so no external
  # bridge is needed. See the `filter` argument of lt::lt_interactive().
  param_levels <- levels(outdata$tbl$parameter)
  filter_cfg <- list(
    name = TRUE,
    parameter = list(
      type = "select", choices = param_levels, selected = param_levels[1],
      label = ae_label
    )
  )
  if (display_filter) {
    slider_col <- if (filter == "prop") "hide_prop" else "hide_n"
    range_cfg <- list(type = "range", label = filter_label)
    if (!is.null(filter_range)) {
      range_cfg$min <- filter_range[1]
      range_cfg$max <- filter_range[2]
    }
    filter_cfg[[slider_col]] <- range_cfg
  }

  x <- lt::lt_interactive(
    built$x,
    sort = TRUE,
    search = FALSE,
    filter = filter_cfg,
    pager = max_page,
    resize = TRUE,
    hide = built$hide_menu,
    detail = detail_cb,
    # lt's own CSV download (current view, displayed text) in its control bar;
    # forestly no longer carries a bespoke download button.
    download = if (download_button) "ae-forest.csv" else FALSE
  )

  # ---- Assemble: lt runtime (interactive + plot) + forestly styles ----
  htmltools::browsable(
    htmltools::tagList(
      lt::lt_dependency(interactive = TRUE, plot = TRUE),
      html_dependency_forestly_widgets(),
      html_dependency_ae_drilldown(),
      htmltools::HTML(format(x, assets = FALSE))
    )
  )
}
