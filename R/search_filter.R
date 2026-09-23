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

#' Build a reactable search / filter function supporting negation and expressions
#'
#' Returns a `reactable::JS()` callback that can be used either as a per-column
#' `filterMethod` or as a table-wide `searchMethod`. In both cases it supports:
#'
#' * Substring matching, with a leading `!` to negate (e.g. `!Rash` keeps rows
#'   that do *not* contain "Rash").
#' * JavaScript expression evaluation when the term references the cell value
#'   `x` (e.g. `x > 5`, `x !== "Rash"`, `!x.includes("Rash")`). The expression
#'   is tested against both the string and, when applicable, the numeric form of
#'   each cell.
#'
#' A row is kept when *any* searched cell satisfies a positive test, and only
#' when *every* searched cell satisfies a negation test (so rows containing the
#' excluded value are dropped). For a single-column filter these two rules
#' collapse to the same thing.
#'
#' @param scope Either `"column"` for a per-column `filterMethod`
#'   (signature `function(rows, columnId, filterValue)`) or `"table"` for a
#'   table-wide `searchMethod` (signature
#'   `function(rows, columnIds, filterValue)`).
#'
#' @return A `reactable::JS()` object referencing the shared implementation.
#'
#' @details The implementation lives in `inst/js/search-filter.js` and is
#'   attached once per table via [html_dependency_search_filter()]. This
#'   function returns only a short reference to that global (e.g.
#'   `window.__forestly_filter_column`). The drill-down listing embeds one
#'   nested `reactable` per row of the main table, so inlining the full
#'   ~1.8 KB function body into every column of every nested table previously
#'   duplicated it hundreds of thousands of times and inflated the
#'   self-contained widget past a gigabyte; referencing a global keeps each
#'   `filterMethod` / `searchMethod` to a few dozen bytes.
#'
#' @noRd
search_filter_js <- function(scope = c("column", "table")) {
  scope <- match.arg(scope)

  ref <- if (scope == "column") {
    "window.__forestly_filter_column"
  } else {
    "window.__forestly_filter_table"
  }

  reactable::JS(ref)
}
