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
#' @return A `reactable::JS()` object.
#'
#' @noRd
search_filter_js <- function(scope = c("column", "table")) {
  scope <- match.arg(scope)

  # The body is identical for both scopes once the set of searched column ids
  # is normalized to an array: a per-column filter searches `[columnId]`, a
  # table-wide search searches all `columnIds`. Iterating with some()/every()
  # over a one-element array reduces to testing that single cell.
  signature <- if (scope == "column") {
    "function(rows, columnId, filterValue)"
  } else {
    "function(rows, columnIds, filterValue)"
  }
  ids <- if (scope == "column") "[columnId]" else "columnIds"

  reactable::JS(sprintf(
    "%s {
      var v = filterValue.trim();
      if (v === '') return rows;
      var ids = %s;
      // JS expression mode: the term references the cell variable `x`
      // (e.g. `x > 5`, `x !== \"Rash\"`, `!x.includes(\"Rash\")`).
      if (/(^|[^\\w$])x([^\\w$]|$)/.test(v)) {
        var fn;
        try {
          fn = new Function('x', 'return (' + v + ');');
        } catch (e) {
          fn = null;
        }
        if (fn) {
          // Negation expressions (leading `!` or `!=`) keep a row only when
          // every searched cell satisfies the test; positive expressions keep
          // a row when any searched cell satisfies it.
          var isNegation = /^\\s*!|!=/.test(v);
          var method = isNegation ? 'every' : 'some';
          var evalCell = function(raw) {
            if (raw == null) return isNegation;
            try {
              var num = Number(raw);
              return !!fn(String(raw)) ||
                (raw !== '' && isFinite(num) && !!fn(num));
            } catch (e) {
              return false;
            }
          };
          return rows.filter(function(row) {
            return ids[method](function(id) {
              return evalCell(row.values[id]);
            });
          });
        }
      }
      // Substring mode with optional leading `!` for negation.
      var negate = v.charAt(0) === '!';
      var term = negate ? v.slice(1).trim() : v;
      if (term === '') return rows;
      var needle = term.toLowerCase();
      return rows.filter(function(row) {
        var match = ids.some(function(id) {
          var raw = row.values[id];
          return raw != null &&
            String(raw).toLowerCase().indexOf(needle) > -1;
        });
        return negate ? !match : match;
      });
    }",
    signature, ids
  ))
}
