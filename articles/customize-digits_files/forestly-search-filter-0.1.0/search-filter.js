// Copyright (c) 2023 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
// All rights reserved.
//
// This file is part of the forestly program.
//
// forestly is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// Shared reactable search / filter implementation.
//
// The drill-down listing embeds one nested `reactable` per row of the main
// table. Serializing the full filter/search function body into every column of
// every nested table duplicates the same ~1.8 KB of JavaScript hundreds of
// thousands of times, which is what made the self-contained widget grow to
// well over a gigabyte. Instead we define the implementation once here as
// globals and reference them by name from R (see `search_filter_js()`), so each
// `filterMethod` / `searchMethod` serializes to a short reference string.
//
// `ids` is the array of column ids to search: a per-column filter passes
// `[columnId]`; a table-wide search passes all `columnIds`. Iterating with
// some()/every() over a one-element array reduces to testing that single cell.
(function () {
  function forestlyFilter(rows, ids, filterValue) {
    var v = filterValue.trim();
    if (v === '') return rows;
    // JS expression mode: the term references the cell variable `x`
    // (e.g. `x > 5`, `x !== "Rash"`, `!x.includes("Rash")`).
    if (/(^|[^\w$])x([^\w$]|$)/.test(v)) {
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
        var isNegation = /^\s*!|!=/.test(v);
        var method = isNegation ? 'every' : 'some';
        var evalCell = function (raw) {
          if (raw == null) return isNegation;
          try {
            var num = Number(raw);
            return !!fn(String(raw)) ||
              (raw !== '' && isFinite(num) && !!fn(num));
          } catch (e) {
            return false;
          }
        };
        return rows.filter(function (row) {
          return ids[method](function (id) {
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
    return rows.filter(function (row) {
      var match = ids.some(function (id) {
        var raw = row.values[id];
        return raw != null &&
          String(raw).toLowerCase().indexOf(needle) > -1;
      });
      return negate ? !match : match;
    });
  }

  // Per-column filterMethod signature: (rows, columnId, filterValue).
  window.__forestly_filter_column = function (rows, columnId, filterValue) {
    return forestlyFilter(rows, [columnId], filterValue);
  };

  // Table-wide searchMethod signature: (rows, columnIds, filterValue).
  window.__forestly_filter_table = function (rows, columnIds, filterValue) {
    return forestlyFilter(rows, columnIds, filterValue);
  };
})();
