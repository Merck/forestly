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

`%||%` <- function(x, y) if (is.null(x)) y else x

# Column labels from a data frame's `label` attributes, falling back to the
# column name when a column has none.
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

# Attach `label` attributes to columns of `data`, defaulting any unlisted
# column's label to its own name.
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

  # Assign label. Resolve each column's position in `var` once via match()
  # instead of scanning `var` for every column.
  idx <- match(name, var)
  for (i in seq_along(name)) {
    attr(data[[i]], "label") <- label[idx[i]]
  }

  data
}
