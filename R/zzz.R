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

# Keep each @importFrom on a single line: roxygen2 reads only the first line of
# an @importFrom tag, so wrapping silently drops the symbols on continuation lines.
#' @importFrom ggplot2 ggplot geom_point aes xlab ylab scale_y_discrete scale_colour_manual scale_shape_manual scale_x_continuous dup_axis guides guide_legend geom_vline geom_errorbar sec_axis annotate scale_x_discrete theme element_blank element_line margin element_rect element_text theme_minimal geom_rect
#' @importFrom utils tail
NULL

utils::globalVariables(
  unique(
    c(
      # From `plot_dot()`
      c(".data"),
      # From `plot_errorbar()`
      c(".data", "x1", "x2", "x3", "y")
    )
  )
)
