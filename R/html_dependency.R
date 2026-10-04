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

# Script + styles for the forestly-owned controls around the main AE forest
# table (parameter dropdown, incidence range slider, CSV download). They drive
# the lt table through its `el._lt.filter()` contract (see
# inst/js/forestly-widgets.js).
html_dependency_forestly_widgets <- function() {
  htmltools::htmlDependency(
    name = "forestly-widgets",
    version = "0.1.0",
    src = system.file(package = "forestly"),
    script = c("js/forestly-widgets.js"),
    stylesheet = c("css/forestly-widgets.css"),
    all_files = FALSE
  )
}

# Cosmetics for the lt-rendered AE drill-down listings (see
# inst/css/ae-drilldown.css).
html_dependency_ae_drilldown <- function() {
  version <- "0.1.0"
  htmltools::htmlDependency(
    name = "forestly-ae-drilldown",
    version = version,
    src = system.file("css", package = "forestly"),
    stylesheet = c("ae-drilldown.css"),
    all_files = FALSE
  )
}
