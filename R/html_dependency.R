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

html_dependency_react_plotly <- function(offline = TRUE) {
  version <- "1.0.2"
  hd <- htmltools::htmlDependency(
    name = "react-plotly",
    version = version,
    src = system.file("js", package = "forestly"),
    script = c("create-plotly-component.js"),
    all_files = FALSE
  )

  if (!offline) {
    hd$src <- list(href = "https://unpkg.com")
    hd$script <- c("react-plotly.js@1.0.2/dist/create-plotly-component.js")
  }

  hd
}

html_dependency_plotly <- function(offline = TRUE) {
  version <- "1.58.5"
  hd <- htmltools::htmlDependency(
    name = "plotly",
    version = version,
    src = system.file("js", package = "forestly"),
    script = c("plotly-min.js"),
    all_files = FALSE
  )

  if (!offline) {
    hd$src <- list(href = "https://cdn.plot.ly")
    hd$script <- c("plotly-1.58.5.min.js")
  }

  hd
}

html_dependency_filter_crosstalk <- function() {
  version <- "0.1.0"
  htmltools::htmlDependency(
    name = "filter-crosstalk",
    version = version,
    src = tempdir(),
    script = c("filter-crosstalk.js"),
    all_files = FALSE
  )
}
