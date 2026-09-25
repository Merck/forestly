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

# Minimal replacement for `brew::brew()`, supporting only the `<%=var%>` output
# form used by this package's JS templates. Each `<%=var%>` in `file` is
# replaced with the value of the variable `var` looked up in `envir` (the
# caller's environment by default), so callers never have to enumerate template
# variables. Returns the rendered text as a single string.
brew <- function(file, envir = parent.frame()) {
  template <- paste(readLines(file), collapse = "\n")
  m <- gregexpr("<%=.*?%>", template)
  names <- sub("^<%=\\s*(.*?)\\s*%>$", "\\1", regmatches(template, m)[[1]])
  regmatches(template, m) <- list(unlist(mget(names, envir = envir)))
  template
}
