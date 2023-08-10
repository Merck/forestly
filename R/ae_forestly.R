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
#' @param filter A character value of the filter variable.
#' @param width A numeric value of width of the table in pixels.
#'
#' @return An AE forest plot saved as a `shiny.tag.list` object.
#'
#' @export
#'
#' @examples
#' adsl <- forestly_adsl[1:100, ]
#' adae <- forestly_adae[1:100, ]
#' if (interactive()) {
#'   meta_forestly(
#'     dataset_adsl = adsl,
#'     dataset_adae = adae,
#'     population_term = "apat",
#'     observation_term = "wk12"
#'   ) |>
#'     prepare_ae_forestly(parameter = "any;rel") |>
#'     format_ae_forestly() |>
#'     ae_forestly()
#' }
ae_forestly <- function(outdata, filter = c("prop", "n"), width = 1400) {
  filter <- match.arg(filter)
  filter_range <- c(0, 100)

  parameters <- unlist(strsplit(outdata$parameter, ";"))
  par_label <- vapply(parameters,
    function(x) metalite::collect_adam_mapping(outdata$meta, x)$label,
    FUN.VALUE = character(1)
  )

  for (par in parameters[(!(parameters %in% unique(outdata$parameter_order)))]){
    outdata$tbl <-
      rbind(outdata$tbl, NA) |>
      dplyr::mutate(
        name = ifelse(is.na(name), "No data to display", name),
        parameter = factor(ifelse(is.na(parameter), par, as.character(parameter)),
                           levels(outdata$parameter_order))
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

  tbl <- crosstalk::SharedData$new(outdata$tbl)
  # Set default to be the first item
  default_param <- as.character(unique(outdata$tbl$parameter)[1])

  random_id <- paste0("filter_ae_", sample(1:9999, 1), "|", default_param)
  filter_ae <- crosstalk::filter_select(
    id = random_id,
    label = "AE Criteria",
    sharedData = tbl,
    group = ~parameter,
    multiple = FALSE
  )

  # Make a select list
  # Make a slider bar of the incidence percentage
  if (filter == "prop") {
    filter_subject <- crosstalk::filter_slider(
      id = "filter_subject",
      label = "Incidence (%) in One or More Treatment Groups",
      sharedData = tbl,
      column = ~hide_prop, # whose values will be used for this slider
      step = 1, # specifies interval between each select-able value on the slider
      width = 250, # width of the slider control
      min = filter_range[1], # the leftmost value of the slider
      max = filter_range[2] # the rightmost value of the slider
    )
  }

  if (filter == "n") {
    filter_subject <- crosstalk::filter_slider(
      id = "filter_subject",
      label = "Number of AE in One or More Treatment Groups",
      sharedData = tbl,
      column = ~hide_n,
      step = 1,
      width = 250,
      min = filter_range[1],
      max = filter_range[2]
    )
  }

  filter_subject$children[[2]]$attribs$`data-from` <- 0

  data_to <- ceiling(as.numeric(filter_subject$children[[2]]$attribs$`data-to`))
  data_to <- (data_to %/% 10 + 1) * 10
  filter_subject$children[[2]]$attribs$`data-to` <- data_to
  filter_subject$children[[2]]$attribs$`data-max` <- data_to

  p_reactable <- reactable2(
    tbl,
    columns = outdata$reactable_columns,
    columnGroups = outdata$reactable_columns_group,
    width = width,
    details = function(index) {
      t_row <- outdata$tbl$name[index]
      t_param <- outdata$tbl$parameter[index]

      t_details <- subset(
        outdata$ae_listing,
        (toupper(outdata$ae_listing$Adverse_Event) %in% toupper(t_row)) &
          (outdata$ae_listing$param == t_param)
      )

      row.names(t_details) <- NULL
      t_details[, !(names(t_details) == "param")] |>
        # eval(collect_adam_mapping(outdata$meta, t_param)$`subset`)) |>
        # & param == as.character(t_param))
        reactable2(
          width = width,
          col_def = reactable::colDef(
            header = function(value) gsub("_", " ", value, fixed = TRUE),
            cell = function(value) format(value, nsmall = 1),
            align = "center",
            minWidth = 70
          )
        )
    },
    # Default sort variable
    defaultSorted = c("parameter", names(outdata$diff)),
    defaultSortOrder = "desc"
  )

  p <- suppressWarnings(
    crosstalk::bscols(
      # Width of the select list and reactable
      widths = c(3, 9, 12, 0),
      filter_ae,
      filter_subject,
      p_reactable
    )
  )

  # Define JavaScript for crosstalk
  # remove (All)
  brew::brew(
    system.file("js/filter-crosstalk.js", package = "forestly"),
    output = file.path(tempdir(), "filter-crosstalk.js")
  )

  # Assemble html file
  offline <- TRUE

  htmltools::browsable(
    htmltools::tagList(
      html_dependency_filter_crosstalk(),
      reactR::html_dependency_react(offline),
      html_dependency_plotly(offline),
      html_dependency_react_plotly(offline),
      p
    )
  )
}
