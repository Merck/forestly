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
#'   ) |>
#'     prepare_ae_forestly() |>
#'     format_ae_forestly() |>
#'     ae_forestly()
#' }
ae_forestly <- function(outdata,
                        display_soc_toggle = TRUE,
                        filter = c("prop", "n"),
                        width = 1400) {
  filter <- match.arg(filter)
  filter_range <- c(0, 100)

  parameters <- unlist(strsplit(outdata$parameter, ";"))
  par_label <- vapply(parameters,
    function(x) metalite::collect_adam_mapping(outdata$meta, x)$label,
    FUN.VALUE = character(1)
  )

  for (par in parameters[(!(parameters %in% unique(outdata$parameter_order)))]){
    outdata$tbl <-
      rbind(outdata$tbl, NA)
    outdata$tbl$name <- ifelse(is.na(outdata$tbl$name), "No data to display", outdata$tbl$name)
    outdata$tbl$parameter <-
      factor(ifelse(is.na(outdata$tbl$parameter), par, as.character(outdata$tbl$parameter)),
             levels(outdata$parameter_order))
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

  random_id <- paste0("filter_ae_", uuid::UUIDgenerate(), "|", default_param)
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
    hidden_item = paste0("'", outdata$hidden_column, "'", collapse = ", "),
    soc_toggle = display_soc_toggle,
    width = width,
    details = function(index) {
      t_row <- outdata$tbl$name[index]
      t_param <- outdata$tbl$parameter[index]

      t_details <- subset(
        outdata$ae_listing,
        (toupper(outdata$ae_listing$Adverse_Event) %in% toupper(t_row)) &
          (outdata$ae_listing$param == t_param)
      )

      # Exclude 'param' column from t_details
      t_details <- t_details[, !(names(t_details) == "param")]

      # Get all labels from the un-subset data
      listing_label <- get_label(outdata$ae_listing)

      # Assign labels
      t_details <- assign_label(
        data = t_details,
        var = names(t_details),
        label = listing_label[match(names(t_details), names(listing_label))]
      )

      row.names(t_details) <- NULL

      # Extract labels for use in column definitions
      labels <- lapply(t_details, function(x) attr(x, "label"))

      # Create named column definitions using the labels
      col_defs <- setNames(
        lapply(names(t_details), function(name) {
          # Use label from the list
          label_name <- if(is.null(labels[[name]])) name else labels[[name]][[1]]
          reactable::colDef(
            header = label_name,  # Use header instead of name
            cell = function(value) format(value, nsmall = 1),
            align = "center",
            minWidth = 70
          )
        }),
        names(t_details)
      )

      # Create and return the reactable table for the nested view
      reactable::reactable(
        t_details,
        columns = col_defs,
        width = "100%", # Adjust width as needed
        resizable = TRUE,
        filterable = TRUE,
        searchable = TRUE,
        showPageSizeOptions = TRUE,
        borderless = TRUE,
        striped = TRUE,
        highlight = TRUE
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
