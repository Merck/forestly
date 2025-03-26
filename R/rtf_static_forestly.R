# Copyright (c) 2024 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
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

#' Output static forest plot
#'
#' @param outdata An `outdata` object created by [format_ae_forestly()].
#' @param plot_calls A list or vector of function calls.
#' @param source A character value of the data source.
#' @param parameter A character value of parameter term name.
#' @param n_rows An integer value of the number of rows per page in a plot.
#' @param fig_size A numeric vector of length 2 of figure width and height.
#'   The length should be 2 (width, height). The unit is inch.
#' @inheritParams r2rtf::rtf_page
#' @inheritParams r2rtf::rtf_body
#' @param footnotes A character vector of table footnotes.
#' @param title Term "analysis", "observation" and "population") for collecting title
#'   from metadata or a character vector of table titles.
#' @param path_outdata A character string of the outdata path.
#' @param path_outtable A character string of the outtable path.
#'
#' @return RTF file and source dataset for baseline characteristic table.
#'
#' @export
#'
#' @examples
#' forestly_adsl$TRTA <- factor(
#'   forestly_adsl$TRT01A,
#'   levels = c("Xanomeline Low Dose", "Placebo"),
#'   labels = c("Low Dose", "Placebo")
#' )
#' forestly_adae$TRTA <- factor(
#'   forestly_adae$TRTA,
#'   levels = c("Xanomeline Low Dose", "Placebo"),
#'   labels = c("Low Dose", "Placebo")
#' )
#' outdata <- meta_forestly(
#'   dataset_adsl = forestly_adsl,
#'   dataset_adae = forestly_adae
#' ) |>
#'   prepare_ae_forestly()|>
#'   format_ae_forestly()
#'
#' p1 <- substitute(
#'   plot_dot(
#'     tbl,
#'     prop_cols = c("prop_1", "prop_2"),
#'     y_var = "name",
#'     label = c("Treatment", "Placebo")
#'   )
#' )
#' p2 <- substitute(
#'   plot_errorbar(
#'     tbl,
#'     ci_cols = c("diff_1", "lower_1", "upper_1"),
#'     y_var = "name",
#'     label = c("Treatment", "Placebo"),
#'     legend_nrow = NULL,
#'     theme = theme_panel(show_ticks = FALSE, show_text = FALSE)
#'   )
#' )
#' p3 <- substitute(
#'   table_panel(
#'     tbl,
#'     n_cols = c("n_1", "n_2"),
#'     prop_cols = c("prop_1", "prop_2"),
#'     y_var = "name",
#'     theme = theme_panel(show_ticks = FALSE, show_text = FALSE),
#'     x_label = c("Treatment \n n(%)", "Placebo \n n(%)")
#'   )
#' )
#' outdata |> rtf_static_forestly(
#'   plot_calls = c(p1, p2, p3),
#'   source = "Source:  [CDISCpilot: adam-adsl; adae]",
#'   path_outdata = tempfile(fileext = ".Rdata"),
#'   path_outtable =  tempfile(fileext = ".rtf")
#' )
rtf_static_forestly <- function(
    outdata,
    plot_calls,
    source,
    parameter = "any", # only one parameter can be selected
    n_rows = 25,
    orientation = "portrait",
    fig_size = c(6, 6),
    title = c("analysis", "observation", "population"),
    footnotes = NULL,
    text_font_size = 9,
    path_outdata = tempfile(fileext = ".Rdata"),
    path_outtable = tempfile(fileext = ".rtf")
) {
  # Check if `plot_calls` is a list of function calls
  if (!all(sapply(plot_calls, is.call))) {
    stop("`plot_calls` must be a list of function calls.")
  }
  # Check if `tbl` is specified in each function call
  if (!all(sapply(plot_calls, function(x){"tbl" %in% as.character(x)}))) {
    stop("`tbl` must be specified in a function call.")
  }
  # Check if `figure_size` is a numeric vector of length 2
  if (!length(fig_size) == 2 | !all(sapply(fig_size, is.numeric))) {
    message("`fig_size` must be a numeric vector of length 2. Using default value.")
    fig_size <- c(6, 6)
  }

  # Load table from outdata
  tbl_all <- outdata$tbl
  tbl_all <- tbl_all[tbl_all[["parameter"]] == parameter, ]

  # Create plots
  i <- 1
  png_names <- NULL
  patches <- list()

  while (i < nrow(tbl_all)) {
    if (i + n_rows - 1 <= nrow(tbl_all)) {
      tbl <- tbl_all[i:(i + n_rows - 1), ]
    } else {
      tbl <- tbl_all[i:nrow(tbl_all), ]
    }

    # Set up env to pass `tbl` to each plot call
    env <- new.env(parent = parent.frame())
    assign("tbl", tbl, env)
    plots <- list()
    plots <- append(plots, lapply(plot_calls, eval, envir = env))

    # Combine plots
    patch <- do.call(patchwork::wrap_plots, plots) +
      # Define outer margin for assembled plot
      patchwork::plot_annotation(
        theme = theme(plot.margin = margin(1, 1, 1, 1, unit = "lines"))
      ) +
      patchwork::plot_layout(guides = "collect") & # Shared legend
      theme(legend.position = "bottom")

    patches <- append(patches, list(patch))

    filename <- file.path(tempdir(), paste0("forestplot_", ceiling((i - 1) / n_rows) + 1, ".png"))

    grDevices::png(filename = filename, width = fig_size[1], height = fig_size[2], units = "in", res = 300)
    print(patch)
    grDevices::dev.off()

    png_names <- c(png_names, filename)

    i <- i + n_rows
  }
  # Save plots in outdata
  outdata$fig <- patches

  # Set default title
  if ("analysis" %in% title | "observation" %in% title | "population" %in% title) {
    title <- metalite::collect_title(
      outdata$meta,
      outdata$population,
      outdata$observation,
      parameter,
      analysis = "ae_forestly", # if inherited, outdata$analysis
      title_order = title
    )
  }

  # Create rtf object
  outdata$rtf <- png_names |>
    r2rtf::rtf_read_figure() |>
    r2rtf::rtf_page(orientation = orientation) |>
    r2rtf::rtf_title(title) |>
    r2rtf::rtf_figure(fig_width = fig_size[1], fig_height = fig_size[2])

  if (!is.null(footnotes)) {
    outdata$rtf <- outdata$rtf |>
      r2rtf::rtf_footnote(footnotes,
                          text_font_size = text_font_size
      )
  }

  if (!is.null(source)) {
    outdata$rtf <- outdata$rtf |>
      r2rtf::rtf_source(source,
                        text_font_size = text_font_size
      )
  }

  # Prepare output
  if (!is.null(path_outdata)) {
    save(outdata, file = path_outdata)
    message("The outdata is saved in ", normalizePath(path_outdata))
  }

  if (!is.null(path_outtable)) {
    outdata$rtf |>
      r2rtf::rtf_encode(doc_type = "figure") |>
      r2rtf::write_rtf(file = path_outtable)
    message("The output is saved in ", normalizePath(path_outtable))
  }

  invisible(outdata)
}