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

#' Dot plot
#'
#' Create a dot plot by item. For instance, this could be used to create AEs
#' incidence plot by Preferred Term and treatment group, as part of a
#' rainfall plot.
#'
#' @param tbl A data frame selected from input data set to display on this plot.
#'   y and x variables are required.
#' @param prop_cols A character vector of proportion columns to be used for a plot.
#' @param y_var A character string that specifies a variable to be displayed
#'   on the y-axis.
#' @param label A character vector of labels for each treatment group.
#'   The control group label should be specified as the last element
#'   of the vector.
#' @param x_breaks A numeric vector for x-axis breaks. Default is `NULL`,
#'   which uses a default ggplot2 x-axis breaks presentation.
#' @param color Color for each treatment group.
#' @param shape Shape for each treatment group. Default is circle and square.
#'   Input values could be either a character or numeric value, For details,
#'   see <https://ggplot2.tidyverse.org/articles/ggplot2-specs.html>.
#' @param title Panel title. Default is `"AE (%)"`.
#' @param background_color Plot background color. Default is
#'   `c("#69B8F7", "#FFFFFF")`, which are pastel blue and white.
#'   The value of this argument is used as input for the `background_color`
#'   argument in [background_panel()].
#' @param background_alpha Opacity of the background. Default is 0.3.
#'   The value of this argument is the input for `background_alpha` argument in
#'   [background_panel()].
#' @param theme Panel theme, including the y-axis text, ticks, and plot margin.
#'   Default is `theme_panel(show_text = TRUE, show_ticks = TRUE)`.
#'   For more details, refer to \code{\link{theme_panel}}.
#' @param legend_nrow Integer, the number of rows for a legend display.
#'   Must be smaller than or equal to the number of the treatment groups.
#'   To omit the legend, set this to `NULL`. Default is 1.
#'
#' @return AEs incidence plot by item and treatment group.
#'
#' @importFrom ggplot2 ggplot geom_point aes xlab ylab scale_y_discrete
#' scale_colour_manual scale_shape_manual scale_x_continuous dup_axis
#' guides guide_legend
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
#'
#' meta <- meta_forestly(
#'   dataset_adsl = forestly_adsl,
#'   dataset_adae = forestly_adae,
#'   population_term = "apat",
#'   observation_term = "wk12",
#'   parameter_term = "any;rel;ser"
#' ) |>
#'   prepare_ae_forestly() |>
#'   format_ae_forestly()
#'
#' meta_any <- meta$tbl[1:20, ] |> dplyr::filter(parameter == "any")
#' meta_any |>
#'   plot_dot("name", prop_cols = c("prop_1", "prop_2"), label = c("Treatment", "Placebo"))
plot_dot <- function(
    tbl,
    prop_cols = c("prop_1", "prop_2"),
    y_var,
    label,
    x_breaks = NULL,
    color = NULL,
    shape = NULL,
    title = "AE (%)",
    background_color = c("#69B8F7", "#FFFFFF"),
    background_alpha = 0.3,
    theme = theme_panel(show_text = TRUE, show_ticks = TRUE),
    legend_nrow = 1) {
  # Choose columns to use in tbl
  tbl <- tbl[, c(y_var, prop_cols)]

  # Define variable index
  y_id <- which(names(tbl) %in% y_var)

  if (length(y_id) != 1) stop("`y_var` is not uniquely defined in `tbl`.")

  # Define item variable for y-axis label
  item <- tbl[[y_var]]

  # Standardize tbl
  tbl <- tbl[, -y_id]

  n_trt <- length(label)
  # Create vector for color
  if (is.null(color)) {
    if (n_trt <= 2) {
      color <- c("#00857C", "#66203A")
    } else {
      if (n_trt > 3) stop("Must define color to display groups.")
      color <- c("#66203A", rev(c("#00857C", "#6ECEB2", "#BFED33")[1:n_trt]))
    }
  }
  color <- rep(color, length.out = n_trt)

  # Create vector for shape
  shape <- if (is.null(shape)) {
    rep(
      c("circle", "square", "diamond", "triangle"),
      length.out = n_trt
    )
  } else {
    rep(shape, length.out = n_trt)
  }

  # Get the number of columns for values
  n_col <- ncol(tbl)
  x <- 1:n_col
  names_col <- names(tbl)
  names(tbl) <- paste0("x", 1:ncol(tbl))

  # Encode as factor
  if (!is.factor(item)) item <- factor(item, levels = unique(item))

  # Define display order, make the first item display on the top
  item <- factor(item, levels = rev(levels(item)))
  names(tbl) <- paste0("x", 1:ncol(tbl))
  disp_order <- as.numeric(item)
  # Start create ana data frame for creating this panel
  ana <- data.frame(tbl, item = item, y = disp_order)

  # Calculate number of unique id combinations
  num_id_combinations <- length(unique(interaction(ana$item, ana$y)))
  num_times <- length(names(tbl)) # Number of time points

  # Calculate the maximum number of new rows
  max_new_rows <- num_id_combinations * num_times

  # This will make data frame 'wide' data to 'long'
  ana <- stats::reshape(
    ana,
    idvar = c("item", "y"),
    varying = c(names(tbl)),
    v.names = "value",
    new.row.names = 1:max_new_rows,
    times = c(names(tbl)),
    timevar = "name",
    direction = "long"
  ) |> unique.data.frame()

  ana <- lapply(
    split(ana, ana$y),
    function(x) {
      row_count <- max(nrow(x[!sum(is.na(x[, 1:n_col])) == n_col, ]), 1)
      x$y <- x$y + rev(nudge_unit(row_count))
      x$grp_order <- 1:nrow(x)
      x
    }
  )
  ana <- do.call(rbind, ana)

  ana <- ana[order(ana$item), ]
  row.names(ana) <- NULL

  # Define value for each variable
  grp_label <- label[ana$grp_order]
  p_color <- color[ana$grp_order]
  p_shape <- shape[ana$grp_order]
  ana$grp_order <- factor(ana$grp_order)
  # Create final ana data frame
  ana <- data.frame(ana, grp_label, p_color, p_shape)

  legend_flag <- if (is.null(legend_nrow)) FALSE else TRUE

  g <- ggplot2::ggplot(data = ana)
  # Create background for this panel
  g <- background_panel(
    g,
    background_color = background_color,
    background_alpha = background_alpha
  )
  # Create scatter plot for this panel
  g <- g + ggplot2::geom_point(
    data = ana, ggplot2::aes(
      x = .data[["value"]],
      y = .data[["y"]],
      color = .data[["grp_order"]],
      shape = .data[["grp_order"]]
    ),
    size = 2, show.legend = legend_flag
  ) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +
    ggplot2::scale_y_discrete(limits = levels(ana[["item"]]))

  # Define color, shape and x-axis scale control and add title
  g <- g +
    ggplot2::scale_colour_manual(
      name = NULL,
      values = color,
      labels = label,
      na.translate = FALSE
    ) +
    ggplot2::scale_shape_manual(
      name = NULL,
      values = shape,
      labels = label,
      na.translate = FALSE
    )

  if (!is.null(title)) {
    if (!is.null(x_breaks)) {
      # Calculate midpoint of x-axis for location of favor bar and panel title
      midpoint <- (min(x_breaks) + max(x_breaks)) / 2
      x_limit <- c(min(x_breaks), max(x_breaks))
      g <- g +
        ggplot2::scale_x_continuous(
          breaks = x_breaks,
          limits = x_limit,
          sec.axis = ggplot2::dup_axis(
            breaks = midpoint,
            labels = title
          )
        )
    } else {
      # Calculate midpoint of x-axis for location of favor bar and panel title
      midpoint <- (min(ana[["value"]]) + max(ana[["value"]])) / 2
      g <- g +
        ggplot2::scale_x_continuous(
          sec.axis = ggplot2::dup_axis(
            breaks = midpoint,
            labels = title
          )
        )
    }
  } else {
    if (is.null(x_breaks)) {
      x_breaks <- pretty(unique(c(ana$x1, ana$x2, ana$x3)))
    }
    x_limit <- c(min(x_breaks), max(x_breaks))
    g <- g +
      ggplot2::scale_x_continuous(
        breaks = x_breaks,
        limits = x_limit
      )
  }

  # Control legend display of the output plot
  if (!is.null(legend_nrow)) {
    if (legend_nrow > length(label)) {
      stop("The number of legend rows can't be larger than the number of treatment groups.")
    }
    g <- g + ggplot2::guides(
      color = ggplot2::guide_legend(nrow = legend_nrow),
      shape = ggplot2::guide_legend(nrow = legend_nrow)
    )
  }
  g + theme
}

#' Plot to display risk difference
#'
#' Create a plot to display risk difference for each item.
#'
#' @inheritParams plot_dot
#'
#' @param ci_cols A character vector of columns for a risk difference to be used for a plot.
#'   Need 3 columns, risk difference, lower bound, and upper bound.
#' @param errbar_width A numeric value to define the error bar width.
#'   Default is 0.4. Value of this argument will be a half length of the
#'   error bar, for example, `errorbar_width = 0.2` means half of the error bar
#'   width is 0.2 unit length. If y = 4, the error bar will range from
#'   y = 3.8 to y = 4.2.
#' @param grp_abbrev A character vector for displaying the treatment groups
#'   on a favor bar.
#'   If `grp_abbrev = "paired"`, treatment label on the error bar will be
#'   the same as in the `label` argument.
#'   If `grp_abbrev = "none"`, the error bar will not be shown.
#'   Also, for customized terms, users can provide an alternative vector
#'   of treatment labels.
#'   Default is `"paired"`.
#' @param favor_direction The position of a favor label under the condition
#'   "comparison is treatment – control". For AEs, `favor_direction` should be
#'   `"negative"`; for efficacy, `favor_direction` should be `"positive"`.
#' @param vline Vertical reference line position. Default is `NULL`.
#'   Users can define one or multiple numeric values in a vector as a
#'   reference line position.
#' @param line_type Reference line type. Default is solid line.
#'   Users can define one or multiple line types in a vector
#'   (can be numeric such as 1, 2, 3 or character such as `"solid"`, `"dashed"`).
#'   The values will be recycled and the order will be consistent with the
#'   argument `vline`.
#' @param title Plot title. Default is `"Risk Diff. + 95% CI \\n (Percentage Points)"`.
#'
#' @return A risk difference plot for each item.
#'
#' @importFrom ggplot2 ggplot geom_point geom_vline geom_errorbar aes
#' scale_x_continuous scale_y_discrete xlab ylab sec_axis guides guide_legend
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
#'
#' meta <- meta_forestly(
#'   dataset_adsl = forestly_adsl,
#'   dataset_adae = forestly_adae,
#'   population_term = "apat",
#'   observation_term = "wk12",
#'   parameter_term = "any;rel;ser"
#' ) |>
#'   prepare_ae_forestly() |>
#'   format_ae_forestly()
#'
#' meta_any <- meta$tbl[1:20, ] |> dplyr::filter(parameter == "any")
#' meta_any |>
#'   dplyr::select(name, diff_1, lower_1, upper_1) |>
#'   plot_errorbar(
#'     y_var = "name",
#'     ci_cols = c("diff_1", "lower_1", "upper_1"),
#'     label = c("Treatment", "Placebo")
#'   )
plot_errorbar <- function(
    tbl,
    ci_cols = c("diff_1", "lower_1", "upper_1"),
    y_var,
    errbar_width = 0.4,
    color = NULL,
    shape = NULL,
    label,
    x_breaks = NULL,
    grp_abbrev = "paired",
    favor_direction = "negative",
    vline = NULL,
    line_type = 1,
    title = "Risk Diff. + 95% CI \n (Percentage Points)",
    background_color = c("#69B8F7", "#FFFFFF"),
    background_alpha = 0.3,
    theme = theme_panel(show_text = TRUE, show_ticks = TRUE),
    legend_nrow = 1) {
  # Choose columns to use in tbl
  tbl <- tbl[, c(y_var, ci_cols)]

  # Define variable index
  y_id <- which(names(tbl) %in% y_var)

  if (length(y_id) != 1) stop("`y_var` is not uniquely defined in `tbl`.")

  # Define item variable for y-axis label
  item <- tbl[[y_var]]

  # Standardize tbl
  tbl <- tbl[, -y_id]

  n_trt <- length(label)
  if (n_trt > 2 & grp_abbrev == "paired") {
    stop("Must set argument `grp_abbrev` as `\"grouped\"` for a grouped plot.")
  }
  # Create vector for color
  # Create vector for color
  if (is.null(color)) {
    if (n_trt <= 2) {
      color <- c("#00857C", "#66203A")
    } else {
      if (n_trt > 3) stop("Must define color to display groups.")
      color <- c("#66203A", rev(c("#00857C", "#6ECEB2", "#BFED33")[1:n_trt]))
    }
  }
  color <- rep(color, length.out = n_trt)

  # Create vector for shape
  shape <- if (is.null(shape)) {
    rep(c("circle", "square", "diamond", "triangle"), length.out = n_trt)
  } else {
    rep(shape, length.out = n_trt)
  }

  # Get the number of columns for values
  n_col <- ncol(tbl)
  x <- 1:n_col
  names_col <- names(tbl)
  names(tbl) <- paste0("x", 1:ncol(tbl))

  # Encode as factor
  if (!is.factor(item)) item <- factor(item, levels = unique(item))

  # Define display order, make the first item display on the top
  item <- factor(item, levels = rev(levels(item)))
  names(tbl) <- paste0("x", 1:ncol(tbl))
  disp_order <- as.numeric(item)
  # Start create ana data frame for creating this panel
  ana <- data.frame(tbl, item = item, y = disp_order)

  ana <- lapply(
    split(ana, ana$y),
    function(x) {
      row_count <- max(nrow(x[!sum(is.na(x[, 1:n_col])) == n_col, ]), 1)
      x$y <- x$y + rev(nudge_unit(row_count))
      x$grp_order <- 1:nrow(x)
      x
    }
  )
  ana <- do.call(rbind, ana)

  # Define each column
  grp_label <- paste0(label[ana$grp_order], " vs ", label[length(label)])
  p_color <- color[ana$grp_order]
  p_shape <- shape[ana$grp_order]
  ana$grp_order <- factor(ana$grp_order)

  # Create final ana data frame
  ana <- data.frame(ana, grp_label, p_color, p_shape)

  legend_flag <- if (is.null(legend_nrow)) FALSE else TRUE

  g <- ggplot2::ggplot(data = ana)
  # Create background for this panel
  g <- background_panel(
    g,
    background_color = background_color,
    background_alpha = background_alpha
  )

  # Rotate use line type for reference line
  line_type <- rep(line_type, length.out = length(vline))

  # Add point range and error bar for this plot panel
  g <- g +
    ggplot2::geom_point(
      data = ana,
      ggplot2::aes(
        x = x1,
        y = y,
        shape = .data[["grp_order"]],
        col = .data[["grp_order"]]
      ),
      show.legend = legend_flag,
      na.rm = TRUE
    ) +
    ggplot2::geom_errorbar(
      data = ana,
      ggplot2::aes(
        xmin = x2,
        xmax = x3,
        y = y,
        col = .data[["grp_order"]],
        width = errbar_width
      ),
      show.legend = FALSE,
      na.rm = TRUE
    ) +
    # Add reference line
    ggplot2::geom_vline(
      xintercept = as.numeric(vline),
      linetype = line_type,
      alpha = 0.5,
      show.legend = FALSE,
      na.rm = TRUE
    ) +
    # Define color, shape scale control
    ggplot2::scale_colour_manual(
      name = NULL,
      values = color,
      labels = unique(grp_label),
      na.translate = FALSE
    ) +
    ggplot2::scale_shape_manual(
      name = NULL,
      values = shape,
      labels = unique(grp_label),
      na.translate = FALSE
    ) +
    ggplot2::xlab(NULL) + # Add x-axis label
    ggplot2::ylab(NULL) +
    ggplot2::scale_y_discrete(limits = levels(ana[["item"]])) # Define y-axis scale control

  # Generate label for favor bar
  if (grp_abbrev == "none") {
    favor_bar <- ""
  } else if (grp_abbrev == "paired") {
    grp_abbrev <- label
    if (favor_direction == "positive") {
      favor_bar <- paste0(grp_abbrev[2], "\u2190 Favor \u2192", grp_abbrev[1])
    } else if (favor_direction == "negative") {
      favor_bar <- paste0(grp_abbrev[1], "\u2190 Favor \u2192", grp_abbrev[2])
    }
  } else if (grp_abbrev != "none") {
    grp_abbrev <- c(grp_abbrev, "Control")
    if (favor_direction == "positive") {
      favor_bar <- paste0(grp_abbrev[2], "\u2190 Favor \u2192", grp_abbrev[1])
    } else if (favor_direction == "negative") {
      favor_bar <- paste0(grp_abbrev[1], "\u2190 Favor \u2192", grp_abbrev[2])
    }
  }

  if (!is.null(title)) {
    if (!is.null(x_breaks)) {
      # Calculate midpoint of x-axis for location of favor bar and panel title
      midpoint <- (min(ana$x2, x_breaks, na.rm = TRUE) + max(ana$x3, x_breaks, na.rm = TRUE)) / 2
      x_limit <- c(min(ana$x2, x_breaks, na.rm = TRUE), max(ana$x3, x_breaks, na.rm = TRUE))
      g <- g +
        ggplot2::scale_x_continuous(
          breaks = unique(c(x_breaks, vline)),
          labels = unique(as.character(c(x_breaks, vline))),
          limits = x_limit,
          name = favor_bar, # Add favor bar
          # Second x-axis for adding panel title
          sec.axis = ggplot2::sec_axis(trans = ~., name = "", breaks = midpoint, labels = title)
        )
    } else {
      x_breaks <- unique(c(pretty(c(ana$x1, ana$x2, ana$x3)), vline))
      brk_labels <- unique(as.character(c(pretty(c(ana$x1, ana$x2, ana$x3)), vline)))
      x_limit <- c(min(ana$x2, x_breaks, na.rm = TRUE), max(ana$x3, x_breaks, na.rm = TRUE))
      # Calculate midpoint of x-axis for location of favor bar and panel title
      midpoint <- sum(x_limit) / 2
      g <- g +
        ggplot2::scale_x_continuous(
          name = favor_bar, # Add favor bar
          breaks = x_breaks,
          labels = brk_labels,
          limits = x_limit,
          # Second x-axis for adding panel title
          sec.axis = ggplot2::sec_axis(trans = ~., name = "", breaks = midpoint, labels = title)
        )
    }
  } else {
    if (is.null(x_breaks)) {
      x_breaks <- unique(c(pretty(c(ana$x1, ana$x2, ana$x3)), vline))
      brk_labels <- unique(as.character(c(pretty(c(ana$x1, ana$x2, ana$x3)), vline)))
    } else {
      x_breaks <- unique(c(x_breaks, vline))
      brk_labels <- unique(as.character(c(x_breaks, vline)))
    }
    x_limit <- c(min(ana$x2, x_breaks, na.rm = TRUE), max(ana$x3, x_breaks, na.rm = TRUE))
    g <- g +
      ggplot2::scale_x_continuous(
        breaks = x_breaks,
        labels = brk_labels,
        limits = x_limit,
        name = favor_bar
      )
  }

  # Control legend cutoff
  if (!is.null(legend_nrow)) {
    if (legend_nrow > length(label)) {
      stop("The number of legend rows can't be larger than the number of treatment groups.")
    }
    g <- g + ggplot2::guides(
      color = ggplot2::guide_legend(nrow = legend_nrow),
      shape = ggplot2::guide_legend(nrow = legend_nrow)
    )
  }
  g + theme
}

#' Nudge around a unit
#'
#' Nudges around a unit.
#'
#' @param n A number of a group within a unit.
#'
#' @return A nudge for unit.
#'
#' @noRd
#'
#' @examples
#' nudge_unit(10)
nudge_unit <- function(n) {
  -0.5 + (1:n - 0.5) / n
}

#' Create table panel ggplot2 object for rainfall or forest plot
#'
#' Creates a table panel ggplot2 object for rainfall or forest plot.
#'
#' @param tbl A data frame to be displayed in this table.
#' @param y_var A string of a variable name from `tbl` for the y axis variable.
#' @param n_cols A character vector of columns for subject count to be used for a plot.
#' @param prop_cols A character vector of proportion columns to be used for a plot.
#' @param x_label Labels displayed on the top of table for each column of table.
#'   Default is `NULL`, variable name will display as label.
#' @param text_color Defines colors to display each treatment group.
#' @param text_size Numeric font size for data on each column.
#'   Default is 8 for each column.
#' @param text_format_by An option for formatting a data by columns or rows.
#'   Default is `"column"` and text color will be varied by column.
#'   If `text_format_by = "row"`, then text color will be varied by row.
#'   If `text_format_by = "group"`, then text color will be varied by treatment group.
#' @param background_color Color for the plot background.
#'   Default is `c("#69B8F7", "#FFFFFF")` which are pastel blue and white.
#'   The value of this argument will be the input value for the
#'   `background_color` argument in [background_panel()].
#' @param background_alpha Opacity of the background. Default is 0.3.
#'   The value of this argument will be the input value for the
#'   `background_alpha` argument in [background_panel()].
#' @param theme Controls display of y axis text, ticks and plot margin.
#'   By default, `theme_panel(show_text = TRUE, show_ticks = TRUE)` is used.
#'   Users are suggested to use [theme_panel()].
#'
#' @return A ggplot2 object for table panel.
#'
#' @importFrom ggplot2 annotate scale_x_discrete scale_y_discrete xlab ylab
#' @importFrom utils tail
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
#'
#' meta <- meta_forestly(
#'   dataset_adsl = forestly_adsl,
#'   dataset_adae = forestly_adae,
#'   population_term = "apat",
#'   observation_term = "wk12",
#'   parameter_term = "any;rel;ser"
#' ) |>
#'   prepare_ae_forestly() |>
#'   format_ae_forestly()
#'
#' meta_any <- meta$tbl[1:20, ] |> dplyr::filter(parameter == "any")
#'
#' meta_any |>
#'   table_panel(y_var = "name")
table_panel <- function(
    tbl,
    n_cols = c("n_1", "n_2"),
    prop_cols = c("prop_1", "prop_2"),
    y_var,
    x_label = NULL,
    text_color = NULL,
    text_size = 8,
    text_format_by = "column",
    background_color = c("#69B8F7", "#FFFFFF"),
    theme = theme_panel(
      show_ticks = TRUE,
      show_text = TRUE
    ),
    background_alpha = 0.3) {
  # Check that one variable name is provided as y_var
  if (length(y_var) > 1) stop("`y_var` should contain only one variable name.")

  # Check if the length of n and prop columns are same
  if (length(n_cols) != length(prop_cols)) {
    stop("The length of `n_cols` and `prop_cols` should be the same.")
  }
  # Derive combined columns from n and prop columns
  stats <- lapply(
    1:length(n_cols),
    function (x) {
      paste0(tbl[[n_cols[[x]]]], " (", tbl[[prop_cols[[x]]]], ")")
    }
  )
  stat_cols <- paste0("stat_", 1:length(n_cols))
  names(stats) <- stat_cols
  tbl <- cbind(tbl, stats)

  # Choose columns to use in tbl
  tbl <- tbl[, c(y_var, stat_cols)]

  # Define variable index
  y_id <- which(names(tbl) %in% y_var)

  if (length(y_id) == 0) stop("`y_var` does not exist in `tbl`.")

  # Define item variable for y-axis label
  item <- tbl[[y_var]]

  # Standardize tbl
  tbl <- tbl[, -y_id]

  # Get the number of columns for values
  n_col <- ncol(tbl)
  x <- 1:n_col
  names_col <- names(tbl)
  names(tbl) <- paste0("x", 1:ncol(tbl))

  # Encode as factor
  if (!is.factor(item)) item <- factor(item, levels = unique(item))

  # Define Default Values
  if (is.null(x_label)) x_label <- names_col
  if (is.null(text_color)) text_color <- "black"

  # Define display order, make the first item display on the top
  item <- factor(item, levels = rev(levels(item)))
  disp_order <- as.numeric(item)

  # Define dataset for visualization
  ana_wide <- data.frame(tbl, item = item, y = disp_order)

  # Calculate number of unique id combinations
  num_id_combinations <- length(unique(interaction(ana_wide$item, ana_wide$y)))
  num_times <- length(names(tbl)) # Number of time points

  # Calculate the maximum number of new rows
  max_new_rows <- num_id_combinations * num_times

  if (!text_format_by == "group") {
    ana_wide <- lapply(
      split(ana_wide, ana_wide$y),
      function(x) {
        row_count <- max(nrow(x[!sum(is.na(x[, 1:n_col])) == n_col, ]), 1)
        x$y <- x$y + rev(nudge_unit(row_count))
        if (text_format_by == "row") x$p_color <- rep(text_color, length.out = nrow(x))
        x
      }
    )
    ana_wide <- do.call(rbind, ana_wide)
    row.names(ana_wide) <- NULL

    # Transform to long format
    ana <- stats::reshape(
      data = ana_wide,
      idvar = c("item", "y"),
      varying = c(names(tbl)),
      new.row.names = 1:max_new_rows,
      v.names = "value",
      direction = "long"
    )
    row.names(ana) <- NULL
  }

  if (text_format_by == "group") {
    ana_long <- stats::reshape(
      data = ana_wide,
      idvar = c("item", "y"),
      varying = c(names(tbl)),
      new.row.names = 1:max_new_rows,
      v.names = "value",
      direction = "long"
    )
    row.names(ana_long) <- NULL
    ana_1 <- ana_long[!ana_long$time == max(ana_long$time), ]
    ana_2 <- ana_long[ana_long$time == max(ana_long$time), ] |> unique.data.frame()
    ana <- rbind(ana_1, ana_2)

    ana <- lapply(
      split(ana, list(ana$y, ana$time)),
      function(x) {
        x$y <- x$y + rev(nudge_unit(nrow(x)))
        x$p_color <- ifelse(
          x$time == max(ana$time),
          tail(text_color, n = 1),
          rep(text_color, length.out = nrow(x))
        )
        x
      }
    )
    ana <- do.call(rbind, ana)
    row.names(ana) <- NULL
  }

  # Define color by column
  if (text_format_by == "column") {
    if (length(text_color) == 1) text_color <- rep(text_color, n_col)
    ana$p_color <- text_color[ana$time]
  }

  # Transform to string
  ana$value <- ifelse(is.na(ana$value), NA, as.character(ana$value))
  # Default size for `ggplot::annotate()` is mm,
  # we convert value into points (pt) here.
  ana$p_size <- text_size / ggplot2::.pt

  # Define ggplot2 object
  g <- ggplot(data = ana)

  # Create background for the panel
  g <- background_panel(
    g,
    background_color = background_color,
    background_alpha = background_alpha
  )

  # Add value to the table panel
  g <- g + annotate(
    "text",
    x = ana[["time"]],
    y = ana[["y"]],
    label = ana[["value"]],
    color = ana[["p_color"]],
    size = ana[["p_size"]],
    na.rm = TRUE
  )

  # Scale control
  g <- g +
    scale_x_discrete(limits = factor(1:n_col), labels = x_label, position = "top") +
    scale_y_discrete(limits = levels(ana[["item"]])) +
    xlab(NULL) +
    ylab(NULL)

  # Add theme
  g + theme
}

#' Theme function for plot with multiple panels
#'
#' Specifies theme for a plot with multiple panels.
#'
#' @param show_ticks A logical value that controls ticks display on the y axis.
#'   Default is `TRUE`.
#' @param show_text A logical value that controls text display on the y axis.
#'   Default is `TRUE`.
#'
#' @return Theme for a specific panel.
#'
#' @importFrom ggplot2 ggplot theme element_blank element_line margin
#' element_rect element_text theme_minimal
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(mpg, aes(displ, hwy, colour = class)) +
#'   geom_point()
#'
#' p
#' p + theme_panel()
theme_panel <- function(show_text = TRUE, show_ticks = TRUE) {
  # Control display of ticks
  tick_control <- if (!show_ticks) element_blank() else element_line()

  # Control display of breaks
  text_control <- if (!show_text) element_blank() else element_text(color = "black", face = "bold")

  theme_minimal() +
    theme(
      panel.grid.major = element_blank(), # Remove major grid lines
      panel.grid.minor = element_blank(), # Remove minor grid lines
      panel.background = element_blank(), # Remove panel background
      plot.background = element_blank(), # Remove plot background
      panel.border = element_rect(fill = NA, color = "black", size = 1), # Full frame
      axis.ticks = element_line(color = "black"), # Ensure axis ticks are visible
      axis.text.y = text_control,
      axis.ticks.y = tick_control,
      legend.position = "bottom",
      plot.margin = margin(0, 0, 0, 0)
    )
}

#' Add background for creating plot with customized color
#'
#' Creates colored background for panels of rainfall or forest plot.
#'
#' @param g A ggplot object for adding colored background.
#' @param background_color A vector of colors that defines the color for the
#'   plot background. Default is c("#69B8F7", "#FFFFFF"),
#'   which are pastel blue and white. The colors will be recycled.
#' @param background_alpha Opacity of a geom. Default is 0.3.
#'
#' @return Plot as a colored background to add panels for rainfall or forest plot.
#'
#' @importFrom ggplot2 ggplot aes geom_rect
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' df <- data.frame(
#'   study = c("S1", "S2", "S3", "S4", "S5", "S6", "S7"),
#'   item = as.factor(1:7),
#'   effect = c(-.4, -.25, -.1, .1, .15, .2, .3),
#'   lower = c(-.43, -.29, -.17, -.02, .04, .17, .27),
#'   upper = c(-.37, -.21, -.03, .22, .24, .23, .33)
#' )
#'
#' ggplot(data = df) |>
#'   background_panel(background_color = c("grey", "white"), background_alpha = 0.4) +
#'   geom_point(aes(y = item, x = effect)) +
#'   geom_errorbar(aes(y = item, x = effect, xmin = lower, xmax = upper), width = 0.4) +
#'   scale_y_discrete(name = "", breaks = 1:nrow(df), labels = df$study)
background_panel <- function(
    g,
    background_color = c("#69B8F7", "#FFFFFF"),
    background_alpha = 0.3) {
  # Get levels' number of item as limits displayed on y-axis
  n <- length(levels(g$data$item))

  # check_factor(g$data$item)
  # Create tmp to define background color for each row
  tmp <- data.frame(
    row = 1:n,
    color = rev(rep(background_color, length.out = n))
  )

  # Create background
  ggplot(data = g$data) +
    geom_rect(
      data = tmp,
      xmin = -Inf,
      xmax = Inf,
      ymin = tmp[["row"]] - 0.5,
      ymax = tmp[["row"]] + 0.5,
      fill = tmp[["color"]],
      alpha = background_alpha,
      show.legend = FALSE,
      na.rm = TRUE
    )
}
