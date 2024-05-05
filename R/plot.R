plot_dot <- function(tbl,
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

  # Define variable index
  y_id <- which(names(tbl) %in% y_var)

  if (length(y_id) != 1) {
    stop(paste0("y_var is not uniquely defined in tbl"))
  }

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
      if (n_trt > 3) stop("Please define color to display groups")
      color <- c("#66203A", rev(c("#00857C", "#6ECEB2", "#BFED33")[1:n_trt]))
    }
  }
  color <- rep(color, length.out = n_trt)

  # Create vector for shape
  if (is.null(shape)) {
    shape <- rep(c("circle", "square", "diamond", "triangle"), length.out = n_trt)
  } else {
    shape <- rep(shape, length.out = n_trt)
  }

  # get number of columns for values
  n_col <- ncol(tbl)
  x <- 1:n_col
  names_col <- names(tbl)
  names(tbl) <- paste0("x", 1:ncol(tbl))

  # Transfer to factor
  if (class(item) != "factor") {
    item <- factor(item, levels = unique(item))
  }

  # define display order, make the first item display on the top
  item <- factor(item, levels = rev(levels(item)))
  names(tbl) <- paste0("x", 1:ncol(tbl))
  disp_order <- as.numeric(item)
  # Start create ana data frame for creating this panel
  ana <- data.frame(tbl, item = item, y = disp_order)

  num_id_combinations <- length(unique(interaction(ana$item, ana$y)))  # Calculate number of unique id combinations
  num_times <- length(names(tbl))  # Number of time points

  # Calculate maximum number of new rows
  max_new_rows <- num_id_combinations * num_times

  # This will make data frame 'wide' data to 'long'
  ana <- stats::reshape(ana,
                        idvar = c("item", "y"),
                        varying = c(names(tbl)),
                        v.names = "value",
                        new.row.names = 1:max_new_rows,
                        times = c(names(tbl)),
                        timevar = "name",
                        direction = "long"
  ) |>
    unique.data.frame()

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

  # define value for each variable
  grp_label <- label[ana$grp_order]
  p_color <- color[ana$grp_order]
  p_shape <- shape[ana$grp_order]
  ana$grp_order <- factor(ana$grp_order)
  # Creating final ana data frame
  ana <- data.frame(ana, grp_label, p_color, p_shape)

  if (is.null(legend_nrow)) {
    legend_flag <- FALSE
  } else {
    legend_flag <- TRUE
  }

  g <- ggplot2::ggplot(data = ana)
  # Create background for this panel
  g <- background_panel(g, background_color = background_color, background_alpha = background_alpha)
  # Create scatter plot for this panel
  g <- g + ggplot2::geom_point(
    data = ana, ggplot2::aes(x = .data[["value"]], y = .data[["y"]], color = .data[["grp_order"]], shape = .data[["grp_order"]]),
    size = 2, show.legend = legend_flag
  ) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +
    # theme +
    ggplot2::scale_y_discrete(limits = levels(ana[["item"]]))

  # Define color, shape and x-axis scale control and add title
  g <- g + ggplot2::scale_colour_manual(name = NULL, values = color, labels = label, na.translate = FALSE) +
    ggplot2::scale_shape_manual(name = NULL, values = shape, labels = label, na.translate = FALSE)

  if (!is.null(title)) {
    if(!is.null(x_breaks)){
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
        )} else{
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

  # control legend display of the output plot
  if (!is.null(legend_nrow)) {
    if (legend_nrow > length(label)) {
      stop("Number of legend row could not larger than number of treatment group")
    }
    g <- g + ggplot2::guides(
      color = ggplot2::guide_legend(nrow = legend_nrow),
      shape = ggplot2::guide_legend(nrow = legend_nrow)
    )
  }
  g + theme
}

plot_errorbar <- function(tbl,
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
  # Define variable index
  y_id <- which(names(tbl) %in% y_var)

  if (length(y_id) != 1) {
    stop(paste0("y_var is not uniquely defined in tbl"))
  }

  # Define item variable for y-axis label
  item <- tbl[[y_var]]

  # Standardize tbl
  tbl <- tbl[, -y_id]

  n_trt <- length(label)
  if (n_trt > 2 & grp_abbrev == "paired") {
    stop(paste0("Need to set argument 'grp_abbrev' as 'grouped' for a 'grouped' plot'"))
  }
  # Create vector for color
  # Create vector for color
  if (is.null(color)) {
    if (n_trt <= 2) {
      color <- c("#00857C", "#66203A")
    } else {
      if (n_trt > 3) stop("Please define color to display groups")
      color <- c("#66203A", rev(c("#00857C", "#6ECEB2", "#BFED33")[1:n_trt]))
    }
  }
  color <- rep(color, length.out = n_trt)

  # Create vector for shape
  if (is.null(shape)) {
    shape <- rep(c("circle", "square", "diamond", "triangle"), length.out = n_trt)
  } else {
    shape <- rep(shape, length.out = n_trt)
  }

  # get number of columns for values
  n_col <- ncol(tbl)
  x <- 1:n_col
  names_col <- names(tbl)
  names(tbl) <- paste0("x", 1:ncol(tbl))

  # Transfer to factor
  if (class(item) != "factor") {
    item <- factor(item, levels = unique(item))
  }


  # define display order, make the first item display on the top
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

  # define each column
  grp_label <- paste0(label[ana$grp_order], " vs ", label[length(label)])
  p_color <- color[ana$grp_order]
  p_shape <- shape[ana$grp_order]
  ana$grp_order <- factor(ana$grp_order)

  # create final ana data frame
  ana <- data.frame(ana, grp_label, p_color, p_shape)

  if (is.null(legend_nrow)) {
    legend_flag <- FALSE
  } else {
    legend_flag <- TRUE
  }

  g <- ggplot2::ggplot(data = ana)
  # create background for this panel
  g <- background_panel(g, background_color = background_color, background_alpha = background_alpha)

  # rotate use line type for reference line
  line_type <- rep(line_type, length.out = length(vline))

  # add point range and error bar for this plot panel
  g <- g + ggplot2::geom_point(data = ana, ggplot2::aes(x = x1, y = y, shape = .data[["grp_order"]], col = .data[["grp_order"]]), show.legend = legend_flag, na.rm = TRUE) +
    ggplot2::geom_errorbar(data = ana, ggplot2::aes(xmin = x2, xmax = x3, y = y, col = .data[["grp_order"]], width = errbar_width), show.legend = FALSE, na.rm = TRUE) +
    # add reference line
    ggplot2::geom_vline(xintercept = as.numeric(vline), linetype = line_type, alpha = 0.5, show.legend = FALSE, na.rm = TRUE) +
    # define color, shape scale control
    ggplot2::scale_colour_manual(name = NULL, values = color, labels = unique(grp_label), na.translate = FALSE) +
    ggplot2::scale_shape_manual(name = NULL, values = shape, labels = unique(grp_label), na.translate = FALSE) +
    ggplot2::xlab(NULL) + # add x-axis label
    ggplot2::ylab(NULL) +
    ggplot2::scale_y_discrete(limits = levels(ana[["item"]])) # define y-axis scale control


  # Generate label for favor bar
  if (grp_abbrev == "none"){
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
    if(!is.null(x_breaks)){
      # Calculate midpoint of x-axis for location of favor bar and panel title
      midpoint <- (min(ana$x2, x_breaks, na.rm = TRUE) + max(ana$x3, x_breaks, na.rm = TRUE)) / 2
      x_limit <- c(min(ana$x2, x_breaks, na.rm = TRUE), max(ana$x3, x_breaks, na.rm = TRUE))
      g <- g +
        ggplot2::scale_x_continuous(
          breaks = unique(c(x_breaks, vline)),
          labels = unique(as.character(c(x_breaks, vline))),
          limits = x_limit,
          name = favor_bar, # add favor bar
          # second x-axis for adding panel title
          sec.axis = ggplot2::sec_axis(trans = ~., name = "", breaks = midpoint, labels = title)
        )
    } else{
      x_breaks <- unique(c(pretty(c(ana$x1, ana$x2, ana$x3)), vline))
      brk_labels <- unique(as.character(c(pretty(c(ana$x1, ana$x2, ana$x3)), vline)))
      x_limit <- c(min(ana$x2, x_breaks, na.rm = TRUE), max(ana$x3, x_breaks, na.rm = TRUE))
      # Calculate midpoint of x-axis for location of favor bar and panel title
      midpoint <- sum(x_limit)/2
      g <- g +
        ggplot2::scale_x_continuous(
          name = favor_bar, # add favor bar
          breaks = x_breaks,
          labels = brk_labels,
          limits = x_limit,
          # second x-axis for adding panel title
          sec.axis = ggplot2::sec_axis(trans = ~., name = "", breaks = midpoint, labels = title)
        )
    }

  } else {
    if (is.null(x_breaks)) {
      x_breaks <- unique(c(pretty(c(ana$x1, ana$x2, ana$x3)), vline))
      brk_labels <- unique(as.character(c(pretty(c(ana$x1, ana$x2, ana$x3)), vline)))
    }else{
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

  # control legend cutoff
  if (!is.null(legend_nrow)) {
    if (legend_nrow > length(label)) {
      stop("Number of legend row could not larger than number of treatment group")
    }
    g <- g + ggplot2::guides(color = ggplot2::guide_legend(nrow = legend_nrow), shape = ggplot2::guide_legend(nrow = legend_nrow))
  }
  g + theme
}

nudge_unit <- function(n) {
  -0.5 + (1:n - 0.5) / n
}

table_panel <- function(tbl,
                        y_var,
                        x_label = NULL,
                        text_color = NULL,
                        text_size = 8,
                        text_format_by = "column",
                        background_color = c("#69B8F7", "#FFFFFF"),
                        theme = theme_panel(
                          show_ticks = TRUE,
                          show_text = TRUE),
                        background_alpha = 0.3
                        ) {

  #check that one variable name is provided as y_var
  if (length(y_var) > 1) {
    stop("y_var should contain only one variable name")
  }

  # Define variable index
  y_id <- which(names(tbl) %in% y_var)

  if (length(y_id) == 0) {
    stop("y_var does not exist in tbl")
  }

  # Define item variable for y-axis label
  item <- tbl[[y_var]]

  # Standardize tbl
  tbl <- tbl[, -y_id]


  # get number of columns for values
  n_col <- ncol(tbl)
  x <- 1:n_col
  names_col <- names(tbl)
  names(tbl) <- paste0("x", 1:ncol(tbl))

  # Transfer to factor
  if (class(item) != "factor") {
    item <- factor(item, levels = unique(item))
  }

  # Define Default Values
  if (is.null(x_label)) x_label <- names_col
  if (is.null(text_color)) text_color <- "black"

  # define display order, make the first item display on the top
  item <- factor(item, levels = rev(levels(item)))
  disp_order <- as.numeric(item)

  # Define data set for visulization
  ana_wide <- data.frame(tbl, item = item, y = disp_order)

  num_id_combinations <- length(unique(interaction(ana_wide$item, ana_wide$y)))  # Calculate number of unique id combinations
  num_times <- length(names(tbl))  # Number of time points

  # Calculate maximum number of new rows
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


    ## Transform to long format
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
        x$p_color <- ifelse(x$time == max(ana$time), tail(text_color, n = 1), rep(text_color, length.out = nrow(x)))
        x
      }
    )
    ana <- do.call(rbind, ana)
    row.names(ana) <- NULL
  }




  ## Define color by column
  if (text_format_by == "column") {
    if (length(text_color) == 1) text_color <- rep(text_color, n_col)
    ana$p_color <- text_color[ana$time]
  }

  # Transform to string
  ana$value <- ifelse(is.na(ana$value), NA, as.character(ana$value))
  ana$p_size <- text_size / ggplot2::.pt  # default size for ggplot::annotate is mm, we convert value into points(.pt) here

  # Define ggplot2 object
  g <- ggplot(data = ana)

  # create background for the panel
  g <- background_panel(g, background_color = background_color, background_alpha = background_alpha)

  # add value to the table panel
  g <- g + annotate("text",
                    x = ana[["time"]],
                    y = ana[["y"]],
                    label = ana[["value"]],
                    color = ana[["p_color"]],
                    size = ana[["p_size"]],
                    na.rm = TRUE
  )

  # scale control
  g <- g + scale_x_discrete(limits = factor(1:n_col), labels = x_label, position = "top") +
    scale_y_discrete(limits = levels(ana[["item"]])) +
    xlab(NULL) +
    ylab(NULL)

  # add theme
  g + theme
}

theme_panel <- function(show_text = TRUE,
                        show_ticks = TRUE){
  # Control display of ticks
  if (!show_ticks) {
    tick_control <- element_blank()
  } else {
    tick_control <- element_line()
  }
  # Control display of breaks
  if (!show_text) {
    text_control <- element_blank()
  } else {
    text_control <- element_text(color = "black", face = "bold")
  }
  theme_minimal() +
    theme(
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      panel.background = element_blank(),  # Remove panel background
      plot.background = element_blank(),   # Remove plot background
      panel.border = element_rect(fill = NA, color = "black", size = 1),  # Full frame
      axis.ticks = element_line(color = "black"),  # Ensure axis ticks are visible
      axis.text.y = text_control,
      axis.ticks.y = tick_control,
      legend.position = "bottom",
      plot.margin = margin(0,0,0,0)
    )
}

background_panel <- function(g,
                             background_color = c("#69B8F7", "#FFFFFF"),
                             background_alpha = 0.3) {
  # get levels' number of item as limits displayed on y-axis
  n <- length(levels(g$data$item))

  # check_factor(g$data$item)
  # create tmp to define background color for each row
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