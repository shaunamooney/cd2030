check_file_path <- function(path, call = caller_env()) {

  check_required(.data, call = call)

  # Validate if the file exists
  if (!file.exists(path)) {
    cd_abort(
      c("x" = "The specified file {.val {path}} does not exist. Please provide a valid file path."),
      call = call
    )
  }
}

check_cd_indicator_coverage <- function(.data, call = caller_env()) {

  check_required(.data, call = call)

  if (!inherits(.data, 'cd_indicator_coverage')) {
    cd_abort(c('x' = 'The data object must be of class "cd_indicator_coverage".'), call = call)
  }

}

check_cd_data <- function(.data, call = caller_env()) {

  check_required(.data, call = call)

  if (!inherits(.data, 'cd_data')) {
    cd_abort(c('x' = 'The data object must be of class "cd_data".'), call = call)
  }

}

check_un_estimates_data <- function(.data, call = caller_env()) {

  check_required(.data, call = call)

  if (!inherits(.data, 'cd_un_estimates')) {
    cd_abort(c('x' = 'The data object must be of class "cd_un_estimates".'), call = call)
  }
}

check_wuenic_data <- function(.data, call = caller_env()) {

  check_required(.data, call = call)

  if (!inherits(.data, 'cd_wuenic_data')) {
    cd_abort(c('x' = 'The data object must be of class "cd_un_estimates".'), call = call)
  }
}

check_survey_data <- function(.data, call = caller_env()) {

  check_required(.data, call = call)

  if (!inherits(.data, 'cd_survey_data')) {
    cd_abort(c('x' = 'The data object must be of class "cd_un_estimates".'), call = call)
  }
}

check_equity_data <- function(.data, call = caller_env()) {

  check_required(.data, call = call)

  if (!inherits(.data, 'cd_equity_data')) {
    cd_abort(c('x' = 'The data object must be of class "cd_un_estimates".'), call = call)
  }
}

check_ratio_pairs <- function(.list, arg = call_args(.list), call = caller_env()) {

  check_required(.data, call = call)

  # Check that ratio_pairs is a named list with each element as a character vector of length 2
  is_ratio_pairs <- all(
    is.list(.list),
    all(lengths(.list) == 2),
    all(map_lgl(.list, ~ is.character(.x) && length(.x) == 2))
  )

  if (!is_ratio_pairs) {
    cd_abort(c('x' = '{.arg arg} is not a proper ratio pair.'), call = call)
  }

}


cd_plot_theme <- function() {
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),

    legend.background = element_rect(color = "black",size = 0.5),
    legend.position = "bottom",
    legend.title = element_blank(),
    # legend.text = element_text(size = 8),
    legend.text = element_text(size = 13),
    legend.key.size = unit(8, "mm"),

    # plot.title = element_text(size = 10, hjust = 0.5),
    # plot.subtitle = element_text(size = 10, hjust = 0.5),
    # plot.caption = element_text(hjust = 0),

    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 12, hjust = 0),

    # axis.text = element_text(size = 8),
    axis.text = element_text(size = 14),
    # axis.title = element_text(size = 10),
    axis.title = element_text(size = 18),

    strip.background = element_blank(),
    # strip.text = element_text(size = 10),
    strip.text = element_text(size = 12)
  )
}

cd_report_theme <- function() {
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),

    legend.background = element_rect(color = "black",size = 0.5),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    # legend.text = element_text(size = 13),
    # legend.key.size = unit(8, "mm"),

    plot.title = element_text(size = 10, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.caption = element_text(hjust = 0),

    # plot.title = element_text(size = 16, hjust = 0.5),
    # plot.subtitle = element_text(size = 12, hjust = 0.5),
    # plot.caption = element_text(size = 12, hjust = 0),

    axis.text = element_text(size = 8),
    # axis.text = element_text(size = 14),
    axis.title = element_text(size = 10),
    # axis.title = element_text(size = 18),

    strip.background = element_blank(),
    strip.text = element_text(size = 10),
    # strip.text = element_text(size = 12)
  )
}



#' Plot Line Graph for Multiple Series with Dynamic Y-axis Scaling
#'
#' Generates a line graph for specified y variables over a shared x-axis, with dynamic scaling of the y-axis based on the data.
#'
#' @param .data A data frame containing the variables to plot.
#' @param x The unquoted column name for the x-axis variable (e.g., `year`).
#' @param y_vars A character vector of column names for the y variables to plot.
#' @param y_labels A character vector of labels for the y variables (must match the length of `y_vars`).
#' @param title The title of the plot.
#' @param y_axis_title The title of the y-axis.
#' @param hline An optional numeric value to draw a horizontal line.
#' @param hline_style The line style for the horizontal line, default is "dashed".
#'
#' @return A ggplot object.
#' @export
plot_line_graph <- function(.data, x, y_vars, y_labels, title, y_axis_title, hline = NULL, hline_style = 'dashed') {

  variable = value = NULL

  # Ensure `y_vars` and `y_labels` are of the same length
  if (length(y_vars) != length(y_labels)) {
    cd_abort(c('x' = '`y_vars` and `y_labels` must have the same length.'))
  }

  # Determine max and min for dynamic scaling, accounting for negatives
  y_max <- max(sapply(y_vars, function(var) max(.data[[var]], na.rm = TRUE)), na.rm = TRUE)
  y_min <- min(sapply(y_vars, function(var) min(.data[[var]], na.rm = TRUE)), na.rm = TRUE)

  # Set y_min to 0 if all values are non-negative
  if (y_min > 0) {
    y_min <- 0
  }

  # Choose the scaling factor and y-axis label suffix
  scale_factor <- if (y_max > 1e6) 1e6 else if (y_max > 1e3) 1e3 else 1
  y_label_suffix <- if (scale_factor == 1e6) " (Millions)" else if (scale_factor == 1e3) " (Thousands)" else ""

  # Adjust limits based on scale factor and round up/down
  y_max <- ceiling(y_max / scale_factor) * scale_factor
  y_min <- floor(y_min / scale_factor) * scale_factor

  # Add a small allowance to y_max to create space at the top of the plot
  y_max <- y_max * 1.05

  # Set dynamic breaks for the y-axis
  y_breaks <- scales::pretty_breaks(n = 6)(c(y_min, y_max))

  # Reshape data for ggplot
  .data_long <- .data %>%
    select({{ x }}, !!!syms(y_vars)) %>%
    pivot_longer(cols = all_of(y_vars), names_to = 'variable', values_to = 'value') %>%
    mutate(
      variable = factor(variable, levels = y_vars, labels = y_labels),
      value = value / scale_factor
    )

  # Generate the line plot
  p <- .data_long %>%
    ggplot(aes(x = !!sym(x), y = value, colour = variable, group = variable)) +
      geom_point(size = 4) +
      geom_line(size = 1) +
      labs(
        title = title,
        x = NULL,
        y = paste0(y_axis_title, y_label_suffix)
      ) +
      scale_y_continuous(
        limits = c(y_min / scale_factor, y_max / scale_factor),
        breaks = y_breaks / scale_factor,
        labels = scales::label_number(accuracy = 1, big.mark = ",")
        # expand = c(0, 0)
      ) +
      scale_color_manual(values = set_names(c("darkgreen", "red2", "blue", "purple", "orange", "brown")[1:length(y_vars)], y_labels)) +
      cd_plot_theme() +
      theme(
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_blank()
    )

  # Add horizontal line if specified
  if (!is.null(hline)) {
    p <- p +
      geom_hline(yintercept = hline, linetype = hline_style, color = "grey40")
  }

  return(p)
}
