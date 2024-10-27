#' Plot Comparison with Linear Fit and R-squared
#'
#' Creates a scatter plot to compare two indicators over multiple years, including
#' a linear regression line and a diagonal reference line. The plot is faceted by year,
#' and displays R-squared values for each year to assess the relationship between indicators.
#'
#' This function is primarily used to assess internal consistency across selected indicators
#' for a `cd_data` object. Users can specify any pair of indicators as x and y variables, allowing for
#' flexible comparison of various indicators.
#'
#' @param .data A `cd_data` object containing merged data for the specified indicators.
#' @param x_var Character. Name of the variable to plot on the x-axis (e.g., 'anc1').
#' @param y_var Character. Name of the variable to plot on the y-axis (e.g., 'penta1').
#' @param title Character. The main title for the plot. Defaults to a title based on `x_var` and `y_var`.
#' @param x_label Character. Label for the x-axis. Defaults to the value of `x_var`.
#' @param y_label Character. Label for the y-axis. Defaults to the value of `y_var`.
#' @param call The calling environment.
#' @param ... Additional arguments for customization, such as `size`, `color`, or `linetype`, for finer control over plot appearance.
#'
#' @details
#' This function calculates R-squared values for each year and displays them in each facet,
#' helping assess the relationship between the two indicators over time. The diagonal reference line
#' is added to aid in visualizing deviations from a perfect 1:1 relationship.
#'
#' Common indicator comparisons include:
#' - ANC1 vs PENTA1
#' - PENTA1 vs PENTA3
#' - OPV1 vs OPV3
#'
#' @return A ggplot2 object showing the comparison plot of two indicators with linear regression and R-squared values.
#'
#' @examples
#' \dontrun{
#'   # Example 1: Basic comparison between ANC1 and PENTA1 indicators
#'   plot_comparison(cd_data, x_var = 'anc1', y_var = 'penta1')
#'
#'   # Example 2: Customized plot with larger points and a custom theme
#'   plot_comparison(cd_data, x_var = 'anc1', y_var = 'penta1',
#'                   title = 'ANC1 vs PENTA1 Comparison',
#'                   x_label = 'ANC1 Coverage', y_label = 'PENTA1 Coverage',
#'                   facet_ncol = 2, plot_theme = theme_minimal())
#' }
#'
#' @export
plot_comparison.cd_data <- function(.data, x_var, y_var, title = NULL, x_label = NULL, y_label = NULL,
                                    call = caller_env(), ...) {

  district = year = min_x = max_x = r_squared = NULL

  check_cd_data(.data)
  check_required(x_var)
  check_required(y_var)

  # Check if specified indicators exist in the data
  if (!(x_var %in% names(.data$merged_data)) || !(y_var %in% names(.data$merged_data))) {
    cd_abort(c('x' = 'Specified indicators do not exist in "merged_data". Please check variable names.'))
  }

  # Filter data and calculate yearly sums
  data_filtered <- .data$merged_data %>%
    summarize(
      across(all_of(c(x_var, y_var)), sum, na.rm = TRUE),
      .by = c(district, year)
    ) %>%
    # Add min and max for the diagonal line
    mutate(
      min_x = min(!!sym(x_var), na.rm = TRUE),
      max_x = max(!!sym(x_var), na.rm = TRUE),
      .by = year
    )

  # Calculate R-squared values by year
  r_squared_values <- data_filtered %>%
    summarize(
      r_squared = round(summary(lm(as.formula(paste(y_var, "~", x_var)), data = cur_data()))$r.squared, 4),
      .by = year
    )


  # Set default title and labels if not provided
  title <- title %||% paste("Comparison of", x_var, "and", y_var, "by Year")
  x_label <- x_label %||% x_var
  y_label <- y_label %||% y_var

  # Create the plot
  plot <- ggplot(data_filtered, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    # Scatter plot points
    geom_point(color = 'navy', size = 1.5) +
    # Linear fit line
    geom_smooth(method = 'lm', formula = y ~ x, color = 'black', linetype = 'solid', se = FALSE, size = 0.8) +
    # Diagonal (identity) line
    # geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 0.8) +
    geom_segment(aes(x = min_x, y = min_x, xend = max_x, yend = max_x),
                 linetype = 'dashed', colour = 'red', size = 0.8) +
    # Facet by year with a 2-row layout
    facet_wrap(~year, nrow = 2) +
    labs(x = x_label, y = y_label, title = title) +
    # Add R-squared as text annotation
    geom_text(
      data = r_squared_values, aes(x = Inf, y = Inf, label = paste("R-squared:", r_squared)),
      hjust = 1.2, vjust = 1.2, size = 3, color = "grey", inherit.aes = FALSE
    ) +
    # Adjust theme for similar styling
    theme_bw() +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5),
      plot.subtitle = element_text(size = 8),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 8),
      strip.text = element_text(size = 8),
      # legend.position = 'none',
      legend.position = 'bottom',
      legend.box = "horizontal",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray", size = 0.1)
    ) +
    guides(color = guide_legend(title = 'Legend'))

  return(plot)
}

#' Internal Consistency Plot Functions for Indicator Comparisons
#'
#' These functions create scatter plots of two indicators with a linear regression line,
#' faceted by year, to assess internal consistency. Each function compares a specific pair of indicators,
#' with options for customizing titles, labels, and displaying R-squared values per year.
#'
#' @param .data A data frame of class `cd_data`.
#' @param x_var Name of the x-axis variable.
#' @param y_var Name of the y-axis variable.
#' @param title Title for the plot. Defaults to a title based on `x_var` and `y_var`.
#' @param x_label X-axis label. Defaults to `x_var`.
#' @param y_label Y-axis label. Defaults to `y_var`.
#' @param call The caller environment.
#' @param ... Additional parameters for customization.
#'
#' @return A `ggplot2` object. This plot includes:
#' - A scatter plot of the two specified indicators across years.
#' - A linear regression line to indicate the trend between the indicators.
#' - An optional R-squared value for each year, displayed on each facet.
#' - Faceting by year, allowing for easy comparison of trends over time.
#'
#' Users can render this `ggplot2` object directly, save it, or add additional
#' layers and modifications using the `+` operator.
#'
#' @examples
#' \dontrun{
#'   # Plot comparisons with generic function
#'   plot_comparison(cd_data, x_var = 'anc1', y_var = 'penta1')
#'
#'   # Compare ANC1 and Penta1 by year with the dedicated function
#'   plot_comparison_penta1_anc1(cd_data)
#' }
#'
#' @rdname internal_consistency
#'
#' @export
plot_comparison <- function(.data, x_var, y_var, title = NULL, x_label = NULL, y_label = NULL,
                            call = caller_env(), ...) {
  check_cd_data(.data, call = call)
  check_required(x_var, call = call)
  check_required(y_var, call = call)
  UseMethod("plot_comparison")
}

#' Plot Comparison of Penta1 and ANC1
#'
#' `plot_comparison_penta1_anc1` compares Penta1 and ANC1 indicators across.
#'
#' @rdname internal_consistency
#'
#' @export
plot_comparison_penta1_anc1 <- function(.data) {
  .data %>%
    plot_comparison(
      x_var = 'anc1',
      y_var = 'penta1',
      title = 'Comparison of ANC1 and Penta1 by Year',
      x_label = 'ANC1',
      y_label = 'Penta1'
    )
}

#' Plot Comparison of Penta1 and Penta2
#'
#' `plot_comparison_penta1_penta2` compares Penta1 and Penta2 indicators.
#'
#' @rdname internal_consistency
#'
#' @export
plot_comparison_penta1_penta2 <- function(.data) {
  .data %>%
    plot_comparison(
      x_var = 'penta1',
      y_var = 'penta3',
      title = 'Comparison of Penta1 and Penta3 by Year',
      x_label = 'Penta1',
      y_label = 'Penta3'
    )
}

#' Plot Comparison of OPV1 and OPV3
#'
#' `plot_comparison_opv1_opv3` compares OPV1 and OPV3 indicators.
#'
#' @rdname internal_consistency
#'
#' @export
plot_comparison_opv1_opv3 <- function(.data) {
  .data %>%
    plot_comparison(
      x_var = 'opv1',
      y_var = 'opv3',
      title = 'Comparison of OPV1 and OPV3 by Year',
      x_label = 'OPV1',
      y_label = 'OPV3'
    )
}

#' Plot Comparison of OPV1 and ANC1
#'
#' `plot_comparison_opv1_anc1` compares OPV1 and ANC1 indicators.
#'
#' @rdname internal_consistency
#'
#' @export
plot_comparison_opv1_anc1 <- function(.data) {
  .data %>%
    plot_comparison(
      x_var = 'opv1',
      y_var = 'anc1',
      title = 'Comparison of OPV1 and ANC1 by Year',
      x_label = 'OPV1',
      y_label = 'ANC1'
    )
}

#' Plot Comparison of PCV1 and PCV3
#'
#' `plot_comparison_pcv1_pcv3` compares PCV1 and PCV3 indicators.
#'
#' @rdname internal_consistency
#'
#' @export
plot_comparison_pcv1_pcv3 <- function(.data) {
  .data %>%
    plot_comparison(
      x_var = 'pcv1',
      y_var = 'pcv3',
      title = 'Comparison of PCV1 and PCV3 by Year',
      x_label = 'PCV1',
      y_label = 'PCV3'
    )
}

#' Plot Comparison of ROTA1 and ROTA2
#'
#' `plot_comparison_rota1_rota2` compares ROTA1 and ROTA2 indicators.
#'
#' @rdname internal_consistency
#'
#' @export
plot_comparison_rota1_rota2 <- function(.data) {
  .data %>%
    plot_comparison(
      x_var = 'rota1',
      y_var = 'rota2',
      title = 'Comparison of Rota1 and Rota2 by Year',
      x_label = 'Rota1',
      y_label = 'Rota2'
    )
}

#' Plot Comparison of IPV1 and IPV2
#'
#' `plot_comparison_ipv1_ipv2` compares IPV1 and IPV2 indicators.
#'
#' @rdname internal_consistency
#'
#' @export
plot_comparison_ipv1_ipv2 <- function(.data) {
  .data %>%
    plot_comparison(
      x_var = 'ipv1',
      y_var = 'ipv2',
      title = 'Comparison of IPV1 and IPV2 by Year',
      x_label = 'IPV1',
      y_label = 'IPV2'
    )
}

#' Plot Comparison of Penta1 and PCV1
#'
#' `plot_comparison_penta1_pcv1` compares Penta1 and PCV1 indicators.
#'
#' @rdname internal_consistency
#'
#' @export
plot_comparison_penta1_pcv1 <- function(.data) {
  .data %>%
    plot_comparison(
      x_var = 'penta1',
      y_var = 'pcv1',
      title = 'Comparison of Penta1 and PCV1 by Year',
      x_label = 'Penta1',
      y_label = 'PCV1'
    )
}

#' Plot Comparison of PCV3 and Penta3
#'
#' `plot_comparison_pcv3_penta3` compares PCV3 and Penta3 indicators.
#'
#' @rdname internal_consistency
#'
#' @export
plot_comparison_pcv3_penta3 <- function(.data) {
  .data %>%
    plot_comparison(
      x_var = 'pcv3',
      y_var = 'penta3',
      title = 'Comparison of PCV3 and Penta3 by Year',
      x_label = 'PCV3',
      y_label = 'Penta3'
    )
}

#' Plot Comparison of OPV1 and Penta1
#'
#' `plot_comparison_opv1_penta1` compares OPV1 and Penta1 indicators.
#'
#' @rdname internal_consistency
#'
#' @export
plot_comparison_opv1_penta1 <- function(.data) {
  .data %>%
    plot_comparison(
      x_var = 'opv1',
      y_var = 'penta1',
      title = 'Comparison of OPV1 and Penta1 by Year',
      x_label = 'OPV1',
      y_label = 'Penta1'
    )
}

#' Plot Comparison of OPV3 and Penta3
#'
#' `plot_comparison_opv3_penta3` compares OPV3 and Penta3 indicators.
#'
#' @rdname internal_consistency
#'
#' @export
plot_comparison_opv3_penta3 <- function(.data) {
  .data %>%
    plot_comparison(
      x_var = 'opv3',
      y_var = 'penta3',
      title = 'Comparison of OPV3 and Penta3 by Year',
      x_label = 'OPV3',
      y_label = 'Penta3'
    )
}

