#' Extended Plot Comparison between Two Indicators for cd_data
#'
#' This function creates a scatter plot comparison between two indicators for
#' each year in a `cd_data` object, with options for a linear fit, R-squared values,
#' and customization of plot appearance.
#'
#' @param .data A `cd_data` object containing the data to be plotted.
#' @param x_var A string specifying the column name for the x-axis variable (required).
#' @param y_var A string specifying the column name for the y-axis variable (required).
#' @param title The title of the plot. Defaults to a title based on `x_var` and `y_var`.
#' @param x_label The label for the x-axis. Defaults to `x_var`.
#' @param y_label The label for the y-axis. Defaults to `y_var`.
#' @param show_r_squared Logical; whether to display R-squared values on each facet. Default is TRUE.
#' @param facet_ncol Number of columns for faceting by year. Default is 3.
#' @param plot_theme ggplot theme to apply to the plot. Default is `theme_bw()`.
#' @param call The caller environment
#' @param ... Additional arguments for further customization, such as `size`, `color`, or `linetype`.
#'   These arguments allow finer control of plot aesthetics and appearance.
#' @return A ggplot object representing the comparison plot.
#' @examples
#' \dontrun{
#'   # Basic comparison plot of ANC1 and Penta1
#'   plot_comparison(cd_data_object, x_var = 'anc1', y_var = 'penta1')
#'
#'   # Custom comparison plot with increased point size and minimal theme
#'   plot_comparison(cd_data_object, x_var = 'anc1', y_var = 'penta1',
#'                   title = 'Custom Comparison', point_size = 2, plot_theme = theme_minimal())
#' }
#' @export
plot_comparison.cd_data <- function(.data, x_var, y_var, title = NULL, x_label = NULL, y_label = NULL,
                                    show_r_squared = TRUE, facet_ncol = 3, plot_theme = theme_bw(),
                                    call = caller_env(), ...) {

  district = year = min_x = max_x = r_squared = NULL

  check_cd_data(.data)
  check_required(x_var)
  check_required(y_var)

  # Check if specified indicators exist in the data
  if (!(x_var %in% names(.data$merged_data)) || !(y_var %in% names(.data$merged_data))) {
    cd_abort(c('x' = 'Specified indicators do not exist in "merged_data". Please check variable names.'))
  }

  # Prepare data for plotting
  df2 <- .data$merged_data %>%
    reframe(
      across(c(!!sym(x_var), !!sym(y_var)), sum, na.rm = TRUE),
      .by = c(district, year)
    )

  # Calculate R-squared per year if required
  if (show_r_squared) {
    r2_data <- df2 %>%
      summarise(
        r_squared = summary(lm(as.formula(paste(y_var, '~', x_var))))$r.squared,
        .by = year
      )
  }

  # Set default title and labels if not provided
  title <- title %||% paste("Comparison of", x_var, "and", y_var, "by Year")
  x_label <- x_label %||% x_var
  y_label <- y_label %||% y_var

  # Add min and max for the diagonal line
  df2 <- df2 %>%
    mutate(
      min_x = min(!!sym(x_var), na.rm = TRUE),
      max_x = max(!!sym(x_var), na.rm = TRUE),
      .by = year
    )

  # Create the plot
  plot <- ggplot(df2, aes(x = !!sym(x_var), y = !!sym(y_var))) +
    geom_point(colour = 'navy', size = 1.5) +
    geom_smooth(method = 'lm', formula = y ~ x , colour = 'black', se = FALSE) +
    geom_segment(aes(x = min_x, y = min_x, xend = max_x, yend = max_x),
                 linetype = 'dashed', colour = 'red', size = 1) +
    facet_wrap(~year, ncol = facet_ncol) +
    labs(x = x_label, y = y_label, title = title) +
    plot_theme +
    theme(legend.position = 'bottom', legend.box = "horizontal", strip.text = element_text(size = 8)) +
    guides(color = guide_legend(title = 'Legend'))

  # Conditionally add R-squared as text on each facet
  if (show_r_squared) {
    plot <- plot +
      geom_text(
        data = r2_data, aes(x = Inf, y = Inf, label = paste('R-squared:', round(r_squared, 4))),
        hjust = 1.1, vjust = 1.1, size = 3, color = 'grey', inherit.aes = FALSE
      )
  }

  return(plot)
}


#' Generic Function for Plotting Comparisons
#'
#' `plot_comparison` is a generic function that creates a comparison plot for
#' different data objects. It will dispatch to specific methods depending on
#' the class of the input object.
#'
#' @param .data A data object to be plotted.
#' @param x_var A string specifying the column name for the x-axis variable (required for `cd_data` objects).
#' @param y_var A string specifying the column name for the y-axis variable (required for `cd_data` objects).
#' @param title An optional title for the plot.
#' @param x_label An optional label for the x-axis.
#' @param y_label An optional label for the y-axis.
#' @param show_r_squared Logical; whether to display R-squared values on each facet. Default is TRUE.
#' @param facet_ncol Number of columns for faceting by year. Default is 3.
#' @param plot_theme ggplot theme to apply to the plot. Default is `theme_bw()`.
#' @param call The caller environment
#' @param ... Additional arguments passed to specific methods, such as `plot_comparison.cd_data`.
#' @export
plot_comparison <- function(.data, x_var, y_var, title = NULL, x_label = NULL, y_label = NULL,
                            show_r_squared = TRUE, facet_ncol = 3, plot_theme = theme_bw(),
                            call = caller_env(), ...) {
  check_cd_data(.data, call = call)
  check_required(x_var, call = call)
  check_required(y_var, call = call)
  UseMethod("plot_comparison")
}

#' Plot Comparison of Penta1 and ANC1
#'
#' This function compares Penta1 and ANC1 indicators across different years
#'   using a scatter plot with a linear fit.
#'
#' @param .data A data frame containing Penta1 and ANC1 data.
#' @return A ggplot object comparing Penta1 and ANC1.
#' @examples
#' \dontrun{
#'
#'   plot_comparison_penta1_anc1(data)
#' }
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
#' This function compares Penta1 and Penta2 indicators across different years
#'   using a scatter plot with a linear fit.
#'
#' @param .data A data frame containing Penta1 and Penta2 data.
#' @return A ggplot object comparing Penta1 and Penta2
#' @examples
#' \dontrun{
#'
#'   plot_comparison_penta1_penta2(data)
#' }
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
#' This function compares OPV1 and OPV3 indicators across different years
#'   using a scatter plot with a linear fit.
#'
#' @param .data A data frame containing OPV1 and OPV3 data.
#' @return A ggplot object comparing OPV1 and OPV3
#' @examples
#' \dontrun{
#'
#'   plot_comparison_opv1_opv3(data)
#' }
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
#' This function compares OPV1 and ANC1 indicators across different years
#'   using a scatter plot with a linear fit.
#'
#' @param .data A data frame containing OPV1 and ANC1 data.
#' @return A ggplot object comparing OPV1 and ANC1
#' @examples
#' \dontrun{
#'
#'   plot_comparison_opv1_anc1(data)
#' }
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
#' This function compares PCV1 and PCV3 indicators across different years
#'   using a scatter plot with a linear fit.
#'
#' @param .data A data frame containing PCV1 and PCV3 data.
#' @return A ggplot object comparing PCV1 and PCV3
#' @examples
#' \dontrun{
#'
#'   plot_comparison_pcv1_pcv3(data)
#' }
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
#' This function compares ROTA1 and ROTA2 indicators across different years
#'   using a scatter plot with a linear fit.
#'
#' @param .data A data frame containing ROTA1 and ROTA2 data.
#' @return A ggplot object comparing ROTA1 and ROTA2
#' @examples
#' \dontrun{
#'
#'   plot_comparison_rota1_rota2(data)
#' }
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
#' This function compares IPV1 and IPV2 indicators across different years
#'   using a scatter plot with a linear fit.
#'
#' @param .data A data frame containing IPV1 and IPV2 data.
#' @return A ggplot object comparing IPV1 and IPV2
#' @examples
#' \dontrun{
#'
#'   plot_comparison_ipv1_ipv2(data)
#' }
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
#' This function compares Penta1 and PCV1 indicators across different years
#'   using a scatter plot with a linear fit.
#'
#' @param .data A data frame containing Penta1 and PCV1 data.
#' @return A ggplot object comparing Penta1 and PCV1
#' @examples
#' \dontrun{
#'
#'   plot_comparison_penta1_pcv1(data)
#' }
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
#' This function compares PCV3 and Penta3 indicators across different years
#'   using a scatter plot with a linear fit.
#'
#' @param .data A data frame containing PCV3 and Penta3 data.
#' @return A ggplot object comparing PCV3 and Penta3
#' @examples
#' \dontrun{
#'
#'   plot_comparison_pcv3_penta3(data)
#' }
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
#' This function compares OPV1 and Penta1 indicators across different years
#'   using a scatter plot with a linear fit.
#'
#' @param .data A data frame containing OPV1 and Penta1 data.
#' @return A ggplot object comparing OPV1 and Penta1
#' @examples
#' \dontrun{
#'
#'   plot_comparison_opv1_penta1(data)
#' }
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
#' This function compares OPV3 and Penta3 indicators across different years
#'   using a scatter plot with a linear fit.
#'
#' @param .data A data frame containing OPV3 and Penta3 data.
#' @return A ggplot object comparing OPV3 and Penta3
#' @examples
#' \dontrun{
#'
#'   plot_comparison_opv3_penta3(data)
#' }
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

