#' Plot Outlier Detection Summary
#'
#' This method visualizes outlier detection results for immunization indicators
#' at subnational levels. The output plot depends on the `selection_type`:
#' - `'region'`: Plots percent of non-outliers by year and region.
#' - `'vaccine'`: Plots average non-outlier rate by year across all vaccines.
#' - `'heat_map'`: Shows either a heatmap of all indicators (if `indicator = NULL`)
#'   or a single indicator by year and region.
#'
#' @param x A `cd_outlier` object containing pre-processed outlier data.
#' @param selection_type One of `"region"`, `"vaccine"`, or `"heat_map"`:
#'   - `"region"`: Non-outlier rates for each region over time.
#'   - `"vaccine"`: Average non-outlier rates for each vaccine over time.
#'   - `"heat_map"`: Tile plot of indicators or a specific one.
#' @param indicator Optional. One of the supported indicators (`"opv1"`, `"penta3"`, etc.)
#'   to visualize in the plot. If `NULL` and `selection_type = "heat_map"`, all indicators
#'   will be shown.
#' @param ... Reserved for future use.
#'
#' @details
#' - Outliers are identified using the Hampel X84 method and summarized by indicator.
#' - Region and vaccine plots show % non-outliers using a diverging gradient scale.
#' - Heatmaps display the raw `*_outlier5std` values directly.
#'
#' @return A `ggplot` or `plotly` object depending on the selection type.
#'
#' @examples
#' \dontrun{
#'   # Region-level summary
#'   plot(outlier_data, selection_type = "region", indicator = "penta3")
#'
#'   # Heatmap of all indicators
#'   plot(outlier_data, selection_type = "heat_map")
#'
#'   # Heatmap of one indicator
#'   plot(outlier_data, selection_type = "heat_map", indicator = "bcg")
#' }
#' @export
plot.cd_outlier <- function(x,
                            selection_type = c('region', 'vaccine', 'heat_map'),
                            indicator = NULL,
                            ...) {

  admin_level <- attr(x, 'admin_level')

  indicator <- if (is.null(indicator) || indicator == '') {
    NULL
  } else {
    arg_match(indicator, list_vaccines())
  }

  selection_type <- arg_match(selection_type)

  if (selection_type %in% c('region', 'vaccine')) {
    data_prepared <- if (selection_type == 'region') {
      x %>%
        mutate(
          category = !!sym(admin_level),
          value = !!sym(paste0(indicator, '_outlier5std'))
        )
    } else {
      x %>%
        pivot_longer(cols = ends_with('_outlier5std'),
                     names_to = 'category',
                     names_pattern = '^(.*)_outlier5std') %>%
        summarise(value = mean(value, na.rm = TRUE), .by = c(year, category))
    }

    min_rr <- min(data_prepared$value, na.rm = TRUE)
    low_threshold <- ifelse(min_rr < 80, min_rr, 70)
    breaks_vals <- c(low_threshold, 70, 80, 90, 100)

    ggplot(data_prepared, aes(x = factor(year), y = value, fill = value)) +
      geom_col() +
      facet_wrap(~category) +
      labs(
        title = paste('Percent Non-Outliers by Year and', selection_type),
        x = 'Year', y = '% Non-Outliers', fill = '% Non-Outliers'
      ) +
      scale_fill_gradientn(
        colors = c('red', 'red', 'orange', 'yellowgreen', 'forestgreen'),
        values = scales::rescale(breaks_vals),
        limits = c(low_threshold, 100)
      ) +
      theme_minimal() +
      theme(
        panel.grid.major = element_line(color = 'gray95'),
        axis.ticks = element_blank(),
        strip.background = element_blank()
      )

  } else if (selection_type == 'heat_map') {
    if (is.null(indicator)) {
      x <- x %>%
        pivot_longer(cols = ends_with('_outlier5std'), names_to = 'indicator') %>%
        mutate(indicator = str_remove(indicator, '_outlier5std'))

      ggplot(x, aes(x = !!sym(admin_level), y = indicator, fill = value)) +
        geom_tile(color = 'white') +
        geom_text(aes(label = value), color = 'black', size = 3, vjust = 0.5) +
        scale_fill_gradient2(low = 'red3', mid = 'orange', high = 'forestgreen', midpoint = 80) +
        labs(x = admin_level, y = 'Indicator', fill = 'Value') +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
    } else {
      column_name <- paste0(indicator, '_outlier5std')
      ggplot(x, aes(x = !!sym(admin_level), y = factor(year), fill = !!sym(column_name))) +
        geom_tile(color = 'white') +
        geom_text(aes(label = !!sym(column_name)), color = 'black', size = 3, vjust = 0.5) +
        scale_fill_gradient2(low = 'red3', mid = 'orange', high = 'forestgreen', midpoint = 80) +
        labs(x = admin_level, y = 'Year', fill = paste0(indicator, ' Value')) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
    }
  }
}

#' Plot Outlier Series for a Single Region and Indicator
#'
#' This method visualizes the time series of a specific indicator for a single
#' region or district. It highlights outlier points beyond 5 MAD from the median.
#'
#' @param x A `cd_outlier_list` object, typically from `list_outlier_units()`.
#' @param region Character. Name of the region or district to plot.
#'
#' @details
#' - The plot includes:
#'   - Observed indicator values (green line and dots)
#'   - Median trend (cyan dashed line)
#'   - Shaded band for the 5×MAD threshold
#'   - Red points for flagged outliers
#'
#' This is intended for diagnostic plots during indicator validation or quality checks.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#'   # Visualize outliers for 'pcv1' in Nakuru
#'   dt %>%
#'     list_outlier_units("pcv1") %>%
#'     plot(region = "Nakuru")
#' }
#'
#' @export
plot.cd_outlier_list <- function(x, region = NULL) {

  indicator <- attr(x, 'indicator')
  admin_level <- attr(x, 'admin_level')
  if (is.null(region) || !is_scalar_character(region)) {
    abort('region must be a string')
  }

  med <- paste0(indicator, '_med')
  mad <- paste0(indicator, '_mad')

  x %>%
    mutate(
      date = ym(paste0(year, month, sep = '-')),
      upper_bound = !!sym(med) + !!sym(mad) * 5,
      lower_bound = !!sym(med) - !!sym(mad) *5,
      outlier_flag = !!sym(indicator) > upper_bound | !!sym(indicator) < lower_bound
    ) %>%
    filter(!!sym(admin_level) == region) %>%
    ggplot(aes(date)) +
    geom_line(aes(y = !!sym(indicator)), colour = 'forestgreen') +
    geom_point(aes(y = !!sym(indicator)), colour = 'forestgreen') +
    geom_line(aes(y = !!sym(med)), colour = 'cyan', linetype = 'dashed') +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "gray80", alpha = 0.5) +
    geom_point(data = function(df) filter(df, outlier_flag),
               aes(y = !!sym(indicator)), color = 'red', size = 2) +
    labs(
      title = NULL,
      y = paste0(indicator, ' doses given'),
      x = 'Month'
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_date(date_breaks = "1 months", date_labels = "%Y %b") +
    cd_plot_theme() +
    # theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, size = 16)
    )
}
