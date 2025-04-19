#' Plot Derived vs Traditional Coverage Over Time
#'
#' This function generates a line plot comparing traditional (`coverage_old`)
#' and derived (`coverage_new`) coverage estimates over time for a single indicator.
#' It supports both national and subnational views.
#'
#' The plot title and y-axis labels are automatically generated from the
#' indicator metadata stored in the input object attributes.
#'
#' @param x A `cd_coverage_trends` object returned by [generate_coverage_data()].
#' @param region (Optional) A region or district name. Required for subnational data,
#'   must be `NULL` for national data.
#' @param ... Additional arguments passed to `ggplot2` layers (not used).
#'
#' @return A ggplot object showing coverage trends.
#'
#' @examples
#' \dontrun{
#'   generate_coverage_data(dhis_data, 'penta1', 2019) %>%
#'     plot(region = 'Nairobi')
#'
#'   generate_coverage_data(dhis_data, 'rota1', 2019) %>%
#'     plot()
#' }
#'
#' @export
plot.cd_derived_coverage <- function(x, region = NULL, ...) {

  admin_level <- attr_or_abort(x, 'admin_level')
  indicator <- attr_or_abort(x, 'indicator')
  indicator_label <- str_to_title(indicator)

  # Validate region input logic
  if (admin_level == 'national' && !is.null(region)) {
    cd_abort('x' = '{.arg region} must be null in national data.')
  }

  if (admin_level != 'national' && is.null(region)) {
    cd_abort('x' = '{.arg region} must not be null in subnational data.')
  }

  # Filter for specified region if applicable
  data <- if (admin_level != 'national') {
    x %>% filter(!!sym(admin_level) == region)
  } else {
    x
  }

  # Dynamic title based on admin level
  title_text <- if (admin_level == 'national') {
    str_glue('National {indicator_label} Coverage Over Time')
  } else {
    str_glue('{indicator_label} Coverage Over Time for {region}')
  }

  # Determine upper y-axis limit using rounded max
  max_val <- max(c(data$coverage_new, data$coverage_old), na.rm = TRUE)
  y_max <- ceiling(max_val / 10) * 10

  # Define custom legend labels
  legend_labels <- setNames(
    c("blue", "red"),
    c(paste0(indicator_label, " New"), paste0(indicator_label, " Old"))
  )

  ggplot(data, aes(x = year)) +
    geom_line(aes(y = coverage_new, color = paste0(indicator_label, ' New')), size = 1) +
    geom_line(aes(y = coverage_old, color = paste0(indicator_label, ' Old')), size = 1) +
    geom_hline(yintercept = 100, linetype = 'dashed', color = 'gray') +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 13),
      expand = expansion(mult = c(0, 0.05)),
      limits = c(0, y_max)
    ) +
    scale_color_manual(values = legend_labels) +
    labs(
      title = title_text,
      y = str_glue('{indicator_label} Coverage (%)'),
      x = 'Year',
      color = NULL
    ) +
    cd_plot_theme()
}
