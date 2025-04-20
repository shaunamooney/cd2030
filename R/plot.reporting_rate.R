#' Plot Reporting Rates for Subnational Units
#'
#' This S3 method visualizes reporting rates for ANC, delivery, or vaccination indicators
#' at subnational administrative levels using either heat maps or bar plots.
#'
#' @param x A `cd_reporting_rate` object. This should be the output of a reporting rate
#'   calculation function containing subnational reporting rate data.
#' @param plot_type A character string specifying the plot type. Options:
#'   - `'heat_map'`: Creates a tile map of reporting rates across years and units.
#'   - `'bar'`: Creates bar plots faceted by region/district showing year-wise trends.
#' @param indicator A character. The indicator to visualize. Must be one of:
#'   - `'anc_rr'`: Reporting rate for antenatal care visits
#'   - `'idelv_rr'`: Reporting rate for institutional deliveries
#'   - `'vacc_rr'`: Reporting rate for vaccination services
#' @param threshold Numeric. The threshold for categorizing high, medium, and low reporting
#'   (default = 90). Used in `heat_map` mode only.
#' @param ... Additional arguments passed to internal methods (currently unused).
#'
#' @details
#' The plot adapts based on:
#'
#' - `plot_type = 'heat_map'`:
#'   - Classifies reporting rates into three bands:
#'     - Red: `< 70`
#'     - Orange: `>= 70 & < threshold`
#'     - Green: `>= threshold`
#'   - Labels are overlaid on each tile.
#'
#' - `plot_type = 'bar'`:
#'   - Displays reporting rates as filled bars, faceted by district or adminlevel_1.
#'   - Colors are scaled using a gradient from red to green.
#'
#' This function only supports subnational data. National-level objects will trigger an error.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#'   # Example: Plot heat map of vaccination reporting rates
#'   plot(cd_reporting_rate_obj, plot_type = 'heat_map', indicator = 'vacc_rr')
#'
#'   # Example: Plot bar charts for institutional delivery reporting
#'   plot(cd_reporting_rate_obj, plot_type = 'bar', indicator = 'idelv_rr')
#' }
#'
#' @export
plot.cd_reporting_rate <- function(x,
                                   plot_type = c('heat_map', 'bar'),
                                   indicator = c('anc_rr', 'idelv_rr', 'vacc_rr'),
                                   threshold = 90,
                                   ...) {

  check_scalar_integerish(threshold)

  plot_type = arg_match(plot_type)
  indicator <- arg_match(indicator)
  admin_level <- attr(x, 'admin_level')

  if (admin_level == 'national') {
    cd_abort(c('x' = '{.fun plot.cd_reporting_rate} does not support national-level data.'))
  }

  all_years <- sort(unique(x$year))

  if (plot_type == 'heat_map') {

    greater <- paste0('>= ', threshold)
    mid <- paste0(' >= 70 and < ', threshold)
    low <- '< 70'

    dt <- x %>%
      mutate(
        year = factor(year, levels = all_years),
        color_category = case_when(
          !!sym(indicator) >= threshold ~ greater,
          !!sym(indicator) >= 70 & !!sym(indicator) < threshold ~ mid,
          !!sym(indicator) < 70 ~ low,
          .ptype = factor(levels = c(low, mid, greater))
        )
      )

    ggplot(dt, aes(x = !!sym(admin_level), y = year, fill = color_category)) +
      geom_tile(color = 'white') +
      scale_fill_manual(
        values = set_names( c('forestgreen', 'orange', 'red'), c(greater, mid, low)),
        name = 'Value Category',
        drop = FALSE
      ) +
      geom_text(aes(label = !!sym(indicator)), color = 'black', size = 3, vjust = 0.5) +
      labs(title = NULL, x = if (admin_level == 'district') 'District' else 'Admin Level 1', y = 'Year', fill = 'Value') +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, size = 9, hjust = 1))
  } else {
    low_value <- min(x[[indicator]], na.rm = TRUE)
    high_value <- max(x[[indicator]], na.rm = TRUE)

    color_vals <- c('red', 'orange', 'forestgreen')

    # Define breakpoints dynamically
    breaks_vals <- if (threshold >= 70) {
      c(low_value, 70, threshold, 100)
    } else {
      c(low_value, mean(low_value, threshold, na.rm = TRUE), threshold, 100)
    }

    ggplot(x, aes(year, !!sym(indicator), fill = !!sym(indicator))) +
      geom_col() +
      facet_wrap(as.formula(paste0('~', admin_level))) +
      scale_x_continuous(
        breaks = all_years,
        expand = expansion(mult = c(0, 0.05))
      ) +
      labs(title = paste0('Reporting rates by years and ', admin_level), x = 'Year', y = 'Reporting Rate') +
      scale_fill_gradientn(
        colors = color_vals,
        values = scales::rescale(breaks_vals, to = c(0, 1)),  # Ensures precise cutoffs
        breaks = scales::pretty_breaks(n = 5)(c(low_value, 100)),  # Uniformly spread breaks
        labels = scales::pretty_breaks(n = 5)(c(low_value, 100)),
        limits = c(low_value, 100),  # Ensure full scale is covered
        name = 'Reporting Rate'
      ) +
      theme(
        panel.background = element_blank(),
        strip.background = element_blank(),
        # strip.text = element_text(size = 12)
        panel.grid.major = element_line(colour = 'gray95'),
        axis.ticks = element_blank()
      )
  }
}

#' Plot District Reporting Rate Summary
#'
#' Generates a bar plot displaying the percentage of districts with reporting rates below
#' the defined threshold for multiple indicators across various years. This plot provides
#' a quick visual assessment of district-level reporting compliance across indicators
#' like ANC, Institutional Delivery, PNC, Vaccination, OPD, and IPD.
#'
#' @param x A `cd_district_reporting_rate` data frame containing reporting rate data,
#'   processed by [calculate_district_reporting_rate()].
#' @param ... Additional parameters passed to the plotting function.
#'
#' @details
#' This function inverts reporting rate percentages to display the proportion of
#' districts with rates below the threshold for each indicator (e.g., if a district
#' achieves a 95% rate, it shows as 5% below threshold). Each indicator is visualized
#' in a separate panel with data grouped by year, providing an overview of performance
#' trends over time.
#'
#' Indicators plotted include:
#' * **Antenatal Care (ANC)** - Percentage of districts meeting or exceeding target rates
#' * **Institutional Delivery** - Institutional delivery compliance over time
#' * **Postnatal Care (PNC)** - Districts' PNC service rates against thresholds
#' * **Vaccination** - Vaccination service coverage at district level
#' * **Outpatient Department (OPD)** - OPD reporting rates in districts
#' * **Inpatient Department (IPD)** - IPD reporting compliance by district
#'
#' The plot output includes a title, axis labels, and a legend for year, allowing
#' users to identify service areas with low reporting compliance.
#'
#' @return A ggplot object visualizing reporting rates across indicators and years.
#'
#' @examples
#' \dontrun{
#'   # Generate a plot of district reporting rates below a threshold of 90%
#'   plot(cd_district_reporting_rate(data), threshold = 90)
#' }
#' @export
plot.cd_district_reporting_rate <- function(x, ...) {

  year = value = indicator = low_mean_rr = NULL

  threshold <- attr(x, 'threshold')

  years <- x %>% distinct(year) %>% pull(year)
  base_colors <- c('darkgreen', 'orangered', 'royalblue4', 'indianred4', 'darkslategray4')

  extra_needed <- max(0, length(years) - length(base_colors))
  extra_colors <- if (extra_needed > 0) {
    scales::hue_pal()(extra_needed)
  } else {
    NULL
  }

  colors <- c(base_colors, extra_colors)
  names(colors) <- years

  # Invert the reporting rates and reshape for plotting
  x %>%
    select(-low_mean_rr) %>%
    mutate(across(starts_with('low_'), ~ 100 - ., .names = 'inv_{col}')) %>%
    pivot_longer(cols = starts_with('inv_low_'), names_to = 'indicator') %>%
    # Define indicator names and corresponding titles
    mutate(
      title = case_when(
        indicator == 'inv_low_anc_rr' ~ 'Antenatal Care',
        indicator == 'inv_low_idelv_rr' ~ 'Institutional Delivery',
        indicator == 'inv_low_vacc_rr' ~ 'Vaccination',
        indicator == 'inv_low_pnc_rr' ~ 'Postnatal Care',
        indicator == 'inv_low_opd_rr' ~ 'OPD',
        indicator == 'inv_low_ipd_rr' ~ 'IPD',
        .default = indicator
      )
    ) %>%
    # Create the plot with facet_wrap
    ggplot(aes(x = as.factor(year), y = value, fill = as.factor(year))) +
    geom_col(position = 'dodge') +
    geom_text(aes(label = round(value, 0)), position = position_dodge(width = 0.9), vjust = -1.5, color = 'black', size = 3) +
    facet_wrap(~title, scales = 'free_y', ncol = 3) +
    labs(
      title = paste('Figure 1a - Percentage of districts with low reporting rate (<', threshold, '%) by service and by year'),
      x = NULL, y = '%',
      caption = paste('Low reporting rate (<', threshold, '%)')
    ) +
    # scale_x_continuous(labels = scales::label_number()) +
    scale_y_continuous(limits = c(0, 100),
                       breaks = scales::pretty_breaks(n = 6),
                       expand = c(0, 0)) +
    cd_plot_theme() +
    theme(
      panel.grid.major.y = element_line(colour = 'gray90', size = 0.5),

      axis.text.x = element_blank(),
      axis.ticks = element_blank()
    ) +
    scale_fill_manual(values = colors)
}
