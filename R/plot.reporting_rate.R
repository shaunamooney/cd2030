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
  colors <- c("darkgreen", "orangered", "royalblue4", "indianred4", "darkslategray4")

  names(colors) <- years

  # Invert the reporting rates and reshape for plotting
  x %>%
    select(-low_mean_rr) %>%
    mutate(across(starts_with('low_'), ~ 100 - ., .names = 'inv_{col}')) %>%
    pivot_longer(cols = starts_with('inv_low_'), names_to = 'indicator') %>%
    # Define indicator names and corresponding titles
    mutate(
      title = dplyr::case_when(
        indicator == 'inv_low_anc_rr' ~ 'Antenatal Care',
        indicator == 'inv_low_idelv_rr' ~ 'Institutional Delivery',
        indicator == 'inv_low_vacc_rr' ~ 'Vaccination',
        indicator == 'inv_low_pnc_rr' ~ 'Postnatal Care',
        indicator == 'inv_low_opd_rr' ~ 'OPD',
        indicator == 'inv_low_ipd_rr' ~ 'IPD',
        TRUE ~ indicator
      )
    ) %>%
    # Create the plot with facet_wrap
    ggplot(aes(x = as.factor(year), y = value, fill = as.factor(year))) +
      geom_col(position = 'dodge') +
      geom_text(aes(label = round(value, 0)), position = position_dodge(width = 0.9), vjust = -1.5, color = 'black', size = 5) +
      facet_wrap(~title, scales = 'free_y', ncol = 3) +
      labs(
        title = paste("Figure 1a - Percentage of districts with low reporting rate (<", threshold, "%) by service and by year"),
        x = NULL, y = '%',
        caption = paste("Low reporting rate (<", threshold, "%)")
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
