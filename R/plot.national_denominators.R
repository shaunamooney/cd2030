#' Plot National Denominators: Population or Births
#'
#' This function generates a plot to visualize national-level demographic data
#' for either total population or total live births over time. It allows a
#' comparison between DHIS-2 and UN projections for the specified metric, enabling
#' easy tracking of trends and discrepancies between these two data sources.
#'
#' @param x A `cd_national_denominators` object containing national-level
#'   demographic data.
#' @param metric A character string specifying the type of data to plot. It must
#'   be one of:
#'   - **"population"**: Plots total population estimates in thousands from DHIS-2 and UN sources.
#'   - **"births"**: Plots total live births in thousands from DHIS-2 and UN sources.
#' @param ... Additional arguments (not used in this function).
#'
#' @details
#' The `plot.cd_national_denominators` function provides two distinct visualizations
#' based on the selected `metric`:
#'   - **Population Plot**: Shows total population (in thousands) by year, with
#'     separate lines for DHIS-2 and UN projections.
#'   - **Live Births Plot**: Displays total live births (in thousands) by year,
#'     also comparing DHIS-2 and UN projections.
#'
#' The function dynamically adjusts the y-axis limits based on the maximum values
#' in the dataset to ensure a consistent scale and clear visualization. Each line
#' in the plot is accompanied by points to indicate yearly data, with legend labels
#' and colors specified for easy differentiation between DHIS-2 and UN data.
#'
#' @return A ggplot object visualizing the selected demographic data over time.
#'
#' @examples
#' \dontrun{
#'   # Plot total population estimates
#'   plot(cd_national_denominators_data, metric = "population")
#'
#'   # Plot total live births estimates
#'   plot(cd_national_denominators_data, metric = "births")
#' }
#'
#' @export
plot.cd_national_denominators <- function(x, metric = c("population", "births"), ...) {

  year = un_population = totpop_dhis2 = un_births = totlivebirths_dhis2 = NULL

  # Match argument to enforce valid choices
  metric <- match.arg(metric)

  if (metric == "population") {
    un_max <- max(x$un_population)
    dhis2_max <- max(x$totpop_dhis2)
    tot_max <- max(un_max, dhis2_max) *1.1
    tot_max <- ceiling(tot_max /1000 *1000)

    # Plot for Total Population (in thousands)
    ggplot(x, aes(x = year)) +
      geom_point(aes(y = un_population, color = "UN Population (in 1000)"), size = 4) +
      geom_line(aes(y = un_population, color = "UN Population (in 1000)"), size = 1) +
      geom_point(aes(y = totpop_dhis2, color = "DHIS-2 Population Projection (in 1000)"), size = 4) +
      geom_line(aes(y = totpop_dhis2, color = "DHIS-2 Population Projection (in 1000)"), size = 1) +
      labs(
        title = "Fig 2a: Total Population (in thousands), DHIS2 and UN projections",
        x = NULL
      ) +
      scale_y_continuous(
        name = "Population",
        limits = c(0, tot_max),
        breaks = scales::pretty_breaks(n = 6),
        expand = c(0,0)
      ) +
      cd_plot_theme() +
      theme(
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 10),

        legend.text = element_text(size = 10)
      ) +
      scale_color_manual(values = c("UN Population (in 1000)" = "darkgreen",
                                    "DHIS-2 Population Projection (in 1000)" = "red2"))
  } else if (metric == "births") {
    un_max <- max(x$un_births)
    dhis2_max <- max(x$totlivebirths_dhis2)
    tot_max <- max(un_max, dhis2_max) *1.1
    tot_max <- ceiling(tot_max /1000 *1000)

    # Plot for Total Live Births (in thousands)
    ggplot(x, aes(x = year)) +
      geom_point(aes(y = un_births, color = "UN Live Births (in 1000)"), size = 4) +
      geom_line(aes(y = un_births, color = "UN Live Births (in 1000)"), size = 1) +
      geom_point(aes(y = totlivebirths_dhis2, color = "DHIS-2 Live Births Projection (in 1000)"), size = 4) +
      geom_line(aes(y = totlivebirths_dhis2, color = "DHIS-2 Live Births Projection (in 1000)"), size = 1) +
      labs(
        title = "Fig 2b: Total Live Births (in thousands), DHIS2 and UN projections",
        x = NULL
      ) +
      scale_y_continuous(
        name = "Live Births",
        limits = c(0, tot_max),
        breaks = scales::pretty_breaks(n = 6),
        expand = c(0,0)
      ) +
      cd_plot_theme() +
      theme(
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 10),

        legend.text = element_text(size = 10)
      ) +
      scale_color_manual(values = c("UN Live Births (in 1000)" = "darkgreen",
                                    "DHIS-2 Live Births Projection (in 1000)" = "red2"))
  }
}
