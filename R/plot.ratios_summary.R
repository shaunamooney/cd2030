#' Plot Ratios Summary for Indicator Ratios Summary Object
#'
#' This function generates a bar plot for a `cd_ratios_summary` object,
#' displaying the calculated indicator ratios for each year. It allows
#' for visual comparison across years, showing how each indicator ratio
#' changes over time.
#'
#' @param x A `cd_ratios_summary` object created by the `calculate_ratios_summary` function.
#'   It should contain a `year` column and one or more columns with names starting with `"Ratio"`,
#'   representing the calculated indicator ratios.
#' @param ... Additional arguments passed to other methods (currently unused).
#'
#' @return A `ggplot` object representing a bar plot of indicator ratios by year.
#'
#' @details
#' This function provides a visual summary of indicator ratios, with each ratio displayed
#' as a bar for each year. The `Expected Ratio` row is included if available, allowing
#' for easy comparison of actual ratios against expected values. The bars are grouped
#' by year, with distinct colors representing each year for clear differentiation.
#'
#' @examples
#' \dontrun{
#'   # Assuming `cd_ratios_summary` is the object returned by `calculate_ratios_summary`
#'   plot(cd_ratios_summary)
#' }
#'
#' @export
plot.cd_ratios_summary <-  function(x, ...) {

  year = name = value = NULL

  plot_data <- x %>%
    select(year, starts_with('Ratio')) %>%
    pivot_longer(cols = -year) %>%
    mutate(year = factor(year, levels = sort(unique(year))))

  # Generate color palette with unique colors for each combination of name and year
  unique_names <- unique(plot_data$year)
  colors <- c('darkgreen', 'darkgoldenrod3', 'firebrick4', 'springgreen3', 'darkolivegreen3', 'steelblue2')
  color_mapping <- setNames(colors, unique_names)

  plot_data %>%
    ggplot(aes(name, value, fill = year)) +
      geom_col(position = position_dodge2()) +
      scale_y_continuous(expand = c(0,0)) +
      scale_fill_manual(values = color_mapping) +
      labs(
        title = 'Figure 1b: Ratio of number of facility reported ANC1 to penta1, penta1 to penta3 and of OPV1 to OPV3. compared with expected ratioss',
        x = NULL,
        y = NULL
      ) +
      cd_plot_theme()
}
