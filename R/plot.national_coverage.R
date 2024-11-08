#' Plot National Coverage Data
#'
#' This function generates a line plot to visualize national immunization coverage
#' data across years, allowing comparison between different estimates (e.g., DHIS2
#' estimates, WUENIC estimates, and Survey estimates).
#'
#' @param x A data frame of type `cd_national_coverage`, containing year-wise
#'   national coverage data for the specified country and indicator.
#' @param ... Additional arguments passed to or from other methods (currently
#'   unused).
#'
#' @details
#' This plot function transforms the coverage data into a long format suitable for
#' plotting with ggplot2, and extracts the denominator information for display in
#' the plot caption. It shows coverage values for each year, with different lines
#' and points for each estimate type.
#'
#' The plot includes:
#' - **Y-axis**: Coverage percentage.
#' - **X-axis**: Year.
#' - **Line color**: Represents different estimates (e.g., DHIS2, Survey).
#' - **Caption**: Indicates the denominator used in the coverage calculation.
#'
#' This function is meant to be used with the `cd_national_coverage` class, which
#' contains coverage values for different indicators and estimates.
#'
#' @return A ggplot object displaying a line plot of the national coverage data
#'   by year, with lines and points representing different estimate types.
#'
#' @examples
#' \dontrun{
#'   plot(analyze_national_coverage(dt_adj, country_name = "Kenya",
#'         country_iso = "KEN", indicator = "bcg", denominator = "anc1"))
#' }
#'
#' @export
plot.cd_national_coverage <- function(x, ...) {

  estimates = year = value = NULL

  x <- x %>%
    pivot_longer(cols = -estimates, names_to = 'year') %>%
    mutate(year = as.integer(year))

  max_y <- max(x$value, na.rm = TRUE) * 1.05

  denominator <- x %>%
    filter(str_detect(estimates, 'anc') | str_detect(estimates, 'penta') | str_detect(estimates, 'dhis')) %>%
    distinct(estimates) %>%
    pull(estimates)

  x %>%
    ggplot(aes(year, value, colour = estimates)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, max_y),
      breaks = scales::pretty_breaks(11)
    ) +
    labs(y = "Coverage (%)",
         x = "Year",
         caption = paste0('Denominators derived from ', denominator)) +
    cd_plot_theme()
}
