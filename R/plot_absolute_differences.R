#' Plot DHIS2-Based Indicator Coverage with Different Denominators and Survey Coverage
#'
#' `plot_absolute_differences` generates a bar plot comparing facility-based coverage
#' percentages for a selected health indicator with different denominator sources (e.g., DHIS2,
#' ANC1-derived, Penta1-derived, UN projection) and overlays a horizontal line representing
#' national survey coverage for the specified year.
#'
#' @param .data A data frame containing columns for `country`, `year`, and the coverage values
#'   for various denominators. Each indicator should have columns ending with suffixes like `_dhis2`, `_anc1`, `_penta1`, and `_un`.
#' @param indicator A character string specifying the health indicator to plot. Options include, but are not limited to:
#'   `mcv1`, `penta3`, `bcg`.
#' @param survey_coverage A numeric value representing the national survey coverage percentage
#'   to be displayed as a reference line on the plot. Default is `88`.
#' @param coverage_year A single integer specifying the year of interest for the plot.
#'   Only data for this year will be plotted.
#'
#' @return A `ggplot2` object showing the coverage percentages for the selected indicator
#'   from various denominator sources, with a reference line for survey coverage.
#'
#' @details
#' This function allows for visualization of the discrepancy between facility-based coverage
#' data sourced from different denominators (e.g., DHIS2 projections, ANC1-derived, Penta1-derived,
#' UN projections) and the national survey coverage. It helps in comparing how coverage estimates vary
#' depending on the denominator, aiding in the assessment of data completeness and consistency.
#'
#' @examples
#' \dontrun{
#'   # Plot for the "penta3" indicator in the year 2021 with a survey coverage of 90
#'   plot.cd_indicator_coverage(data, indicator = "penta3", survey_coverage = 90, year = 2021)
#'
#'   # Plot for the "bcg" indicator with default survey coverage and year 2020
#'   plot.cd_indicator_coverage(data, indicator = "bcg", year = 2020)
#' }
#'
#' @import dplyr ggplot2
#' @export
plot_absolute_differences <- function(.data,
                                      indicator = c('penta3',"measles1",'bcg'),
                                      survey_coverage = 88,
                                      coverage_year = 2021) {

  country = year = category = name = indicator_name = value = NULL

  # Match the selected indicator to ensure it is valid
  indicator <- arg_match(indicator)

  # Check if the year is a valid integer
  if (!is_scalar_integerish(coverage_year)) {
    stop("Year must be a single integer value.")
  }

  # Prepare the data for plotting
  plot_data <- .data %>%
    pivot_longer(-any_of(c('country', 'year', 'iso3'))) %>%
    mutate(
      category = case_when(
        grepl('_dhis2$', name) ~ 'DHIS2 projection',
        grepl('_anc1$', name) ~ 'ANC1-derived',
        grepl('_penta1$', name) ~ 'Penta1-derived',
        grepl('_un$', name) ~ 'UN projection',
        grepl('_penta1derived$', name) ~ 'DPT1 Calculated'
      ),
      category = factor(category, levels = c('DHIS2 projection', 'ANC1-derived', 'Penta1-derived', 'UN projection', 'DPT1 Calculated')),
      indicator_name = str_extract(name, "(?<=cov_).*?(?=_)")
    ) %>%
    filter(year == coverage_year, indicator_name == indicator)

  # Auto-generate title based on the selected indicator
  title_text <- paste("Fig 2c:", indicator, "coverage, DHIS2-based with different denominators, and survey coverage")

  # Plot
  ggplot(plot_data, aes(x = category, y = value)) +
    # Define bar plot with specific fill color
    geom_col(aes(color = "Facility-based coverage (%)"), fill = 'darkgoldenrod3', width = 0.6) +

    # Add horizontal line for survey coverage, mapped to color aesthetic
    geom_hline(aes(yintercept = survey_coverage, color = "Coverage survey, national"), size = 1) +

    labs(
      title = title_text,
      x = NULL, y = "Coverage (%)"
    ) +

    # Customize y-axis labels
    scale_y_continuous(labels = scales::number_format()) +

    # Define colors for both elements in the legend
    scale_color_manual(
      values = c("Facility-based coverage (%)" = "darkgoldenrod3", "Coverage survey, national" = "darkgreen"),
      name = "Coverage Type"
    ) +

    # Apply custom theme
    cd_plot_theme()

}
