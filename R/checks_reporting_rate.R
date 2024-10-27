#' Check Average Reporting Rate by Year
#'
#' Calculates the average reporting rate for each indicator across all years in the dataset.
#' This function produces a data frame with annual averages for each reporting indicator
#' and a yearly mean of reporting rates.
#'
#' @param .data A data frame of class `cd_data` containing reporting rate information by year.
#'
#' @return A data frame of class `cd_average_reporting_rate`, containing average reporting rates
#'   for each indicator and a calculated mean reporting rate for each year.
#' @examples
#' \dontrun{
#'   # Example usage of the check_average_reporting_rate function
#'   avg_report_rate <- check_average_reporting_rate(data)
#' }
#' @export
check_average_reporting_rate <- function(.data) {

  year = . = NULL

  check_cd_data(.data)

  reporting_rate <- .data$merged_data %>%
    summarise(across(ends_with('_rr'), mean, na.rm = TRUE), .by = year) %>%
    mutate(mean_rr = rowMeans(select(., ends_with('_rr')), na.rm = TRUE)) %>%
    mutate(across(ends_with('_rr'), round, 1))

  new_tibble(
    reporting_rate,
    class = 'cd_average_reporting_rate'
  )
}

#' Summary for cd_average_reporting_rate
#'
#' Provides a custom summary for the `cd_average_reporting_rate` object, displaying
#' a message indicating the data shows average reporting rates by year for all indicators.
#'
#' @param x A `cd_average_reporting_rate` object containing reporting rate data.
#' @param ... Additional arguments for compatibility with S3 method.
#' @return A character vector summarizing the content and purpose of the data.
#' @export
tbl_sum.cd_average_reporting_rate <- function(x, ...) {
  c(
    'Table' = 'Average reporting rates for all indicators, by year',
    NextMethod()
  )
}

#' Check District-Level Reporting Rates by Year
#'
#' Calculates the percentage of districts with reporting rates above a defined threshold
#' for each year across multiple indicators. The output summarizes the percentage of districts
#' meeting the threshold for each indicator, along with a mean reporting rate.
#'
#' @param .data A data frame of class `cd_data` containing district-level reporting rates.
#' @param threshold A numeric value defining the minimum reporting rate threshold (default is 90).
#'
#' @return A data frame of class `cd_district_reporting_summary`, containing the percentage of districts
#'   with reporting rates above the threshold for each indicator and a calculated mean reporting rate.
#' @examples
#' \dontrun{
#'   # Calculate district-level reporting rates with a threshold of 90%
#'   district_summary <- check_district_reporting_summary(data, threshold = 90)
#' }
#' @export
check_district_reporting_summary <- function(.data, threshold = 90) {

  year = district = . = NULL

  check_cd_data(.data)

  reporting_rate <- .data$merged_data %>%
    summarise(across(ends_with('_rr'), mean, na.rm = TRUE), .by = c(district, year)) %>%
    summarise(across(ends_with('_rr'), ~ mean(.x >= threshold, na.rm = TRUE) * 100, .names = 'low_{.col}'), .by = year) %>%
    mutate(low_mean_rr = rowMeans(select(., starts_with('low_')), na.rm = TRUE)) %>%
    mutate(across(starts_with('low_'), round, 1))

  new_tibble(
    reporting_rate,
    class = 'cd_district_reporting_summary',
    threshold = threshold
  )
}

#' Summary for cd_district_reporting_summary
#'
#' Custom summary for district-level reporting rates, displaying a message with
#' the threshold used for calculating reporting rates by year.
#'
#' @param x A `cd_district_reporting_summary` object with district-level reporting rates.
#' @param ... Additional arguments for compatibility with S3 method.
#'
#' @return A character vector summarizing the reporting threshold used.
#' @export
tbl_sum.cd_district_reporting_summary <- function(x, ...) {
  threshold <- attr(x, "threshold")
  c(
    'Table' = paste0('Percentage of districts with reporting rates >= ', threshold, ', by year'),
    NextMethod()
  )
}

#' Plot District Reporting Rate Summary
#'
#' Generates a bar plot displaying the percentage of districts with reporting rates below
#' the defined threshold for multiple indicators across various years.
#'
#' @param x A `cd_district_reporting_summary` data frame containing reporting rate data,
#'   processed by [check_district_reporting_summary()].
#' @param ... Additional parameters passed to the plotting function.
#' @return A ggplot object visualizing reporting rates across indicators and years.
#' @examples
#' \dontrun{
#'   # Generate a plot of district reporting rates below a threshold of 90%
#'   plot(cd_district_reporting_summary(data), threshold = 90)
#' }
#' @export
plot.cd_district_reporting_summary <- function(x, ...) {

  year = value = indicator = low_mean_rr = NULL

  threshold <- attr(x, "threshold")

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
      geom_text(aes(label = round(value, 1)), position = position_dodge(width = 0.9), vjust = -0.25, color = 'black', size = 3) +
      facet_wrap(~title, scales = 'free_y', nrow = 2, ncol = 3) +
      labs(
        title = paste("Percentage of districts with low reporting rate (<", threshold, "%) by service and by year"),
        x = 'Year', y = 'Percentage'
      ) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 10, hjust = 0.5),
        axis.text = element_text(size = 8)
      ) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
      scale_fill_manual(values = c('2019' = 'darkgreen', '2020' = 'darkorange', '2021' = 'darkblue', '2022' = 'red', '2023' = 'green')) +
      guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
      labs(caption = paste("Low reporting rate (<", threshold, "%)"))
}
