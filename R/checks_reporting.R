#' Check Average Reporting Rate
#'
#' This function calculates the average reporting rate for each indicator across
#'   all years.
#'
#' @param data A data frame of class `cd_data`.
#' @return A data frame with the average reporting rates and a mean reporting rate
#'   for each year.
#' @examples
#' \dontrun{
#'
#'   check_average_reporting_rate(data)
#' }
#' @export
check_average_reporting_rate <- function(data) {

  year = district = . = NULL

  if (!inherits(data, 'cd_data')) stop('The data object must be of class "cd_data".')

  reporting_rate <- data$merged_data %>%
    summarise(across(ends_with('_rr'), mean, na.rm = TRUE), .by = year) %>%
    mutate(mean_rr = rowMeans(select(., ends_with('_rr')), na.rm = TRUE)) %>%
    mutate(across(ends_with('_rr'), round, 2))

  return(reporting_rate)
}

#' Check Reporting Rates by District
#'
#' This function calculates the percentage of districts reporting rates above a
#' defined threshold for each by year.
#'
#' @param data A data frame containing reporting rates for multiple districts
#'   and years.
#' @param threshold Threshold to calculate the reporting rates
#' @return A data frame with the percentage of districts with reporting rates
#'   above the threshold for each indicator.
#' @examples
#' \dontrun{
#'
#'   check_reporting_rates(data)
#' }
#' @export
check_reporting_rates <- function(data, threshold = 90) {

  year = district = . = NULL

  if (!inherits(data, 'cd_data')) stop('The data object must be of class "cd_data".')

  reporting_rate <- data$merged_data %>%
    # Step 1: Calculate the mean reporting rates by district and year
    summarise(across(ends_with('_rr'), ~ mean(as.numeric(.x), na.rm = TRUE)), .by = c(district, year)) %>%
    # Step 2: Calculate the percentage of districts with reporting rates above the threshold
    summarise(across(ends_with('_rr'), ~ (sum(.x >= threshold, na.rm = TRUE) / n()) * 100), .by = year) %>%
    # Step 3: Rename columns to have the 'low_' prefix
    rename_with(~ paste0('low_', .x), .cols = ends_with('_rr')) %>%
    # Step 4: Calculate the row mean of the 'low_' columns
    mutate(low_mean_rr = rowMeans(select(., starts_with('low')), na.rm = TRUE)) %>%
    # Step 5: Round all 'low_' columns and the row mean to 2 decimal places
    mutate(across(ends_with('_rr'), round, 2))

  new_tibble(
    reporting_rate,
    class = 'cd_reporting_rate'
  )
}

#' Plot Reporting Rate Summary
#'
#' This function creates a bar plot of reporting rates, showing the percentage
#'   of districts with reporting rates for different indicators over the years.
#'
#' @param data A data frame containing reporting rate data that has already been
#'   processed by [check_reporting_rates()].
#' @return A ggplot object displaying reporting rates across indicators and years.
#' @examples
#' \dontrun{
#'
#'   print_reporting_rate(data)
#' }
#' @export
print_reporting_rate <- function(data) {

  year = value = NULL

  if (!inherits(data, 'cd_reporting_rate')) stop('Reporting rate has not been checked.')

  # Invert the reporting rates and reshape for plotting
  data %>%
    mutate(across(starts_with('low_'), ~ 100 - ., .names = 'inv_{col}')) %>%
    pivot_longer(cols = starts_with('inv_low_'), names_to = 'indicator', values_to = 'value') %>%
    # Define indicator names and corresponding titles
    mutate(title = case_when(
      indicator == 'inv_low_anc_rr' ~ 'Antenatal Care',
      indicator == 'inv_low_idelv_rr' ~ 'Institutional Delivery',
      indicator == 'inv_low_vacc_rr' ~ 'Vaccination'
    )) %>%
    # Create the plot with facet_wrap
    ggplot(aes(x = year, y = value, fill = as.factor(year))) +
    geom_col(position = 'dodge') +
    geom_text(aes(label = round(value, 1)), position = position_dodge(width = 0.9), vjust = -0.25, color = 'black', size = 3) +
    facet_wrap(~title, scales = 'free_y') +
    labs(x = 'Year', y = 'Percentage') +
    theme_bw() +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
    scale_fill_manual(values = c('2019' = 'darkgreen', '2020' = 'darkorange', '2021' = 'darkblue', '2022' = 'red', '2023' = 'green'))
}
