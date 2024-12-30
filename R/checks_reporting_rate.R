#' Calculate Average Reporting Rate by Year
#'
#' `calculate_average_reporting_rate` computes the average reporting rate for
#' each indicator across all years available in the dataset. This function
#' generates a tibble with annual averages for each reporting indicator and
#' an overall mean reporting rate per year.
#'
#' @param .data A data frame of class `cd_data` that contains reporting rate
#'   information by year.
#'
#' @return A data frame of class `cd_average_reporting_rate`, which includes
#'   average reporting rates for each indicator along with an overall mean
#'   reporting rate for each year.
#'
#' @details This function produces a summary of reporting completeness across
#'   multiple indicators. The results provide insights on the annual average
#'   completeness and can help identify trends over time in facility reporting,
#'   supporting routine data quality assessments as recommended in Countdown 2030.
#'
#' @examples
#' \dontrun{
#'   # Calculate average reporting rate by year
#'   avg_report_rate <- calculate_average_reporting_rate(data)
#' }
#'
#' @export
calculate_average_reporting_rate <- function(.data) {

  year = . = NULL

  check_cd_data(.data)

  indicator_groups <- attr(.data, 'indicator_groups')
  indicator_groups <- paste0(names(indicator_groups), '_rr')

  reporting_rate <- .data %>%
    summarise(across(indicator_groups, mean, na.rm = TRUE), .by = year) %>%
    mutate(mean_rr = rowMeans(select(., indicator_groups), na.rm = TRUE)) %>%
    mutate(across(ends_with('_rr'), round, 0))

  new_tibble(
    reporting_rate,
    class = 'cd_average_reporting_rate'
  )
}

#' Calculate District-Level Reporting Rates by Year
#'
#' `calculate_district_reporting_rate` calculates the percentage of districts with
#' reporting rates above a specified threshold for each year across various indicators.
#' The function provides a summary of the percentage of districts meeting the threshold
#' for each indicator and includes a mean reporting rate for each year.
#'
#' @param .data A data frame of class `cd_data` containing district-level
#'   reporting rates.
#' @param threshold A numeric value representing the minimum acceptable reporting
#'   rate threshold (default is 90).
#'
#' @return A data frame of class `cd_district_reporting_rate`, containing the
#'   percentage of districts with reporting rates above the specified threshold
#'   for each indicator and a mean reporting rate.
#'
#' @details This function supports subnational data quality assessments by
#' identifying districts with low reporting completeness for defined indicators.
#' Calculating the percentage of districts with acceptable reporting rates
#' provides insight into data quality gaps and helps target data quality
#' improvements at the district level, as outlined in Countdown 2030 guidance.
#'
#' @examples
#' \dontrun{
#'   # Calculate district-level reporting rates with a threshold of 90%
#'   district_summary <- calculate_district_reporting_rate(data, threshold = 90)
#' }
#'
#' @export
calculate_district_reporting_rate <- function(.data, threshold = 90) {

  year = district = . = NULL

  check_cd_data(.data)

  indicator_groups <- attr(.data, 'indicator_groups')
  indicator_groups <- paste0(names(indicator_groups), '_rr')

  reporting_rate <- .data %>%
    summarise(across(indicator_groups, mean, na.rm = TRUE), .by = c(district, year)) %>%
    summarise(across(indicator_groups, ~ round(mean(.x >= threshold, na.rm = TRUE) * 100, 1), .names = 'low_{.col}'), .by = year) %>%
    mutate(
      low_mean_rr = round(rowMeans(select(., starts_with('low_')), na.rm = TRUE), 0)
    )

  new_tibble(
    reporting_rate,
    class = 'cd_district_reporting_rate',
    threshold = threshold
  )
}

#' Identify Districts with Low Reporting Rates
#'
#' `district_low_reporting` identifies districts that have reporting rates below
#' a specified threshold.
#'
#' @param .data A data frame of class `cd_data` that contains reporting rate
#'   information by year.
#' @param threshold Numeric. The reporting rate threshold below which districts
#'   will be flagged. Defaults to 90.
#'
#' @return A data frame or tibble containing the districts and years where
#'   reporting rates for any indicator group fall below the specified threshold.
#'
#' @examples
#' \dontrun{
#'   # Calculate district-level reporting rates with a threshold of 90%
#'   district_summary <- district_low_reporting(data, threshold = 90)
#' }
#'
#' @export
district_low_reporting <- function(.data, threshold = 90) {

  district = year = NULL

  check_cd_data(.data)

  indicator_groups <- attr(.data, 'indicator_groups')
  indicator_groups <- paste0(names(indicator_groups), '_rr')

  reporting_rate <- .data %>%
    summarise(across(all_of(indicator_groups), mean, na.rm = TRUE), .by = c(district, year)) %>%
    filter(if_any(ends_with("rr"), ~ .x < threshold))

  return(reporting_rate)
}
