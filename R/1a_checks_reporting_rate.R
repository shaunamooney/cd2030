#' Reporting Rate Summary by Administrative Level
#'
#' `calculate_reporting_rate()` computes the average reporting rate for each indicator
#' across available years, disaggregated by the specified administrative level. It also
#' calculates an overall mean reporting rate per group.
#'
#' @param .data A data frame of class `cd_data` that contains reporting rate
#'   columns (e.g., `anc_rr`, `idelv_rr`, `vacc_rr`).
#' @param admin_level Character. The administrative level at which to calculate
#'   reporting rates. Must be one of `"national"`, `"adminlevel_1"` or `"district"`.
#'
#' @return A tibble of class `cd_reporting_rate`, containing:
#'   - One row per grouping (`year`, `adminlevel_1`, `district` as applicable).
#'   - One column per reporting indicator.
#'   - `mean_rr`: average of all indicators per group.
#'
#' @details
#' Use this function to monitor reporting completeness at national or subnational
#' levels. It supports trend analysis and program performance reviews in line
#' with Countdown 2030 guidelines.
#'
#' @examples
#' \dontrun{
#'   # National summary
#'   calculate_reporting_rate(data, admin_level = "national")
#'
#'   # Regional summary
#'   calculate_reporting_rate(data, admin_level = "adminlevel_1")
#'
#'   # District-level summary
#'   calculate_reporting_rate(data, admin_level = "district")
#' }
#'
#' @export
calculate_reporting_rate <- function(.data,
                                     admin_level = c('national', 'adminlevel_1', 'district')) {

  . = NULL

  check_cd_data(.data)
  admin_level <- arg_match(admin_level)

  admin_level_cols <- switch(
    admin_level,
    national = 'year',
    adminlevel_1 = c('adminlevel_1', 'year'),
    district = c('adminlevel_1', 'district', 'year')
  )

  indicators <- paste0(get_indicator_group_names(), '_rr')

  reporting_rate <- .data %>%
    summarise(across(all_of(indicators), mean, na.rm = TRUE), .by = admin_level_cols) %>%
    mutate(mean_rr = rowMeans(select(., indicators), na.rm = TRUE)) %>%
    mutate(across(ends_with('_rr'), round, 0))

  new_tibble(
    reporting_rate,
    class = 'cd_reporting_rate',
    admin_level = admin_level
  )
}

#' District-Level Reporting Rates by Year
#'
#' `calculate_district_reporting_rate` calculates the percentage of districts that
#' meet or exceed a specified reporting rate threshold for each indicator, by year.
#' Also computes an overall yearly mean.
#'
#'
#' @param .data A `cd_data` object containing district-level reporting rate columns.
#' @param threshold Numeric. Minimum acceptable reporting rate (as a percentage).
#'   Default: 90.
#'
#' @return A tibble of class `cd_district_reporting_rate` with:
#'   - One row per year.
#'   - Columns `low_<indicator>`: percentage of districts meeting the threshold.
#'   - `low_mean_rr`: mean percentage of districts meeting the threshold across
#'      indicators.
#'
#' @details
#' This function supports subnational data quality monitoring by identifying the
#' proportion of districts with sufficiently complete reporting. It enables
#' year-over-year comparisons and helps target underperforming regions.
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

  indicators <- paste0(get_indicator_group_names(), '_rr')

  reporting_rate <- .data %>%
    summarise(across(all_of(indicators), mean, na.rm = TRUE), .by = c(district, year)) %>%
    summarise(across(all_of(indicators), ~ mean(.x >= threshold, na.rm = TRUE) * 100, .names = 'low_{.col}'), .by = year) %>%
    mutate(low_mean_rr = rowMeans(select(., starts_with('low_')), na.rm = TRUE)) %>%
    mutate(across(starts_with('low_'), round, 0))

  new_tibble(
    reporting_rate,
    class = 'cd_district_reporting_rate',
    threshold = threshold
  )
}
