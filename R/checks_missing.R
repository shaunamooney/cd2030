#' Calculate Percentage of Non-Missing Values by Year
#'
#' `calculate_completeness_summary` computes the yearly percentage of non-missing
#' values across specified indicators, summarizing data completeness for each year.
#' Indicators with missing value flags are identified by a `mis_` prefix, representing
#' the completeness of various indicators for each year.
#'
#' This function aggregates the percentage of non-missing values for each indicator
#' and calculates the overall average completeness across all indicators. This
#' provides a comprehensive view of data availability trends over time.
#'
#' @param .data A `cd_data` object containing indicator data, including columns with
#'   the `mis_` prefix which represent missing value flags for each indicator.
#' @return A `cd_missing_summary` object containing a tibble. This tibble shows
#'   the yearly percentages of non-missing values for each indicator and the average
#'   percentage of non-missing values across all indicators per year.
#'
#' @details
#' The function computes non-missing percentages as follows:
#' - For each indicator, calculates the mean percentage of non-missing values by year.
#' - Calculates additional summaries specifically for vaccination and tracer indicators.
#' - Rounds results to two decimal places to maintain readability.
#'
#' @examples
#' \dontrun{
#'   # Calculate the percentage of non-missing values by year for the data
#'   calculate_completeness_summary(data)
#' }
#' @export
calculate_completeness_summary <- function(.data) {

  year = . = NULL

  check_cd_data(.data)

  indicator_groups <- attr(.data, 'indicator_groups')
  vaccine_only <- indicator_groups[['vacc']]
  tracers <- attr(.data, 'tracers')
  allindicators <- list_c(indicator_groups)

  data <- .data %>%
    add_missing_column(allindicators) %>%
    summarise(across(starts_with('mis_'), mean, na.rm = TRUE), .by = year) %>%
    mutate(
      mean_mis_all = rowMeans(select(., any_of(starts_with('mis_'))), na.rm = TRUE),
      mean_mis_vacc_only = rowMeans(select(.,  any_of(paste0('mis_', vaccine_only))), na.rm = TRUE),
      mean_mis_vacc_tracer = rowMeans(select(.,  any_of(paste0('mis_', tracers))), na.rm = TRUE),

      across(c(starts_with('mis_'), starts_with('mean_mis_')), ~ round((1 - .x) * 100, 2))
    )

  new_tibble(
    data,
    class = 'cd_completeness_summary'
  )
}

#' Check for Missing Values by District and Year
#'
#' `calculate_district_completeness_summary` calculates the percentage of districts
#' with complete data (no missing values) for each year. By assessing missingness
#' across all districts for each indicator, it provides a summary of data completeness
#' on a per-district basis.
#'
#' @param .data A `cd_data` object containing indicator data with columns prefixed by
#'   `mis_`, indicating missing values for each district and year.
#' @return A `cd_district_completeness_summary` object containing a tibble. This
#'   tibble summarizes the percentage of districts with no missing values for each
#'   indicator per year, as well as an overall yearly summary across all indicators.
#'
#' @details
#' - For each year, the function calculates the percentage of districts without missing
#'   values per indicator.
#' - The output includes specific summary statistics for key indicator groups, such as
#'   vaccination indicators and tracer indicators, providing insights into data
#'   completeness at a granular level.
#' - All percentages are rounded to two decimal places.
#'
#' @examples
#' \dontrun{
#'   # Calculate the percentage of districts with complete data per year
#'   calculate_district_completeness_summary(data)
#' }
#' @export
calculate_district_completeness_summary <- function(.data) {

  year = district = . = NULL

  check_cd_data(.data)

  indicator_groups <- attr(.data, 'indicator_groups')
  vaccine_only <- indicator_groups[['vacc']]
  tracers <- attr(.data, 'tracers')
  allindicators <- list_c(indicator_groups)

  data <- .data %>%
    add_missing_column(allindicators) %>%
    summarise(across(starts_with('mis_'), mean, na.rm = TRUE), .by = c(year, district)) %>%
    summarise(across(starts_with('mis_'), ~ mean(.x != 0, na.rm = TRUE)), .by = year) %>%
    mutate(
      mean_mis_all = rowMeans(select(., any_of(starts_with('mis_'))), na.rm = TRUE),
      mean_mis_vacc_only = rowMeans(select(.,  any_of(paste0('mis_', vaccine_only))), na.rm = TRUE),
      mean_mis_vacc_tracer = rowMeans(select(.,  any_of(paste0('mis_', tracers))), na.rm = TRUE),

      across(c(starts_with('mis_'), starts_with('mean_mis_')), ~ round((1 - .x) * 100, 2))
    )

  new_tibble(
    data,
    class = 'cd_missing_district'
  )
}
