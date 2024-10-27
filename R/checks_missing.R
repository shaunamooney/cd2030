#' Calculate Percentage of Non-Missing Values by Year
#'
#' This function calculates the yearly percentage of non-missing values for a given set of indicators.
#' It assesses the availability of data by examining columns with prefixes of `mis_`, which represent
#' indicators for missing values across various data points. For each year, the function computes the
#' mean percentage of non-missing values for all indicators, resulting in a summary of data completeness
#' over time.
#'
#' @param .data A `cd_data` object containing a `quality_metrics` attribute, which is a data frame that
#'   includes columns indicating missing values, prefixed with `mis_`.
#' @return A `cd_missing_summary` object that includes a data frame summarizing the yearly percentages of
#'   non-missing values for each indicator, as well as an average percentage of non-missing values across
#'   all indicators for each year.
#'
#' @examples
#' \dontrun{
#'   # Calculate the non-missing value percentages by year for the provided data
#'   check_no_missing_year(data)
#' }
#' @export
check_missing_summary <- function(.data) {

  year = . = NULL

  check_cd_data(.data)

  # Calculate percentage of non-missing data by year
  mis_summary_by_year <- .data$quality_metrics %>%
    summarise(
      across(starts_with('mis_'), ~ mean(1 - .x, na.rm = TRUE) * 100),
      .by = year
    ) %>%
    mutate(mis_mean = rowMeans(select(., starts_with('mis_')), na.rm = TRUE)) %>%
    mutate(across(starts_with('mis_'), round, 2))

  new_tibble(
    mis_summary_by_year,
    class = 'cd_missing_summary'
  )
}

#' Summary for `cd_missing_summary`
#'
#' This function provides a custom summary for `cd_missing_summary` objects, offering an overview
#' of the percentage of monthly values that are non-missing for each indicator, aggregated by year.
#' The summary output helps users quickly understand data completeness across multiple indicators
#' and time points, giving insight into potential gaps in data collection.
#'
#' @param x A `cd_missing_summary` object containing yearly data completeness metrics.
#' @param ... Additional arguments for compatibility with S3 methods.
#'
#' @return A character vector that summarizes the data content and purpose, specifically indicating
#'   the proportion of values that are complete (non-missing) across years.
#'
#' @examples
#' \dontrun{
#'   # Generate a quick summary of non-missing data percentage by year
#'   tbl_sum(cd_missing_summary_object)
#' }
#'
#' @export
tbl_sum.cd_missing_summary <- function(x, ...) {
  c(
    'Table' = 'Percentage of monthly values that are not missing, by year',
    NextMethod()
  )
}


#' Check for Missing Values by District and Year
#'
#' This function calculates the percentage of districts with no missing values
#'   for each year across all indicators.
#'
#' @param .data A data frame containing missing value indicators for each district and year.
#' @return A tibble summarizing the percentage of districts with no missing values by year.
#' @examples
#' \dontrun{
#'   # Example usage
#'   check_missing_district(data)
#' }
#' @export
check_missing_district <- function(.data) {

  year = district = . = NULL

  check_cd_data(.data)

  districts_no_missing <- .data$quality_metrics %>%
    # Summarize missing values by district and year
    summarise(
      across(starts_with('mis_'), mean, na.rm = TRUE),
      .by = c(district, year)
    ) %>%
    # Calculate percentage of districts with no missing values per year
    summarise(
      across(starts_with('mis_'), ~ mean(1 - .x, na.rm = TRUE) * 100),
      .by = year
    ) %>%
    # Calculate average of missing values indicators for each year
    mutate(mis_mean = rowMeans(select(., starts_with('mis_')), na.rm = TRUE)) %>%
    # Round to two decimal places for readability
    mutate(across(starts_with('mis_'), round, 2))

  new_tibble(
    districts_no_missing,
    class = 'cd_missing_district'
  )
}

#' Summary for `cd_missing_district`
#'
#' Provides a custom summary for the `cd_missing_district` object, displaying the
#' percentage of districts with complete (non-missing) data, aggregated by year.
#'
#' @param x A `cd_missing_district` object containing yearly data completeness metrics.
#' @param ... Additional arguments for compatibility with S3 methods.
#'
#' @return A character vector that summarizes the data content and purpose,
#' indicating the proportion of values that are complete (non-missing) across districts and years.
#' @examples
#' \dontrun{
#'   # Generate a quick summary of non-missing data percentage by year
#'   tbl_sum(cd_missing_district_object)
#' }
#'
#' @export
tbl_sum.cd_missing_district <- function(x, ...) {
  c(
    'Table' = 'Percentage of districts with no missing values, by year',
    NextMethod()
  )
}
