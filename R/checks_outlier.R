#' Check for Outliers in Indicator Data
#'
#' This function identifies and quantifies extreme outliers for a specified set of indicators in a dataset,
#' flagging them if values fall beyond five times the Median Absolute Deviation (MAD) from the median.
#' It returns the percentage of non-outliers for each indicator by year, reflecting data quality trends.
#'
#' @param .data A data frame containing indicator data for districts and years. The data frame should be processed
#'   with calculated outlier flags (columns ending in `_outlier5std`), where 1 indicates an outlier and 0 a non-outlier.
#'
#' @details
#' The function operates as follows:
#' - Collapses the data by year, calculating the mean percentage of non-outliers for each indicator.
#' - Aggregates indicators by calculating the mean percentage of non-outliers across all indicators to provide
#'   an annual summary.
#'
#' The function uses the Median Absolute Deviation (MAD) method for robust outlier detection, ensuring that only
#' extreme deviations from the median are flagged.
#'
#' @return A `cd_outliers_summary` object (data frame) containing the yearly summary of non-outliers as a
#'   percentage, with each indicator reported individually and a calculated overall mean.
#'
#' @examples
#' \dontrun{
#'   # Check for extreme outliers in indicator data
#'   check_outliers(data)
#' }
#'
#' # Output: Percentage of monthly values that are not extreme outliers, by year
#'
#' @export
check_outliers <- function(.data) {

  year = . = NULL

  check_cd_data(.data)

  outliers_summary <- .data$quality_metrics %>%
    # Collapse data by year and calculate mean percentage of non-outliers
    summarise(
      across(ends_with('_outlier5std'), ~ mean(1 - .x, na.rm = TRUE) * 100),
      .by = year
    ) %>%
    mutate(mean_outlier5std = round(rowMeans(select(., ends_with('_outlier5std')), na.rm = TRUE), 1))

  new_tibble(
    outliers_summary,
    class = 'cd_outliers_summary'
  )
}

#' Summary for `cd_outliers_summary`
#'
#' Provides a custom summary for the `cd_outliers_summary` object, displaying
#' the percentage of monthly indicator values that are not considered extreme outliers, aggregated by year.
#'
#' @param x A `cd_outliers_summary` object containing data quality metrics.
#' @param ... Additional arguments for compatibility with S3 methods.
#'
#' @return A character vector summarizing the content and purpose of the data,
#' indicating the proportion of values that meet the criteria for non-outliers.
#'
#' @export
tbl_sum.cd_outliers_summary <- function(x, ...) {
  c(
    'Table' = 'Percentage of monthly values that are not extreme outliers, by year',
    NextMethod()
  )
}

#' Check for Extreme Outliers at District Level
#'
#' This function identifies and aggregates extreme outlier counts for various indicators
#' by calculating the maximum outlier flag for each indicator within each district and year.
#' An extreme outlier for an indicator in a given district is flagged when the value lies
#' more than five standard deviations from the median, based on the Hampel X84 method.
#' This function outputs the percentage of districts without extreme outliers per indicator,
#' summarized by year.
#'
#' @param .data A data frame containing the processed indicator data with outlier flags (e.g., `*_outlier5std`).
#'
#' @return A `cd_extreme_outliers` object summarizing the percentage of districts without extreme
#'   outliers by year. The output includes each indicator's percentage and an overall yearly mean
#'   percentage for non-outliers across all indicators.
#'
#' @examples
#' \dontrun{
#'   # Summarize the proportion of districts without extreme outliers
#'   check_extreme_outlier(data)
#' }
#'
#' @export
check_extreme_outlier <- function(.data) {

  district = year = . = NULL

  check_cd_data(.data)

  outliers_summary <- .data$quality_metrics %>%
    summarise(
      across(
        ends_with('_outlier5std'),
        ~ ifelse(all(is.na(.x)), NA_real_, max(.x, na.rm = TRUE))
      ),
      .by = c(district, year)
    ) %>%
    summarise(
      across(ends_with('_outlier5std'), ~ round((1 - mean(.x, na.rm = TRUE)) * 100, 0.01)),
      .by = year
    ) %>%
    # Add a mean row for district data
    mutate(
      mean_outlier5std = round(rowMeans(select(., -year), na.rm = TRUE), 2)
    )

  new_tibble(
    outliers_summary,
    class = 'cd_extreme_outliers'
  )
}

#' Summary for `cd_extreme_outliers`
#'
#' Provides a summary of the `cd_extreme_outliers` object, indicating the percentage of districts with no
#' extreme outliers by year for each indicator. This summary facilitates data quality monitoring by showing
#' the proportion of monthly values that are non-extreme for all indicators.
#'
#' @param x A `cd_extreme_outliers` object containing the percentage of districts without extreme outliers
#'   by year and indicator.
#' @param ... Additional arguments for compatibility with S3 methods.
#'
#' @return A character vector describing the purpose of the data, highlighting the percentage of districts
#'   with no extreme outliers for each year.
#'
#' @export
tbl_sum.cd_extreme_outliers <- function(x, ...) {
  c(
    'Table' = 'Percentage of districts with no extreme outliers, by year',
    NextMethod()
  )
}
