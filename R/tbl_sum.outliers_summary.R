#' Summary for `cd_outliers_summary`
#'
#' Provides a custom summary for the `cd_outliers_summary` object, displaying
#' the percentage of monthly indicator values not considered extreme outliers, aggregated by year.
#' This summary is essential for assessing data quality and trends across indicators.
#'
#' @param x A `cd_outliers_summary` object containing aggregated data quality metrics for monthly values.
#' @param ... Additional arguments for compatibility with S3 methods.
#'
#' @return A character vector summarizing the content and purpose of the data,
#' indicating the proportion of values meeting the criteria for non-outliers by year.
#'
#' @export
tbl_sum.cd_outliers_summary <- function(x, ...) {
  c(
    'Table 2a' = 'Percentage of monthly values that are not extreme outliers, by year',
    NextMethod()
  )
}

#' Summary for `cd_district_outliers_summary`
#'
#' Provides a summary for the `cd_district_outliers_summary` object, indicating the percentage of districts
#' without extreme outliers for each indicator by year. This summary supports monitoring data quality across
#' districts by showing the proportion of non-extreme values across all indicators.
#'
#' @param x A `cd_district_outliers_summary` object containing the percentage of districts without extreme outliers
#'   for each indicator and year.
#' @param ... Additional arguments for compatibility with S3 methods.
#'
#' @return A character vector describing the purpose and content of the data, highlighting the percentage of districts
#'   with no extreme outliers for each indicator annually.
#'
#' @export
tbl_sum.cd_district_outliers_summary <- function(x, ...) {
  c(
    'Table 2b' = 'Percentage of districts with no extreme outliers, by year',
    NextMethod()
  )
}

