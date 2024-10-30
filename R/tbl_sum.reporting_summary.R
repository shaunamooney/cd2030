#' Summary for Average Reporting Rates by Year
#'
#' Provides a custom summary for the `cd_average_reporting_rate` object, indicating
#' that the data represents annual average reporting rates across all indicators.
#' This summary is useful for quickly verifying the consistency of indicator reporting rates.
#'
#' @param x A `cd_average_reporting_rate` object containing reporting rate data by year.
#' @param ... Additional arguments for compatibility with the S3 method.
#'
#' @return A character vector describing the content and purpose of the
#'   `cd_average_reporting_rate` data.
#' @export
tbl_sum.cd_average_reporting_rate <- function(x, ...) {
  c(
    'Table 1a' = 'Annual average reporting rates for all indicators, by year',
    NextMethod()
  )
}


#' Summary for District-Level Reporting Rates by Year
#'
#' Provides a custom summary for the `cd_district_reporting_rate` object, displaying
#' a message with the threshold used in calculating district-level reporting rates by year.
#' This summary serves to validate which districts meet the defined threshold for reporting completeness.
#'
#' @param x A `cd_district_reporting_rate` object containing district-level reporting rates.
#' @param ... Additional arguments for compatibility with the S3 method.
#'
#' @return A character vector describing the reporting threshold applied and summarizing
#' district reporting rates by year.
#' @export
tbl_sum.cd_district_reporting_rate <- function(x, ...) {
  threshold <- attr(x, "threshold")
  c(
    'Table 1b' = paste0('District reporting rates meeting threshold >= ', threshold, ' by year'),
    NextMethod()
  )
}

