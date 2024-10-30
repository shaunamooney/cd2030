#' Summary for `cd_completeness_summary`
#'
#' This function provides a custom summary for `cd_completeness_summary` objects,
#' offering an overview of the percentage of data completeness (non-missing values)
#' for each indicator, aggregated by year. The summary output helps users quickly
#' identify data completeness trends across indicators and time points, giving insight
#' into potential gaps in data collection.
#'
#' @param x A `cd_completeness_summary` object containing yearly data completeness metrics.
#' @param ... Additional arguments for compatibility with S3 methods.
#'
#' @return A character vector that summarizes the data content and purpose,
#' specifically indicating the proportion of values that are complete across years.
#'
#' @examples
#' \dontrun{
#'   # Generate a summary of data completeness by year
#'   tbl_sum(cd_completeness_summary_object)
#' }
#'
#' @export
tbl_sum.cd_completeness_summary <- function(x, ...) {
  c(
    'Table 4a' = 'Percentage of monthly values with complete data, by year',
    NextMethod()
  )
}

#' Summary for `cd_district_completeness_summary`
#'
#' Provides a custom summary for the `cd_district_completeness_summary` object,
#' showing the percentage of districts with complete (non-missing) data aggregated
#' by year. This summary helps users understand the distribution of data completeness
#' at the district level across time.
#'
#' @param x A `cd_district_completeness_summary` object containing yearly district-level
#'   data completeness metrics.
#' @param ... Additional arguments for compatibility with S3 methods.
#'
#' @return A character vector that summarizes the data content and purpose,
#' indicating the percentage of districts with complete data across years.
#'
#' @examples
#' \dontrun{
#'   # Generate a summary of district-level data completeness by year
#'   tbl_sum(cd_district_completeness_summary_object)
#' }
#'
#' @export
tbl_sum.cd_district_completeness_summary <- function(x, ...) {
  c(
    'Table 4b' = 'Percentage of districts with complete data, by year',
    NextMethod()
  )
}
