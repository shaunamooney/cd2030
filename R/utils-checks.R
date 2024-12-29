#' Calculate Quality Metrics for Health Indicators
#'
#' `calculate_quality_metrics` assesses data quality for various health indicators
#' across districts and years. It includes outlier detection based on Hampel's X84
#' method and flags for missing values to ensure comprehensive data quality checks.
#'
#' @param .data A data frame containing health indicator data with columns for
#'   district, year, and each indicator in the predefined groups.
#' @param call The calling environment for error handling. Defaults to `caller_env()`.
#'
#' @details
#' - **Indicator Groups**: The function expects indicators grouped into categories,
#'    e.g., ANC, IDelivery, Vaccination, to calculate MAD and median values up to
#'    the latest year (`lastyear`) for each indicator.
#' - **Outlier Detection**: Outliers are identified using Hampel's X84 method,
#'    where bounds are defined as 5 standard deviations from the median (scaled
#'    by `1.4826` for robust MAD estimation).
#' - **Missingness Flag**: Values are flagged as "Missing" (1) or "Non-Missing"
#'    (0) in alignment with standard handling for quality assessment.
#'
#' @return A data frame (outliers_summary) containing:
#' - Outlier flags for each indicator, suffixed with `_outlier5std`.
#' - Missingness status for each indicator, prefixed with `mis_`.
#' - `district` and `year` columns to maintain grouping information.
#'
#' @examples
#' \dontrun{
#' # Assuming .data is a data frame with necessary indicator columns
#' quality_metrics <- calculate_quality_metrics(.data)
#' }
#'
#' @noRd
calculate_quality_metrics <- function(.data, call = caller_env()) {

  district = year = NULL

  check_cd_data(.data, call = call)

  indicator_groups <- attr(.data, 'indicator_groups')
  allindicators <- list_c(indicator_groups)

  # Get the latest year in the dataset
  lastyear <- robust_max(.data$year)

  outliers_summary <- .data %>%
    select(district, year, allindicators) %>%
    add_outlier5std_column(allindicators) %>%
    mutate(
      # Flag missing values
      across(allindicators, ~ if_else(is.na(.), 1,  0), .names = "mis_{.col}"),

      .by = district
    ) %>%
    select(district, year, ends_with('_outlier5std'), starts_with('mis_'))

  return(outliers_summary)
}

#' Robust Maximum Value Calculation
#'
#' `robust_max` calculates the maximum value of a numeric vector while handling
#' missing values. If all values in the vector are `NA`, the function returns `NA`
#' instead of raising an error.
#'
#' @param x A numeric vector.
#' @param fallback argument allows you to specify a value to return when all elements are NA
#'
#' @return The maximum value of `x`, or `NA` if all values are `NA`.
#'
#' @examples
#' # Example usage
#' robust_max(c(1, 2, 3, NA))  # Returns 3
#' robust_max(c(NA, NA))       # Returns NA
#' robust_max(c(-Inf, 0, 10))  # Returns 10
#'
#' @export
robust_max <- function(x, fallback = NA_real_) {
  if (all(is.na(x))) {
    # Return the fallback value if all elements are NA
    return(fallback)
  }

  # Return the maximum value, ignoring NA values
  max(x, na.rm = TRUE)  # Use base::max for consistency
}
