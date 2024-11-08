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
  lastyear <- max(.data$year, na.rm = TRUE)

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

add_outlier5std_column <- function(.data, indicators) {

  district = NULL

  check_cd_data(.data, call = call)
  check_required(indicators)

  last_year <- max(.data$year, na.rm = TRUE)

  .data %>%
    mutate(
      # Step 2: Calculate outlier flags based on bounds
      across(
        all_of(indicators),
        ~ {
          mad <-  mad(if_else(year < last_year, ., NA_real_), na.rm = TRUE)
          med <-  median(if_else(year < last_year, ., NA_real_), na.rm = TRUE)

          med <- if_else(
            is.na(med),
            if (all(is.na(med))) NA_real_ else max(med, na.rm = TRUE),
            med
          )
          mad <- if_else(
            is.na(mad),
            if (all(is.na(mad))) NA_real_ else max(mad, na.rm = TRUE),
            mad
          )

          lower_bound <- round(med - 5 * mad, 1)
          upper_bound <- round(med + 5 * mad, 1)

          if_else(!is.na(.) & (. < lower_bound | . > upper_bound), 1, 0)
        },
        .names = '{.col}_outlier5std'
      ),

      .by = district
    )
}
