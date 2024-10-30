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
    mutate(
      # Calculate MAD and median for each indicator up to `lastyear`
      across(allindicators, ~ mad(if_else(year < lastyear, ., NA_real_), na.rm = TRUE), .names = "{.col}_mad"),
      across(allindicators, ~ median(if_else(year < lastyear, ., NA_real_), na.rm = TRUE), .names = "{.col}_med"),

      # Replace NA MAD and median values with max MAD or median within district
      # across(ends_with('_mad'), ~ if_else(is.na(.), max(., na.rm = TRUE), round(., 1))),
      # across(ends_with('_med'), ~ if_else(is.na(.), max(., na.rm = TRUE), round(., 1))),

      # Define Hampel X84 method bounds for outliers
      across(allindicators, ~ round(get(paste0(cur_column(), "_med")) - 5 * get(paste0(cur_column(), "_mad")), 0), .names = "{.col}_outlb5std"),
      across(allindicators, ~ round(get(paste0(cur_column(), "_med")) + 5 * get(paste0(cur_column(), "_mad")), 0), .names = "{.col}_outub5std"),

      # Flag values as outliers if they fall outside the calculated bounds
      across(allindicators, ~ if_else(!is.na(.) & (. < get(paste0(cur_column(), "_outlb5std")) | . > get(paste0(cur_column(), "_outub5std"))), 1, 0), .names = "{.col}_outlier5std"),

      # Flag missing values
      across(allindicators, ~ if_else(is.na(.), 1,  0), .names = "mis_{.col}"),

      .by = district
    ) %>%
    select(district, year, ends_with('_outlier5std'), starts_with('mis_'))

  return(outliers_summary)
}

#' Calculate Indicator Ratios and Adequacy Flags
#'
#' Computes ratios for specified pairs of indicators dynamically and flags whether
#' each calculated ratio falls within an adequate range, per district and year.
#' This function is an intermediate utility used to standardize ratio calculations
#' and adequacy checks across indicator pairs.
#'
#' @param .data A data frame of type `cd_data`.
#' @param ratio_pairs A named list of indicator pairs for which ratios should be
#'   calculated.
#'   - Each pair should be a character vector of length two, with the first
#'     element as the numerator and the second as the denominator. Default is:
#'     `list("ratioAP" = c("anc1", "penta1"), "ratioPP" = c("penta1", "penta3"), "ratioOO" = c("opv1", "opv3"))`.
#'   - Each name in the list becomes the name of the resulting ratio column.
#' @param adequate_range A numeric vector of length two specifying the inclusive
#'   lower and upper bounds for the adequate range, typically between `1` and
#'   `1.5`. Ratios within this range are flagged as adequate.
#'
#' @return A data frame with columns for:
#' - **Summed indicators by district and year**: Summed values for each indicator
#'    across districts within the specified `year`, based on input in `ratio_pairs`.
#' - **Ratios**: Each ratio is calculated as the sum of the specified numerator
#'    divided by the sum of the specified denominator for each pair.
#' - **Adequacy flags**: For each ratio, an adequacy flag (e.g., `adeq_ratioAP`)
#'    is generated, set to `1` if the ratio falls within the `adequate_range`,
#'    and `0` otherwise.
#' - The output is aggregated by year and includes means of ratios and adequacy
#'    flags.
#'
#' @details
#' This function standardizes ratio calculations by dynamically computing ratios
#' and adequacy flags for specified pairs of indicators. It operates in three
#' main stages:
#' 1. **Summing Indicators**: Calculates the sum of each indicator by `district`
#'    and `year`.
#' 2. **Calculating Ratios**: Divides the summed numerator by the summed
#'    denominator for each pair in `ratio_pairs`, creating a column for each ratio.
#' 3. **Checking Adequacy**: Flags each ratio as adequate if it falls within the
#'    specified `adequate_range`, returning `1` for adequate and `0` otherwise.
#'
#' @examples
#' \dontrun{
#'   # Calculate ratios and adequacy checks using custom ranges
#'   calculate_ratios_and_adequacy(data,
#'                                 ratio_pairs = list("ratioAP" = c("anc1", "penta1"),
#'                                                    "ratioPP" = c("penta1", "penta3")),
#'                                 adequate_range = c(1, 1.5))
#' }
#'
#' @noRd
calculate_ratios_and_adequacy <- function(.data, ratio_pairs, adequate_range) {

  district = year = NULL

  check_cd_data(.data)
  check_ratio_pairs(ratio_pairs)
  check_required(adequate_range)

  # Check that adequate_range is a numeric vector of length 2
  if (!is.numeric(adequate_range) || length(adequate_range) != 2){
    cd_abort(c('x' = 'Adequate range must be a numeric vector of length 2.'), call = call)
  }

  all_pairs <- list_c(ratio_pairs)

  data_summary <- .data %>%
    # Calculate the average of indicators by district and year
    summarise(
      across(all_of(all_pairs), sum, na.rm = TRUE),
      .by = c(district, year)
    )

  data_summary <- data_summary %>%
    bind_cols(
      imap_dfc(ratio_pairs, ~ data_summary[[.x[1]]] / data_summary[[.x[2]]] %>% set_names(.y))
    ) %>%
    # Calculate adequacy checks
    mutate(across(names(ratio_pairs), ~ as.integer(.x >= adequate_range[1] & .x <= adequate_range[2]), .names = "adeq_{.col}")) %>%
    # Summarize adequacy checks by year
    summarise(
      across(all_of(all_pairs), sum, na.rm = TRUE),
      across(c(starts_with('adeq_'), starts_with('ratio')), mean, na.rm = TRUE),
      .by = year
    ) %>%
    mutate(
      across(starts_with('adeq_'), ~ round(.x * 100, 1)),
      across(all_of(all_pairs), ~ round(.x, 1))
    )

  return(data_summary)
}
