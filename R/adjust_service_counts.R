#' Adjust Service Counts with Reporting Rate, k-Factor, and Outlier Adjustments
#'
#' This function adjusts service counts based on reporting rates, applies k-factor
#'   adjustments to compensate for incomplete reporting, and handles outliers using
#'   Hampel's method. It supports default adjustments, custom k-factor adjustments,
#'   and no adjustments.
#'
#' @param .data A data frame containing the merged service data for adjustment.
#' @param adjustment A character vector specifying the adjustment type. Options are:
#'        \describe{
#'          \item{"default"}{Applies default k-factors (0.25 for each indicator group).}
#'          \item{"custom"}{Applies user-specified `k_factors`, which must be between 0 and 1 for each indicator group.}
#'          \item{"none"}{Skips adjustments and returns the data as-is.}
#'        }
#' @param k_factors A named numeric vector of custom k-factor values between 0 and 1 for each indicator group (e.g., `c(anc = 0.3, idelv = 0.2, ...)`). Only used if `adjustment = "custom"`.
#'
#' @details
#' The function operates in several steps:
#'
#' 1. **Validate Input**: Ensures `.data` is properly structured and that
#'   `adjustment` is one of the specified options using `arg_match`. If
#'   `adjustment = "custom"`, validates `k_factors` to ensure values are between
#'   0 and 1.
#'
#' 2. **Define k-Factors**: Sets default k-factor values for each indicator group
#'   if `adjustment = "default"`. For `adjustment = "custom"`, the provided
#'   `k_factors` vector replaces default values for any matching indicator groups.
#'
#' 3. **Exit for "none" Adjustment**: If `adjustment = "none"`, the function saves
#'   the data without modifications and returns it directly.
#'
#' 4. **Indicator Grouping**: Defines specific indicators within each k-factor
#'   group, allowing targeted adjustments for each category (e.g., ANC, Delivery,
#'   PNC, Vaccination).
#'
#' 5. **Replace Low Reporting Rates**:
#'    - Reporting rates below 75% are replaced with `NA`, and district-level medians
#'      are calculated for each indicator.
#'    - These medians fill in for the low reporting rates.
#'
#' 6. **Apply k-Factor Adjustments**:
#'    - For each indicator in `indicator_groups`, applies a correction using the
#'      specified k-factor if the reporting rate is above zero.
#'    - Adjustments are calculated as `value * (1 + (1 / (reporting_rate / 100) - 1) * k_factor)`.
#'
#' 7. **Outlier Adjustment Using Hampel’s Method**:
#'    - For each indicator, calculates the median and MAD within each district
#'      and sets outlier bounds at `median ± 5 * MAD`.
#'    - Values outside these bounds are replaced with the district median.
#'
#' 8. **Replace Remaining Missing Values**:
#'    - After outlier adjustments, any remaining `NA` values for each indicator
#'      are replaced with the district-level median to ensure completeness.
#'
#' 9. **Cleanup**:
#'    - Removes intermediate columns used during calculations (e.g., `mad`, `med`,
#'      `adjusted`), leaving only final adjusted values.
#'
#' @return A `cd_data` object.
#'
#' @examples
#' \dontrun{
#' # Default adjustment
#' adjusted_data <- adjust_service_counts(data, adjustment = "default")
#'
#' # Custom adjustment with specific k-factors
#' custom_k <- c(anc = 0.3, idelv = 0.2, pnc = 0.35, vacc = 0.4, opd = 0.3,
#'   ipd = 0.25)
#' adjusted_data_custom <- adjust_service_counts(data, adjustment = "custom",
#'   k_factors = custom_k)
#'
#' # No adjustment
#' unadjusted_data <- adjust_service_counts(data, adjustment = "none")
#' }
#'
#' @export
# Adjust Service Counts with Reporting Rate, k-Factor, and Outlier Adjustments
adjust_service_counts <- function(.data,
                                  adjustment = c('default', 'custom', 'none'),
                                  k_factors = NULL) {

  district <- year <- month <- NULL

  check_cd_data(.data)

  # Validate adjustment argument using match.arg
  adjustment <- arg_match(adjustment)

  # Default k-factor values for each indicator group
  k_defaults <- c(anc = 0.25, idelv = 0.25, pnc = 0.25, vacc = 0.25, opd = 0.25, ipd = 0.25)

  # If custom adjustment is selected, validate the k_factors vector
  if (adjustment == 'custom') {
    if (is.null(k_factors) || any(k_factors < 0 | k_factors > 1)) {
      cd_abort(
        c('x' = 'For custom adjustment, provide a numeric vector of k-factor values (length 6) with each value between 0 and 1.')
      )
    }
    # Filter k_factors to include only names that are also in k_defaults
    k_factors <- k_factors[names(k_factors) %in% names(k_defaults)]

    # Update k_defaults with filtered k_factors
    k_defaults <- modifyList(k_defaults, k_factors)
  }

  # Exit early if adjustment is set to "none"
  if (adjustment == "none") {
    cd_info('No adjustment applied. Data saved as-is.')
    return(data)
  }

  # Define indicator groups for each k-factor
  indicator_groups <- list(
    anc = c("anc1", "anc4", "ipt2"),
    idelv = c("ideliv", "instlivebirths", "csection", "total_stillbirth", "stillbirth_f", "stillbirth_m", "maternal_deaths"),
    # pnc = c("pnc48h"),
    vacc = c("penta1", "penta3", "measles1", "bcg"),
    opd = c("opd_total", "opd_under5"),
    ipd = c("ipd_total", "ipd_under5")
  )

  all_indicators <- list_c(indicator_groups)

  data <- .data$merged_data %>%
    bind_cols(
      imap_dfc(indicator_groups, ~ {

        current_rr <- paste0(.y, '_rr')
        new_columns <- paste0(.x, '_rr')

        values <- .data$merged_data[[current_rr]]

        set_names(rep(list(values), length(new_columns)), new_columns)
      })
    )

  # Replace low reporting rates (<75%) with median reporting rates by district
  data <- data %>%
    group_by(district) %>%
    mutate(
      across(all_of(all_indicators),
             ~ ifelse(get(paste0(cur_column(), "_rr")) < 75, NA_real_, get(paste0(cur_column(), "_rr"))),
             .names = "{.col}_rr_med"),
      across(ends_with("_rr_med"), ~ median(., na.rm = TRUE), .names = "{.col}_med"),
      across(ends_with("_rr"),
             ~ coalesce(get(paste0(cur_column(), "_med")), .),
             .names = "{.col}_adjusted")
    ) %>%
    ungroup() %>%
    arrange(district, year, month)

  # Apply k-factor adjustments based on group and RR
  data <- data %>%
    rowwise() %>%
    mutate(
      across(all_of(indicator_groups),
             ~ ifelse(!is.na(get(paste0(cur_column(), "_adjusted"))) & get(paste0(cur_column(), "_adjusted")) != 0,
                      round(. * (1 + (1 / (get(paste0(cur_column(), "_adjusted")) / 100) - 1) * k_defaults[[str_extract(cur_column(), "^[a-z]+")]]), 1),
                      .),
             .names = "{.col}_adjusted_value")
    ) %>%
    ungroup()

  # Handle outliers with Hampel's method
  data <- data %>%
    group_by(district) %>%
    mutate(
      across(all_of(all_indicators),
             list(mad = ~ mad(., na.rm = TRUE), med = ~ median(., na.rm = TRUE)),
             .names = "{.col}_{fn}"),
      across(all_of(all_indicators),
             ~ ifelse(. < get(paste0(cur_column(), "_med")) - 5 * get(paste0(cur_column(), "_mad")) |
                        . > get(paste0(cur_column(), "_med")) + 5 * get(paste0(cur_column(), "_mad")),
                      get(paste0(cur_column(), "_med")), .),
             .names = "{.col}_outlier_adjusted")
    ) %>%
    ungroup()

  # Replace remaining NA values with the median by district
  data <- data %>%
    group_by(district) %>%
    mutate(
      across(all_of(all_indicators),
             ~ ifelse(is.na(.), get(paste0(cur_column(), "_med")), .),
             .names = "{.col}_final")
    ) %>%
    ungroup()

  # Cleanup intermediate columns
  data <- data %>%
    select(-ends_with("_mad"), -ends_with("_med"), -ends_with("_rr_med"), -ends_with("_adjusted"), -ends_with("_adjusted_value"))

  structure(
    list(
      quality_metrics = .data$quality_metrics,
      merged_data = data
    ),
    class = 'cd_data'
  )
}
