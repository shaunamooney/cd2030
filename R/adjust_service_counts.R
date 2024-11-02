#' Adjust Service Data for Coverage Analysis
#'
#' `adjust_service_data` processes DHIS-2 service data to correct for reporting
#' completeness, apply k-factor adjustments, and handle extreme outliers, ensuring
#' data consistency for analytical use. These steps aim to provide a standardized
#' approach to data preparation for immunization coverage and health service utilization analysis.
#'
#' @param .data A `cd_data` dataframe, typically containing health facility data
#'   from DHIS-2 with service counts by month and district.
#' @param adjustment A string specifying the type of adjustment to apply:
#'   - **"default"**: Applies a standard k-factor (0.25) across all indicators.
#'   - **"custom"**: Uses user-provided `k_factors` for different indicator groups
#'       (values must be between 0 and 1).
#'   - **"none"**: Returns the data without adjustments.
#' @param k_factors A named numeric vector of custom k-factor values between 0 and
#'   1 for each indicator group (e.g., `c(anc = 0.3, idelv = 0.2, ...)`). Only used
#'   when `adjustment = "custom"`.
#'
#' @details This function includes several processing steps to prepare service data:
#'   1. **Validation**: Checks `.data` structure and validates `adjustment` input.
#'      If using `custom`, ensures `k_factors` are valid.
#'   2. **k-Factor Defaults**: Sets default k-factor values at 0.25, which can be
#'      modified if `custom` values are provided.
#'   3. **Reporting Completeness**: For any district-year reporting rates below
#'      75%, imputes missing data by district median, as low reporting can impact
#'      trend reliability.
#'   4. **k-Factor Scaling**: Adjusts service counts by applying the k-factor
#'      relative to reporting rates.
#'   5. **Outlier Detection**: Identifies extreme outliers using Hampel’s X84
#'      method, replacing values beyond 5 MADs from the median.
#'   6. **Data Imputation**: Replaces remaining missing data with district medians.
#'
#' @return A `cd_data` object containing adjusted service data, with flagged outliers
#'   managed, and missing values imputed for completeness.
#'
#' @examples
#' \dontrun{
#' # Default adjustment
#' adjusted_data <- adjust_service_data(data, adjustment = "default")
#'
#' # Custom adjustment
#' custom_k <- c(anc = 0.3, idelv = 0.2, pnc = 0.35, vacc = 0.4,
#'               opd = 0.3, ipd = 0.25)
#' adjusted_data_custom <- adjust_service_data(data, adjustment = "custom",
#'                                             k_factors = custom_k)
#'
#' # No adjustment
#' unadjusted_data <- adjust_service_data(data, adjustment = "none")
#' }
#'
#' @export
adjust_service_data <- function(.data,
                                adjustment = c("default", "custom", "none"),
                                k_factors = NULL) {

  district = year = month = NULL

  check_cd_data(.data)

  adjustment <- arg_match(adjustment)

  k_defaults <- c(anc = 0.25, idelv = 0.25, pnc = 0.25, vacc = 0.25, opd = 0.25, ipd = 0.25)

  if (adjustment == "custom") {
    if (is.null(k_factors) || any(k_factors < 0 | k_factors > 1)) {
      cd_abort(c('x' = 'k_factors must be a numeric vector with values between 0 and 1 for each indicator group.'))
    }
    k_factors <- k_factors[names(k_factors) %in% names(k_defaults)]
    k_defaults <- modifyList(k_defaults, k_factors)
  }

  if (adjustment == "none") {
    cd_info(c('i' = 'No adjustment applied. Data returned as-is.'))
    return(.data)
  }

  indicator_groups = attr(.data, 'indicator_groups')
  all_indicators <- list_c(indicator_groups)
  last_year <- max(.data$year)

  merged_data <- .data %>%
    mutate(
      across(
        all_of(all_indicators),
        ~ get(paste0(names(keep(indicator_groups, ~ cur_column() %in% .x)), '_rr')),
        .names = '{.col}_rr'
      ),
    ) %>%
    mutate(
      across(
        all_of(paste0(all_indicators, '_rr')),
        ~ if_else(. < 75 | is.na(.), median(.[. >= 75 & . <= 100], na.rm = TRUE), .)
      ),

      .by = district
    ) %>%
    arrange(district, year, month) %>%
    mutate(
      across(
        all_of(all_indicators),
        ~ {
          # Identify the main indicator group for the current sub-indicator
          group <- names(keep(indicator_groups, ~ cur_column() %in% .x))

          # Retrieve the rate column for the current group directly within cur_data()
          rate <- get(paste0(cur_column(), '_rr'))

          # Retrieve the k-value from the k_defaults list based on the group
          k_value <- k_defaults[[group]]

          # Apply the adjustment formula if the rate is not missing or zero
          if_else(
            !is.na(rate) & rate != 0,
            round(. * (1 + (1 / (rate / 100) - 1) * k_value), 0),
            .
          )
        }
      )
    ) %>%
    mutate(
      # Step 2: Calculate outlier flags based on bounds
      across(
        all_of(all_indicators),
        ~ {
          mad <-  mad(if_else(year < last_year, ., NA_real_), na.rm = TRUE)
          med <-  median(if_else(year < last_year, ., NA_real_), na.rm = TRUE)

          med <- if_else(is.na(med), max(med, na.rm = TRUE), med)
          mad <- if_else(is.na(mad), max(mad, na.rm = TRUE), mad)

          lower_bound <- round(med - 5 * mad, 1)
          upper_bound <- round(med + 5 * mad, 1)

          if_else(!is.na(.) & (. < lower_bound | . > upper_bound), 1, 0)
        },
        .names = '{.col}_outlier5std'
      ),

      .by = district
    ) %>%
    mutate(

      across(
        all_of(all_indicators),
        ~ {
          outlier <- get(paste0(cur_column(), '_outlier5std'))
          med <- round(median(if_else(outlier != 1, ., NA_real_), na.rm = TRUE), 0)

          if_else(
            outlier == 1,
            if (all(is.na(med))) NA_real_ else max(med, na.rm = TRUE),
            .
          )
        }
      ),

      across(
        all_of(all_indicators),
        ~ {
          med <- round(median(if_else(!is.na(.), ., NA_real_), na.rm = TRUE), 0)
          max_med <- if (all(is.na(med))) NA_real_ else max(med, na.rm = TRUE)
          if_else(
            is.na(.) & !is.na(max_med),
            max_med,
            .
          )
        }
      ),

      .by = c(district, year)
    ) %>%
    select(-any_of(paste0(all_indicators, '_rr')))

  return(merged_data)
}
