#' Calculate Quality Metrics for Health Indicators
#'
#' This function calculates data quality metrics, such as outlier detection and missingness, for a range of health indicators across districts and years.
#' The calculations involve determining bounds for outliers using Hampel's X84 method based on median absolute deviation (MAD), flagging outliers and missing values, and storing results in a summary data frame.
#'
#' @param .data A data frame containing health indicator data. Must include columns for `district` and `year`, as well as columns for each indicator in the predefined groups.
#' @param call The calling environment for error checking. Defaults to `caller_env()`.
#'
#' @details
#' - **Indicator Groups**:
#'   The function groups indicators into categories (e.g., ANC, IDelivery, Vaccination) and calculates MAD and median values up to the `lastyear` for each indicator in `allindicators`.
#' - **Outlier Detection**:
#'   Outliers are determined based on Hampel's X84 method, where outlier bounds are defined as 5 standard deviations from the median.
#' - **Missingness Check**:
#'   Missing values are flagged as "Missing" and non-missing as "Non-Missing."
#'
#' @return A data frame (`outliers_summary`) containing:
#' - Outlier flags for each indicator, suffixed with `_outlier5std`.
#' - Missingness status for each indicator, prefixed with `mis_`.
#' - `district` and `year` columns to maintain grouping information.
#'
#' @examples
#' \dontrun{
#' # Assuming `.data` is a data frame with necessary indicator columns
#' quality_metrics <- calculate_quality_metrics(.data)
#' }
#'
#' @noRd
calculate_quality_metrics <- function(.data, call = caller_env()) {

  district = year = NULL

  check_required(.data, call = call)

  # Define variable groups
  ancvargroup <- c('anc1', 'anc4', 'ipt2')
  idelvvargroup <- c('ideliv', 'instlivebirths', 'csection', 'total_stillbirth', 'stillbirth_f', 'stillbirth_m', 'maternal_deaths')
  vaccvargroup <- c('opv1', 'opv2', 'opv3', 'penta1', 'penta2', 'penta3', 'measles1', 'pcv1', 'pcv2', 'pcv3', 'measles2', 'bcg', 'rota1', 'rota2', 'ipv1', 'ipv2')
  pncvargroup <- 'pnc48h'
  opdvargroup <- c('opd_total', 'opd_under5')
  ipdvargroup <- c('ipd_total', 'ipd_under5')
  allindicators <- c(ancvargroup, idelvvargroup, vaccvargroup, pncvargroup, opdvargroup, ipdvargroup)

  lastyear <- max(.data$year, na.rm = TRUE)

  outliers_summary <- .data %>%
    select(district, year, allindicators) %>%
    mutate(
      # Calculate `_mad` and `_med` for each indicator in `allindicators` up to `lastyear`
      across(allindicators, ~ mad(ifelse(year < lastyear, ., NA_real_), na.rm = TRUE), .names = "{.col}_mad"),
      # across(ends_with('_mad'), ~ ifelse(is.na(.), max(., na.rm = TRUE), .)),
      across(allindicators, ~ median(ifelse(year < lastyear, ., NA_real_), na.rm = TRUE), .names = "{.col}_med"),
      # across(ends_with('_med'), ~ ifelse(is.na(.), max(., na.rm = TRUE), .x)),

      # Calculate lower and upper bounds for outliers using Hampel's X84 method
      across(allindicators, ~ get(paste0(cur_column(), "_med")) - 5 * get(paste0(cur_column(), "_mad")), .names = "{.col}_outlb5std"),
      across(allindicators, ~ get(paste0(cur_column(), "_med")) + 5 * get(paste0(cur_column(), "_mad")), .names = "{.col}_outub5std"),

      # Flag values as outliers if they fall outside the calculated bounds
      across(allindicators,
             ~ ifelse(!is.na(.) & (. < get(paste0(cur_column(), "_outlb5std")) | . > get(paste0(cur_column(), "_outub5std"))),
                      1, 0
             ),
             .names = "{.col}_outlier5std"),

      # Flag missing values
      across(allindicators,
             ~ case_when(
               is.na(.) ~ 1,
               .default = 0),
             .names = "mis_{.col}"),

      .by = district
    ) %>%
    select(district, year, ends_with('_outlier5std'), starts_with('mis_'))

  return(outliers_summary)
}
