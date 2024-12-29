#' Calculate Outliers Summary by Year
#'
#' `calculate_outliers_summary` provides an annual overview of data quality by
#' summarizing extreme outliers for health indicators. Outliers are identified
#' based on robust statistical metrics (Median Absolute Deviation, MAD) and
#' flagged when they deviate significantly (beyond five MADs from the median).
#'
#' @param .data A data frame with district-level health indicators. This data
#'   frame must include a `district` and `year` column, along with indicator columns
#'   for calculating outliers. Outlier flags should be computed prior and named
#'   with the suffix `_outlier5std` (e.g., `anc1_outlier5std` where 1 indicates an
#'   outlier and 0 indicates non-outliers).
#'
#' @details
#' - **Outlier Detection**: Outliers are calculated based on Hampel’s robust X84
#'   method, using the Median Absolute Deviation (MAD). This method identifies values
#'   that exceed five times the MAD from the median, reducing the influence of extreme
#'   values on the analysis.
#' - **Annual Non-Outlier Rate**: For each indicator and each year, the function calculates the
#'   percentage of non-outliers. Additionally, the function aggregates the non-outlier
#'   rates across all indicators, as well as vaccination-only and tracer-only indicators,
#'   providing an overall data quality summary.
#' - **Rounding**: Percentages of non-outliers are rounded to two decimal places for
#'   accuracy and presentation clarity.
#'
#' @return A `cd_outliers_summary` object (tibble) with:
#'   - Each indicator's non-outlier percentage (`_outlier5std` columns).
#'   - Overall non-outlier summaries across all indicators, vaccination indicators, and tracers.
#'
#' @examples
#' \dontrun{
#'   # Check for extreme outliers in indicator data
#'   calculate_outliers_summary(data)
#' }
#'
#' # Output: Percentage of monthly values that are not extreme outliers, by year
#'
#' @export
calculate_outliers_summary <- function(.data) {

  year = . = NULL

  check_cd_data(.data)

  indicator_groups <- attr(.data, 'indicator_groups')
  vaccine_only <- indicator_groups[['vacc']]
  tracers <- attr(.data, 'tracers')
  allindicators <- list_c(indicator_groups)

  data <- .data %>%
    add_outlier5std_column(allindicators) %>%
    summarise(
      across(
        ends_with('_outlier5std'), mean, na.rm = TRUE),
      .by = year
    ) %>%
    mutate(
      mean_out_all = rowMeans(select(., ends_with('_outlier5std')), na.rm = TRUE),
      mean_out_vacc_only = rowMeans(select(., paste0(vaccine_only, '_outlier5std')), na.rm = TRUE),
      mean_out_vacc_tracer = rowMeans(select(., paste0(tracers, '_outlier5std')), na.rm = TRUE),

      across(c(ends_with('_outlier5std'), starts_with('mean_out_')), ~ round((1 - .x) * 100, 2))
    )

  new_tibble(
    data,
    class = 'cd_outliers_summary'
  )
}

#' Calculate District-Level Outliers Summary by Year
#'
#' `calculate_district_outlier_summary` computes a district-level summary of extreme
#' outliers for specified health indicators. This function aggregates extreme outlier counts
#' for each indicator by first identifying the maximum outlier flag within each district
#' and year. An outlier is flagged based on Hampel's X84 method, where values exceeding
#' five Median Absolute Deviations (MAD) from the median are considered extreme outliers.
#'
#' @param .data A data frame containing district-level health indicator data. This data
#'   frame must include precomputed outlier flags (columns ending in `_outlier5std`),
#'   where 1 represents an outlier and 0 represents non-outliers.
#'
#' @details
#' - **Outlier Aggregation**: The function first calculates the maximum outlier flag within
#'   each district and year. This district-level flag is used to determine if extreme outliers
#'   are present for each indicator.
#' - **Non-Outlier Percentage**: After aggregating by district and year, it computes the
#'   mean percentage of districts without extreme outliers for each indicator, as well as
#'   overall summaries for vaccination-only and tracer indicators.
#' - **Rounding**: Non-outlier percentages are rounded to two decimal places for clarity
#'   in reporting and analysis.
#'
#' @return A `cd_district_outliers_summary` object (tibble) with:
#'   - Each indicator's percentage of districts without extreme outliers, calculated yearly.
#'   - Aggregated summaries for non-outliers across all indicators, vaccination indicators,
#'     and tracer indicators.
#'
#' @examples
#' \dontrun{
#'   # Summarize the proportion of districts without extreme outliers
#'   calculate_district_outlier_summary(data)
#' }
#'
#' @export
calculate_district_outlier_summary <- function(.data) {

  district = year = . = NULL

  check_cd_data(.data)

  indicator_groups <- attr(.data, 'indicator_groups')
  vaccine_only <- indicator_groups[['vacc']]
  tracers <- attr(.data, 'tracers')
  allindicators <- list_c(indicator_groups)

  data <- .data %>%
    add_outlier5std_column(allindicators) %>%
    summarise(
      across(ends_with('_outlier5std'), ~ robust_max(.)),
      .by = c(district, year)
    ) %>%
    summarise(across(ends_with('_outlier5std'), mean, na.rm = TRUE), .by = year) %>%
    mutate(
      mean_out_all = rowMeans(select(., ends_with('_outlier5std')), na.rm = TRUE),
      mean_out_vacc_only = rowMeans(select(., paste0(vaccine_only, '_outlier5std')), na.rm = TRUE),
      mean_out_vacc_tracer = rowMeans(select(., paste0(tracers, '_outlier5std')), na.rm = TRUE),

      across(c(ends_with('_outlier5std'), starts_with('mean_out_')), ~ round((1 - .x) * 100, 2))
    )

  new_tibble(
    data,
    class = 'cd_district_outliers_summary'
  )
}
