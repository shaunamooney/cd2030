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
#' @param admin_level Character. The administrative level at which to calculate
#'   reporting rates. Must be one of `"national"`, `"adminlevel_1"` or `"district"`.
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
#'
#' @return
#' A `cd_outliers_summary` object (tibble) with:
#'   - Each indicator's non-outlier percentage (`_outlier5std` columns).
#'   - Overall non-outlier summaries across all indicators, vaccination indicators, and tracers.
#'
#' @examples
#' \dontrun{
#'   # Check for extreme outliers in indicator data
#'   calculate_outliers_summary(data)
#' }
#'
#' @export
calculate_outliers_summary <- function(.data, admin_level = 'national') {

  year = . = NULL

  check_cd_data(.data)
  admin_level_cols <- get_admin_columns(admin_level)

  vaccine_only <- list_vaccines ()
  tracers <- list_tracer_vaccines ()
  allindicators <- get_all_indicators()

  data <- .data %>%
    add_outlier5std_column(allindicators) %>%
    summarise(
      across(ends_with('_outlier5std'), mean, na.rm = TRUE),
      .by = c(admin_level_cols, 'year')
    ) %>%
    mutate(
      mean_out_all = rowMeans(select(., ends_with('_outlier5std')), na.rm = TRUE),
      mean_out_vacc_only = rowMeans(select(., paste0(vaccine_only, '_outlier5std')), na.rm = TRUE),
      mean_out_vacc_tracer = rowMeans(select(., paste0(tracers, '_outlier5std')), na.rm = TRUE),

      across(c(ends_with('_outlier5std'), starts_with('mean_out_')), ~ round((1 - .x) * 100, 2))
    )

  new_tibble(
    data,
    class = 'cd_outlier',
    admin_level = admin_level
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

  vaccine_only <- list_vaccines ()
  tracers <- list_tracer_vaccines ()
  allindicators <- get_all_indicators()

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

#' Identify Outlier Units by Month for a Given Indicator
#'
#' This function summarizes a single immunization indicator by month and administrative level,
#' then applies a Hampel filter (5 × MAD) to flag extreme outliers. It returns a tidy object
#' suitable for plotting or time-series review at subnational levels.
#'
#' @param .data A `cd_data` object containing monthly health indicator data.
#' @param indicator Character. The name of a single indicator to evaluate for outliers.
#'   Must be one of:
#'   `"opv1"`, `"opv2"`, `"opv3"`, `"penta1"`, `"penta2"`, `"penta3"`, `"measles1"`,
#'   `"measles2"`, `"pcv1"`, `"pcv2"`, `"pcv3"`, `"bcg"`, `"rota1"`, `"rota2"`, `"ipv1"`, `"ipv2"`.
#' @param admin_level Character. The administrative level to summarize by.
#'   Options are `"adminlevel_1"` or `"district"`.
#'
#' @details
#' - Computes monthly means of the specified indicator by administrative unit and time.
#' - Calculates median and MAD (Median Absolute Deviation) for outlier detection.
#' - Flags outliers when values exceed ±5×MAD from the median.
#'
#' @return A `cd_outlier_list` object (a tibble) with the following columns:
#'   - Grouping columns (`adminlevel_1`, `district`, `year`, `month`)
#'   - The selected indicator
#'   - Median (`<indicator>_med`)
#'   - MAD (`<indicator>_mad`)
#'   - Outlier flag (`<indicator>_outlier5std`)
#'
#' @examples
#' \dontrun{
#'   # Detect monthly outliers in Penta1 at district level
#'   outliers <- list_outlier_units(cd_data, indicator = "penta1", admin_level = "district")
#'
#'   # Plot flagged points in a specific region
#'   plot(outliers, region = "Nakuru")
#' }
#'
#' @export
list_outlier_units <- function(.data,
                               indicator,
                               admin_level = c('adminlevel_1', 'district')) {
  check_cd_data(.data)
  indicator <- arg_match(indicator, list_vaccines())
  admin_level <- arg_match(admin_level)

  admin_level_cols <- get_admin_columns(admin_level)
  admin_level_cols <- c(admin_level_cols, 'year', 'month')

  x <- .data %>%
    summarise(
      across(all_of(indicator), mean, na.rm  = TRUE),
      .by = admin_level_cols
    ) %>%
    add_outlier5std_column(indicators = indicator, group_by = admin_level) %>%
    select(any_of(c(admin_level_cols, indicator, paste0(indicator, c('_med', '_mad', '_outlier5std')))))

  new_tibble(
    x,
    class = 'cd_outlier_list',
    indicator = indicator,
    admin_level = admin_level
  )
}
