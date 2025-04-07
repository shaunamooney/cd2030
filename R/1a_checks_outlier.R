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
#' - **Rounding**: Percentages of non-outliers are rounded to two decimal places for
#'   accuracy and presentation clarity.
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
#' # Output: Percentage of monthly values that are not extreme outliers, by year
#'
#' @export
calculate_outliers_summary <- function(.data,
                                       admin_level = c('national', 'adminlevel_1', 'district')) {

  year = . = NULL

  check_cd_data(.data)
  admin_level <- arg_match(admin_level)

  admin_level_cols <- switch(
    admin_level,
    national = 'year',
    adminlevel_1 = c('adminlevel_1', 'year'),
    district = c('adminlevel_1', 'district', 'year')
  )

  vaccine_only <- get_indicator_groups(.data)$vacc
  tracers <- get_vaccine_tracers(.data)
  allindicators <- get_all_indicators(.data)

  data <- .data %>%
    add_outlier5std_column(allindicators) %>%
    summarise(
      across(ends_with('_outlier5std'), mean, na.rm = TRUE), .by = admin_level_cols
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

  vaccine_only <- get_indicator_groups(.data)$vacc
  tracers <- get_vaccine_tracers(.data)
  allindicators <- get_all_indicators(.data)

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

#' @export
list_outlier_units <- function(.data,
                               indicator = c(
                                 'opv1', 'opv2', 'opv3', 'penta1', 'penta2', 'penta3', 'measles1',
                                 'measles2', 'pcv1', 'pcv2', 'pcv3', 'bcg', 'rota1', 'rota2', 'ipv1', 'ipv2'
                               ),
                               admin_level = c('adminlevel_1', 'district')) {
  check_cd_data(.data)
  indicator <- arg_match(indicator)
  admin_level <- arg_match(admin_level)

  admin_level_cols <- switch(
    admin_level,
    adminlevel_1 = 'adminlevel_1',
    district = c('adminlevel_1', 'district')
  )

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

#' @export
plot.cd_outlier_list <- function(x, region = NULL) {

  indicator <- attr(x, 'indicator')
  admin_level <- attr(x, 'admin_level')
  if (is.null(region) || !is_scalar_character(region)) {
    abort('region must be a string')
  }

  med <- paste0(indicator, '_med')
  mad <- paste0(indicator, '_mad')

  x %>%
    mutate(
      date = ym(paste0(year, month, sep = '-')),
      upper_bound = !!sym(med) + !!sym(mad) * 5,
      lower_bound = !!sym(med) - !!sym(mad) *5,
      outlier_flag = !!sym(indicator) > upper_bound | !!sym(indicator) < lower_bound
    ) %>%
    filter(!!sym(admin_level) == region) %>%
    ggplot(aes(date)) +
    geom_line(aes(y = !!sym(indicator)), colour = 'forestgreen') +
    geom_point(aes(y = !!sym(indicator)), colour = 'forestgreen') +
    geom_line(aes(y = !!sym(med)), colour = 'cyan', linetype = 'dashed') +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "gray80", alpha = 0.5) +
    geom_point(data = function(df) filter(df, outlier_flag),
               aes(y = !!sym(indicator)), color = 'red', size = 2) +
    labs(
      title = NULL,
      y = paste0(indicator, ' doses given'),
      x = 'Month'
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_date(date_breaks = "3 months", date_labels = "%Y %b") +
    cd_plot_theme() +
    # theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, size = 16)
    )

}
