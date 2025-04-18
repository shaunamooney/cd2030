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

#' @export
outliers_summary <- function(.data, admin_level = c('adminlevel_1', 'district')) {

  check_cd_data(.data)
  admin_level <- arg_match(admin_level)

  indicator_groups <- attr(.data, 'indicator_groups')
  allindicators <- list_c(indicator_groups)

  outlier_data <- .data %>%
    add_outlier5std_column(allindicators)

  new_tibble(
    outlier_data,
    class = 'cd_outlier',
    admin_level = admin_level
  )
}

#' @export
plot.cd_outlier <- function(x,
                            selection_type = c('region', 'vaccine'),
                            indicator = c('anc1', 'bcg', 'dropout_measles12', 'dropout_penta13',
                                          'dropout_penta3mcv1', 'instdeliveries', 'ipv1', 'ipv2',
                                          'measles1', 'measles2', 'opv1', 'opv2', 'opv3', 'pcv1',
                                          'pcv2', 'pcv3', 'penta1', 'penta2', 'penta3', 'rota1',
                                          'rota2', 'undervax', 'zerodose'),
                            ...) {

  admin_level <- attr(x, 'admin_level')

  admin_level_cols <- switch(
    admin_level,
    adminlevel_1 = 'adminlevel_1',
    district = c('adminlevel_1', 'district')
  )

  indicator <- arg_match(indicator)
  selection_type <- arg_match(selection_type)

  data_prepared <- x %>%
    summarise(
      across(ends_with('_outlier5std'), ~ (1 - mean(., na.rm = TRUE)) * 100),
      .by = c(admin_level_cols, year)
    )

  data_prepared <- if (selection_type == 'region') {
    data_prepared %>%
      mutate(
        category = !!sym(admin_level),
        value = !!sym(paste0(indicator, '_outlier5std'))
      )
  } else {
    data_prepared %>%
      pivot_longer(cols = ends_with('_outlier5std'),
                   names_to = 'category',
                   names_pattern = '^(.*)_outlier5std') %>%
        summarise(
          value = mean(value, na.rm = TRUE),
          .by = c(year, category)
        )
  }

  min_rr <- min(data_prepared$value, na.rm = TRUE)
  low_threshold <- ifelse(min_rr < 80, min_rr, 70)

  # breaks_vals <- if (selection_type == 'region') {
  #   c(low_threshold, 95, 97, 99, 100)
  # } else {
  #   c(low_threshold, 70, 80, 90, 100)
  # }

  breaks_vals <- c(low_threshold, 70, 80, 90, 100)

  ggplot(data_prepared, aes(year, value, fill = value)) +
    geom_col() +
    facet_wrap(~category) +
    labs(title = paste0('Percent non-outliers by year and by ', selection_type), x = 'Year', y = '% Non Outliers', fill = '% Non Outliers') +
    scale_fill_gradientn(
      colors = c('red', "red", "orange", "yellowgreen", "forestgreen"),
      values = scales::rescale(breaks_vals, to = c(0, 1)),
      breaks = scales::pretty_breaks(5)(c(low_threshold, 100)),
      labels = scales::pretty_breaks(5)(c(low_threshold, 100)),
      limits = c(low_threshold, 100)
    ) +
    theme(
      panel.background = element_blank(),
      strip.background = element_blank(),
      # strip.text = element_text(size = 12)
      panel.grid.major = element_line(colour = 'gray95'),
      axis.ticks = element_blank()
    )
}
