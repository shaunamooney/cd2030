#' Compute Aggregated Numerators for Health Indicators
#'
#' `compute_indicator_numerator` aggregates the numerators for specified health
#' indicators across different administrative levels, such as national, and
#' and subnational The function groups data by year and the specified administrative
#' level and calculates the sum of the selected indicators.
#'
#' @param .data A tibble of class `cd_data` containing health indicator data.
#'   The dataset must include columns for the relevant indicators, which are organized
#'   by the `indicator_groups` attribute.
#' @param admin_level Character. Specifies the administrative level for aggregation.
#'   Available options are: `"national"`, `"adminlevel_1"` and `"district"`. Default
#'   is `"national"`.
#'
#' @return A tibble containing yearly aggregated totals for each indicator at the
#'   specified administrative level.
#'
#' @examples
#' \dontrun{
#'   # Calculate national-level totals for health indicators
#'   national_totals <- compute_indicator_numerator(dhis2_data, admin_level = "national")
#'
#'   # Calculate administrative-level 1 totals for health indicators
#'   region_totals <- compute_indicator_numerator(dhis2_data, admin_level = "adminlevel_1")
#'
#'   # Calculate district-level totals for health indicators
#'   district_totals <- compute_indicator_numerator(dhis2_data, admin_level = "district")
#' }
#'
#' @export
compute_indicator_numerator <- function(.data, admin_level = c('national', 'adminlevel_1', 'district')) {

  year = NULL

  # Validate inputs
  check_cd_data(.data)
  admin_level <- arg_match(admin_level)

  # Define grouping variables based on admin_level
  group_vars <- switch(admin_level,
                       national = c('year'),
                       adminlevel_1 = c('adminlevel_1', 'year'),
                       district = c('adminlevel_1', 'district', 'year'))

  # Extract indicators from `indicator_groups` attribute
  all_indicators <- get_all_indicators(.data)

  # Check for missing required columns
  missing_columns <- setdiff(all_indicators, colnames(.data))
  if (length(missing_columns) > 0) {
    cd_abort(
      c('x' = 'Missing columns: {.field {paste(missing_columns, collapse = ", ")}}')
    )
  }

  # Summarize data
  .data %>%
    summarise(across(all_of(all_indicators), sum, na.rm = TRUE), .by = all_of(group_vars)) %>%
    arrange(year)
}
