#' Compute Indicator Numerators for Health Metrics by Administrative Level
#'
#' `compute_indicator_numerator` calculates the numerators for various health indicators
#' across specified administrative levels, including national, admin_level_1, and district.
#' This function aggregates counts by year and selected administrative level(s) for a
#' comprehensive view of health metrics at each level.
#'
#' @param .data A `cd_data` tibble containing health indicator counts, with required
#'   indicators organized by the `indicator_groups` attribute.
#' @param admin_level A character string specifying the administrative level to calculate
#'   numerators for. Options are:
#'   - `"national"`: Aggregates data at the country level.
#'   - `"admin_level_1"`: Aggregates data at the first administrative level.
#'   - `"district"`: Aggregates data at the district level.
#'
#' @return A tibble with yearly totals for each indicator at the specified administrative
#'   level, grouped by country, year, and additional grouping variables as per the chosen level.
#'
#' @details
#' This function performs the following steps:
#' 1. **Validation**: Confirms that `.data` is a `cd_data` object and contains
#'    required columns for specified indicators.
#' 2. **Aggregation**: Sums the counts for each indicator, grouped by `country`,
#'    `year`, and additional grouping variables as per the chosen `admin_level`.
#' 3. **Output**: Returns a tibble ordered by `year`, containing aggregated indicators
#'    alongside the `country`, `year`, and administrative level columns.
#'
#' @examples
#' \dontrun{
#' # Calculate national-level totals for health indicators
#' national_totals <- compute_indicator_numerator(dhis2_data, admin_level = "national")
#'
#' # Calculate district-level totals for health indicators
#' district_totals <- compute_indicator_numerator(dhis2_data, admin_level = "district")
#' }
#'
#' @export
compute_indicator_numerator <- function(.data, admin_level = c('national', 'admin_level_1', 'district')) {

  year = NULL

  check_cd_data(.data)
  admin_level <- arg_match(admin_level)

  group_vars <- switch(admin_level,
                       national = c('country', 'year'),
                       admin_level_1 = c('country', 'adminlevel_1', 'year'),
                       district = c('country', 'adminlevel_1', 'district', 'year'))

  indicator_groups <- attr(.data, 'indicator_groups')
  all_indicators <- list_c(indicator_groups)

  missing_columns <- setdiff(all_indicators, colnames(.data))
  if (length(missing_columns) > 0) {
    cd_abort(
      c('x' = 'Missing columns: {.field {paste(missing_columns, collapse = ", ")}}')
    )
  }

  .data %>%
    summarise(across(all_of(all_indicators), sum, na.rm = TRUE), .by = all_of(group_vars)) %>%
    arrange(year)
}
