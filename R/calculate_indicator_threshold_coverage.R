#' Calculate Threshold Coverage for Health Indicators
#'
#' This function calculates the percentage of districts where coverage for specific health
#' indicators falls below a 10% threshold, for each year. The function generates a binary
#' variable for each indicator, denoting whether it is below 10%, and then calculates the
#' average percentage of below-threshold coverage for each indicator.
#'
#' @param .data A data frame or tibble containing health indicator coverage data with columns
#'   named in the format `cov_<indicator>_<source>`, where `<indicator>` is one of
#'   "zerodose", "undervax", "dropout_penta13", "dropout_measles12", "dropout_penta3mcv1",
#'   or "dropout_penta1mcv1", and `<source>` is one of "dhis2", "penta1", or "anc1".
#'
#' @return A tibble with the yearly average percentage of below-threshold coverage
#'   for each indicator-source combination, where each value represents the percentage of
#'   districts with below 10% coverage for that indicator and source.
#'
#' @details
#' The function performs the following steps:
#' 1. **Generate Binary Below-Threshold Variables**: For each indicator-source combination,
#'    it generates a binary variable indicating if coverage is below 10% (1 if below 10% and
#'    0 otherwise).
#' 2. **Rename Columns**: The generated columns are renamed by removing the `cov_` prefix.
#' 3. **Summarize**: The function then calculates the mean of each below-threshold indicator
#'    across all districts, grouped by `year`, resulting in the percentage of districts
#'    below 10% for each indicator-source.
#' 4. **Round to Percentage**: The percentages are then multiplied by 100 and rounded to one decimal place.
#'    Any missing values are set to 0%.
#'
#' @examples
#' \dontrun{
#' # Assuming `data` is a data frame with required columns:
#' result <- calculate_indicator_threshold_coverage(data)
#' print(result)
#' }
#'
#' @export
calculate_indicator_threshold_coverage <- function(.data) {

  year = NULL

  check_cd_indicator_coverage(.data)

  # Step 1: Generate Below 10% Indicator Variables for Each Coverage Metric
  coverage_vars <- c("zerodose", "undervax", "dropout_penta13", "dropout_measles12", "dropout_penta3mcv1", "dropout_penta1mcv1")
  indicators <- c('dhis2', 'penta1', 'anc1')

  .data %>%
    mutate(
      across(
        all_of(c(outer(paste0('cov_', coverage_vars), indicators, paste, sep = '_'))),
        ~ if_else(!is.na(.) & . < 10, 1, 0),
        .names = 'below10_{.col}'
      )
    ) %>%
    rename_with(~ gsub('cov_', '', .x), starts_with('below10_')) %>%
    summarise(
      across(starts_with("below10_"), mean, na.rm = TRUE),
      .by = year
    ) %>%
    mutate(
      across(starts_with("below10_"), ~ if_else(is.na(.x), 0, round(.x * 100, 1)))
    )
}

#' Calculate Dropout Coverage for Health Indicators Below a Threshold
#'
#' This function filters health indicator data to identify the percentage of administrative
#' regions where the coverage for a specified indicator falls below a 10% threshold for a given year.
#' If no regions meet the criteria (i.e., all values are below the threshold), a default output is returned.
#'
#' @param .data A data frame or tibble containing health indicator data, with coverage columns
#'   named in the format `cov_<indicator>_<source>`.
#' @param filter_year Numeric. The specific year to filter the data for dropout calculations.
#' @param indicator Character. The specific health indicator to evaluate. Options are:
#'   - `"zerodose"`: Zero-dose vaccination rate.
#'   - `"undervax"`: Under-vaccination rate.
#'   - `"dropout_penta13"`: Dropout rate from Penta-1 to Penta-3.
#'   - `"dropout_measles12"`: Dropout rate from Measles-1 to Measles-2.
#'   - `"dropout_penta3mcv1"`: Dropout rate from Penta-3 to MCV-1.
#'   - `"dropout_penta1mcv1"`: Dropout rate from Penta-1 to MCV-1.
#' @param source Character. The data source for the indicator. Options are:
#'   - `"dhis2"`: Data from DHIS-2.
#'   - `"anc1"`: Data from ANC-1 surveys.
#'   - `"penta1"`: Data from Penta-1 surveys.
#'
#' @return A tibble with the selected administrative level and coverage value for regions
#'   that do not meet the below-10% threshold for the specified indicator and year. If no regions
#'   meet the criteria, a default row is returned with "None" and 0 as values.
#'
#' @details
#' The function performs the following steps:
#' 1. **Validate Inputs**: Ensures that `filter_year`, `indicator`, and `source` are specified correctly.
#' 2. **Determine Admin Level**: Uses the attribute `admin_level` of `.data` to identify the appropriate
#'    administrative level for filtering (`adminlevel_1` or `district`).
#' 3. **Filter Data Below Threshold**: Creates a binary variable `below10` to indicate whether the
#'    coverage is below 10%. It then filters to keep only rows with `below10 == 0` and for the specified `filter_year`.
#' 4. **Handle No-Data Case**: If no rows meet the filtering criteria, the function returns a default row with
#'    "None" in the admin level column and 0 in the indicator column.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' result <- calculate_dropout(data, filter_year = 2023, indicator = "zerodose", source = "dhis2")
#' print(result)
#' }
#'
#' @export
calculate_dropout <- function(.data,
                              # filter_year,
                              indicator = c("zerodose", "undervax", "dropout_penta13", "dropout_measles12", "dropout_penta3mcv1", "dropout_penta1mcv1"),
                              source = c('dhis2', 'anc1', 'penta1')) {

  below10 = year = NULL

  check_cd_indicator_coverage(.data)
  check_required(filter_year)
  indicator <- arg_match(indicator)
  source <- arg_match(source)

  admin_level <- attr(.data, 'admin_level')

  column_name <- paste0('cov_', indicator, '_', source)

  data_below <- .data %>%
    mutate(
      below10 = if_else(!is.na(!!sym(column_name)) & !!sym(column_name) < 10, 1, 0)
    ) %>%
    # filter(below10 == 0, year == filter_year)
    filter(below10 == 0)

  if (NROW(data_below) == 0) {
    data_below <- tibble(
        !!admin_level := "None",
        !!column_name := 0
      )
    } else {

      data_below <- data_below %>%
        select(all_of(c(admin_level, column_name)))
    }

  return(data_below)
}
