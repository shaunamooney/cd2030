#' Add Outlier Flags Based on 5-Standard Deviation Bounds
#'
#' `add_outlier5std_column` calculates and adds outlier flags for specified indicators
#' in a dataset. The flags indicate whether a value is outside the bounds defined by
#' 5 times the Median Absolute Deviation (MAD) from the median. This method is applied
#' for each indicator, grouping by district and considering only values from years
#' prior to the latest year.
#'
#' @param .data A `cd_data` tibble containing health indicator data. The dataset must
#'   include a `year` column and the specified indicators.
#' @param indicators A character vector specifying the names of the indicator columns
#'   to be analyzed for outliers.
#'
#' @return A tibble with additional columns for each indicator, named `{indicator}_outlier5std`,
#'   containing 1 if the value is an outlier and 0 otherwise.
#'
#' @details
#' - The function calculates the MAD and median for each indicator, excluding the
#'   latest year in the dataset.
#' - Values are flagged as outliers if they fall outside the range:
#'   \[ \text{median} \pm 5 \times \text{MAD} \]
#' - The calculations are performed for each district, grouping by the `district` column.
#'
#' @examples
#' \dontrun{
#' # Example dataset
#' data <- tibble(
#'   district = rep(c("District A", "District B"), each = 5),
#'   year = c(2018:2022, 2018:2022),
#'   indicator1 = c(10, 12, 13, 50, 14, 9, 11, 10, 15, 12)
#' )
#'
#' # Add outlier flags for 'indicator1'
#' flagged_data <- add_outlier5std_column(data, indicators = "indicator1")
#' }
#'
#' @export
add_outlier5std_column <- function(.data, indicators) {

  district = NULL

  check_cd_data(.data, call = call)
  check_required(indicators)

  last_year <- robust_max(.data$year)

  .data %>%
    mutate(
      # Step 2: Calculate outlier flags based on bounds
      across(
        all_of(indicators),
        ~ {
          mad <-  mad(if_else(year < last_year, ., NA_real_), na.rm = TRUE)
          med <-  median(if_else(year < last_year, ., NA_real_), na.rm = TRUE)

          med <- if_else(is.na(med), robust_max(med), med)
          mad <- if_else(is.na(mad), robust_max(mad), mad)

          lower_bound <- round(med - 5 * mad, 1)
          upper_bound <- round(med + 5 * mad, 1)

          if_else(!is.na(.) & (. < lower_bound | . > upper_bound), 1, 0)
        },
        .names = '{.col}_outlier5std'
      ),

      .by = district
    )
}
