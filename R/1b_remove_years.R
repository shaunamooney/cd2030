#' Filter Dataset by Removing Specified Years
#'
#' The `filter_out_years` function is used to exclude specified years from a DHIS-2
#' dataset, refining the data for more targeted analysis or reporting. This is
#' particularly useful when focusing on specific timeframes, such as recent years
#' only or a custom-defined analysis period. By removing data from unwanted years,
#' you can create a tailored dataset aligned with analytical goals.
#'
#' @param .data A `cd_data` object, which contains a dataset with DHIS-2 indicators
#'   and demographic data organized by year.
#' @param years_to_exclude An integer vector that specifies the years to exclude
#'   from the dataset.
#'
#' @return A `cd_data` object with rows filtered to exclude those matching
#'   the specified years in `years_to_exclude`. The resulting data set is intended
#'   for focused analysis across the remaining years.
#'
#' @details
#' - **Purpose**: Use this function when specific years are unnecessary for
#'   analysis, such as baseline years or years with incomplete data.
#' - **Validation**:
#'   - Ensures `.data` is a valid `cd_data` object for DHIS-2 analysis.
#'   - Confirms `years_to_exclude` is a vector of integers; raises an error if
#'     not properly specified.
#' - **Functionality**: Filters `.data` to retain only the rows where the `year`
#'   does not match any value in `years_to_exclude`.
#'
#' @examples
#' \dontrun{
#'   # Example: Remove data for 2017 and 2018 from a DHIS-2 dataset
#'   filtered_data <- filter_out_years(dhis_data, years_to_exclude = c(2017, 2018))
#' }
#'
#' @export
filter_out_years <- function(.data, years_to_exclude) {

  year = NULL

  check_cd_data(.data)
  check_required(years_to_exclude)

  # Check that years_to_exclude is a vector of integers
  if (!is_integerish(years_to_exclude)) {
    cd_abort(
      c('{.arg years_to_exclude} must be a vector of integers')
    )
  }

  # Filter data to exclude specified years
  filtered_data <- .data %>%
    filter(!year %in% years_to_exclude)

  return(filtered_data)
}

