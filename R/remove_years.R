#' Remove Specified Years from DHIS-2 Dataset
#'
#' This function filters out specified years from a DHIS-2 dataset.
#'
#' @param .data A dataframe containing DHIS-2 data with a `year` column, representing
#' demographic information by year.
#' @param years_to_drop An integer vector specifying the years to be removed from the dataset.
#'
#' @return A data of class `cd_data`, but without rows for the specified years in `years_to_drop`.
#'
#' @details The function:
#'   1. Verifies that `.data` is a `cd_data` count down data.
#'   2. Checks that `years_to_drop` is an integer vector.
#'      - If `years_to_drop` is not a vector of integers, an error is raised.
#'   3. Filters `.data` to exclude rows where `year` matches any value in `years_to_drop`.
#'
#' @examples
#' \dontrun{
#'   # Example usage to remove data for 2017 and 2018:
#'   filtered_data <- remove_years(dhis_data, years_to_drop = c(2017, 2018))
#' }
#'
#' @export
remove_years <- function(.data, years_to_drop) {

  year = NULL

  check_cd_data(.data)
  if (!is_integerish(years_to_drop)) {
    cd_abort(
      c('{.arg years_to_drop} must be a vector integer')
    )
  }

  updated_merged_data <- .data$merged_data %>%
    filter(!year %in% years_to_drop)

  structure(
    list(
      quality_metrics = .data$quality_metrics,
      merged_data = updated_merged_data
    ),
    class = 'cd_data'
  )
}
