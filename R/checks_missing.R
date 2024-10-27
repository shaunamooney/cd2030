#' Check for Missing Values in Each Year
#'
#' This function calculates the percentage of non-missing values for all
#'   indicators for each year.
#'
#' @param .data A data frame containing missing value indicators for multiple years.
#' @return A data frame summarizing the percentage of non-missing values by year.
#' @examples
#' \dontrun{
#'
#'   check_no_missing_year(data)
#' }
#' @export
check_no_missing_year <- function(.data) {

  year = var = . = NULL

  check_cd_data(.data)

  # Calculate percentage of non-missing data by year
  mis_summary_by_year <- .data$check_indicator %>%
    summarise(
      across(starts_with('mis_'), ~ mean(1 - .x, na.rm = TRUE) * 100),
      .by = year
    ) %>%
    mutate(mis_mean = rowMeans(select(., starts_with('mis_')), na.rm = TRUE)) %>%
    mutate(across(starts_with('mis_'), round, 2))

  new_tibble(
    mis_summary_by_year,
    class = 'cd_check_missing'
  )
}



#' Check for Missing Values by District
#'
#' This function calculates the percentage of districts with no missing values
#'   for each year.
#'
#' @param .data A data frame containing missing value indicators for districts and
#'   years.
#' @return A data frame summarizing the percentage of districts with no missing
#'   values by year.
#' @examples
#' \dontrun{
#'
#'   check_no_missing_district(data)
#' }
#' @export
check_no_missing_district <- function(.data) {

  year = district = . = NULL

  check_cd_data(.data)

  districts_no_missing <- .data$check_indicator %>%
    select(year, district, starts_with('mis_')) %>%
    group_by(year, district) %>%
    summarise(
      across(starts_with('mis_'), ~ mean(1 - .x, na.rm = TRUE) * 100),
      .by = year
    ) %>%
    mutate(mis_mean = rowMeans(select(., starts_with('mis_')), na.rm = TRUE)) %>%
    mutate(across(starts_with('mis_'), round, 2))

  new_tibble(
    districts_no_missing,
    class = 'cd_check_missing'
  )
}
