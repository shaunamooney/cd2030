#' Check for Outliers
#'
#' This function checks for outliers based on the Median Absolute Deviation (MAD)
#'   for a range of indicators. It flags outliers if values fall beyond five
#'   standard deviations from the median.
#'
#' @param data A data frame containing indicator data for districts and years.
#' @return A data frame summarizing the percentage of non-outliers for each
#'   indicator, by year.
#' @examples
#' \dontrun{
#'
#'   check_outliers(data)
#' }
#' @export
check_outliers <- function(data) {

  year = . = NULL

  if (!inherits(data, 'cd_data')) stop('An object of class cd_data is required.')

  # Define variable groups
  ancvargroup <- c('anc1')
  idelvvargroup <- c('ideliv', 'instlivebirths')
  vaccvargroup <- c('opv1', 'opv2', 'opv3', 'penta1', 'penta2', 'penta3', 'measles1', 'pcv1', 'pcv2', 'pcv3', 'measles2', 'bcg', 'rota1', 'rota2', 'ipv1', 'ipv2')
  allindicators <- c(ancvargroup, idelvvargroup, vaccvargroup)

  # firstyear <- min(data$year, na.rm = TRUE)
  lastyear <- max(data$merged_data$year, na.rm = TRUE)

  outliers_summary <- data$check_indicator %>%
    # Collapse data by year and calculate mean percentage of non-outliers
    summarise(
      across(ends_with('_outlier5std'), ~ mean(1 - .x, na.rm = TRUE) * 100),
      .by = year
    ) %>%
    mutate(
      across(contains('_outlier5std'), round, 2),
      # Calculate row mean of outliers if needed
      mean_outlier5std = round(rowMeans(select(., ends_with('_outlier5std')), na.rm = TRUE), 2)
    )

  new_tibble(
    outliers_summary,
    class = 'cd_outliers_summary'
  )
}

#' Check for Extreme Outliers
#'
#' This function identifies extreme outliers by calculating the maximum outlier
#'   value for each indicator in each district and year.
#'
#' @param data A data frame containing indicator data.
#' @return A data frame summarizing the percentage of districts without extreme
#'   outliers, by year.
#' @examples
#' \dontrun{
#'
#'   check_extreme_outlier(data)
#' }
#' @export
check_extreme_outlier <- function(data) {

  district = year = . = NULL

  if (!inherits(data, 'cd_data')) stop('The data object must be of class "cd_data".')

  # Define variable groups
  ancvargroup <- c('anc1')
  idelvvargroup <- c('ideliv', 'instlivebirths')
  vaccvargroup <- c('opv1', 'opv2', 'opv3', 'penta1', 'penta2', 'penta3', 'measles1', 'pcv1', 'pcv2', 'pcv3', 'measles2', 'bcg', 'rota1', 'rota2', 'ipv1', 'ipv2')
  allindicators <- c(ancvargroup, idelvvargroup, vaccvargroup)

  lastyear <- max(data$merged_data$year, na.rm = TRUE)

  outliers_summary <- map_df(allindicators, ~ {
    data$merged_data %>%
      mutate(
        !!paste0(.x, '_mad') := mad(if_else(year < lastyear, !!sym(.x), NA_real_), constant = 1, na.rm = TRUE),
        !!paste0(.x, '_med') := median(if_else(year < lastyear, !!sym(.x), NA_real_), na.rm = TRUE),
        !!paste0(.x, '_outlb5std') := !!sym(paste0(.x, '_med')) - 7.413 * !!sym(paste0(.x, '_mad')),
        !!paste0(.x, '_outub5std') := !!sym(paste0(.x, '_med')) + 7.413 * !!sym(paste0(.x, '_mad')),
        !!paste0(.x, '_outlier5std') := ifelse(!is.na(!!sym(.x)) & (!!sym(.x) < !!sym(paste0(.x, '_outlb5std')) | !!sym(.x) > !!sym(paste0(.x, '_outub5std'))), 1, 0),
        !!paste0('mis_', .x) := ifelse(is.na(!!sym(.x)), 1, 0),
        .by = district
      )
  }) %>%
    summarise(
      across(
        ends_with('_outlier5std'),
        ~ ifelse(all(is.na(.x)), NA_real_, max(.x, na.rm = TRUE))
      ),
      .by = c(district, year)
    ) %>%
    # Collapse to mean per year across districts
    select(-district, matches('_outlier5std$')) %>%
    summarise(
      across(everything(), ~ round((1 - mean(.x, na.rm = TRUE)) * 100, 0.01)),
      .by = year
    ) %>%
    # Add a mean row for district data
    mutate(
      mean_outlier5std = round(rowMeans(select(., -year), na.rm = TRUE), 2)
    )

  new_tibble(
    outliers_summary,
    class = 'cd_extreme_outliers_summary'
  )
}
