#' Analyze National Immunization Coverage Data
#'
#' This function, `analyze_national_coverage`, calculates and combines national
#' immunization coverage data for a specified country, indicator, and denominator.
#' It integrates internal data sources (`survey_data`, `wuenic_data`)and processes
#' the provided data source (`.data`).
#'
#' @param .data A data frame of type `cd_data` containing immunization coverage data.
#' @param country_name Character. Name of the country, e.g., "Kenya".
#' @param country_iso Character. ISO3 country code, e.g., "KEN".
#' @param indicator Character. The immunization indicator to analyze. Must be one
#'   of the supported values such as "bcg", "penta3", etc.
#'   Supported indicators include:
#'   - General vaccinations: "bcg", "anc1", "pcv3", "opv1", "opv2", "opv3"
#'   - Dose-based vaccinations: "penta2", "penta1", "penta3", "measles1", "measles2"
#'   - Additional vaccines: "rota1", "rota2", "ipv1", "ipv2"
#'   - Other health indicators: "undervax", "dropout_penta13", "zerodose",
#'     "dropout_measles12", "dropout_penta3mcv1"
#' @param denominator Character. The denominator used for calculating coverage
#'   estimates. Must be one of "dhis2", "anc1", or "penta1".
#'
#' @return A data frame of class `cd_national_coverage`, containing year-wise
#'   immunization coverage estimates from DHIS2, WUENIC, and Survey data, along
#'   with confidence intervals if available. The table provides organized coverage
#'   metrics and includes a new `cd_national_coverage` class for enhanced functionality.
#'
#' @details
#' The function performs the following steps:
#' 1. Validates `.data` and verifies required columns.
#' 2. Extracts the appropriate data columns from national, survey, and WUENIC data.
#' 3. Combines all relevant data into a single long-format table, including
#'    estimates and confidence intervals.
#' 4. Creates a new tibble with class `cd_national_coverage`, adding metadata
#'    such as the chosen denominator.
#'
#' This function provides a flexible way to analyze immunization coverage by
#' offering year-wise summaries and estimate categories. The final table format
#' is ready for further analysis or visualization.
#'
#' @examples
#' \dontrun{
#' analyze_national_coverage(.data = my_data, country_name = "Kenya",
#'                           country_iso = "KEN", indicator = "penta3",
#'                           denominator = "penta1")
#'}
#'
#' @export
analyze_national_coverage <- function(.data,
                                     country_name,
                                     country_iso,
                                     indicator = c('bcg', "anc1", "pcv3", "opv1", "opv2", "opv3",
                                                   "penta2", "pcv1", "pcv2", "penta1", "penta3", "measles1",
                                                   "rota1", "rota2", "instdeliveries", "measles2", "ipv1", "ipv2",
                                                   "undervax", "dropout_penta13", "zerodose", "dropout_measles12", "dropout_penta3mcv1"),
                                     denominator = c("dhis2", "anc1", "penta1")) {

  country = iso = year = value = NULL

  check_cd_data(.data)
  indicator <- arg_match(indicator)
  denominator <- arg_match(denominator)

  # Ensure `country_iso` is a character string and `.data` has necessary columns
  if (!is.character(country_iso) || length(country_iso) != 1) {
    cd_abort(
      c('x' = "{.arg country_iso} must be a single character string representing the ISO3 country code.")
    )
  }
  # if (!all(c("year", "iso", "country") %in% colnames(.data))) {
  #   stop("`.data` must contain `year`, `iso`, and `country` columns.")
  # }

  survey_estimate_col <- paste0("r_", indicator)
  lower_ci_col <- paste0("ll_", indicator)
  upper_ci_col <- paste0("ul_", indicator)
  wuenic_col <- paste0("cov_", indicator, "_wuenic")
  dhis2_col <- paste0("cov_", indicator, "_", denominator)

  # Calculate national data and include ISO3 code
  national_data <- calculate_indicator_coverage(.data, country_name) %>%
    mutate(iso = country_iso) %>%
    select(country, iso, year, contains(dhis2_col))


  # Prepare survey data with renaming and filtering
  survey_data <- survdata %>%
    select(-ends_with("_penta1")) %>%
    mutate(iso = country_iso) %>%
    rename_with(~ gsub("dpt", "penta", .x)) %>%
    rename_with(~ gsub("polio", "opv", .x)) %>%
    rename_with(~ gsub("msl", "measles", .x)) %>%
    rename_with(~ gsub("pneumo", "pcv", .x)) %>%
    rename_with(~ gsub("full", "fic", .x)) %>%
    rename_with(~ gsub("zero", "realzerodose", .x)) %>%
    rename_with(~ gsub("zpenta", "zerodose", .x)) %>%
    rename_with(~ gsub("invac", "undervax", .x)) %>%
    rename_with(~ gsub("dppenta", "dropout_penta13", .x)) %>%
    rename_with(~ gsub("dpopv", "dropout_opv13", .x)) %>%
    rename_with(~ gsub("measles22.*", "measles2", .x)) %>%
    rename_with(~ gsub("measles12.*", "measles1", .x)) %>%
    select(-ends_with("r"), -ends_with("24_35"), year) %>%
    filter(year >= 2015) %>%
    select(country, iso, year, contains(survey_estimate_col), contains(lower_ci_col), contains(upper_ci_col))

  wuenic_data <- wuenic %>%
    select(country, iso, year, contains(wuenic_col))

  combined_data <- national_data %>%
    left_join(survey_data, by = c('iso', 'year', 'country')) %>%
    left_join(wuenic_data, by = c('year', 'iso', 'country')) %>%
    pivot_longer(cols = contains(indicator), names_to = 'estimates') %>%
    pivot_wider(names_from = year, values_from = value) %>%
    mutate(
      estimates = case_when(
        str_ends(estimates, denominator) ~ paste0(denominator, ' estimates'),
        str_ends(estimates, '_wuenic') ~ 'WUENIC estimates',
        str_starts(estimates, 'r_') ~ 'Survey estimates',
        str_starts(estimates, 'll') ~ '95%CI LL',
        str_starts(estimates, 'ul') ~ '95%CI UL',
      )
    ) %>%
    select(-country, -iso)

  new_tibble(
    combined_data,
    class = 'cd_national_coverage',
    denominator = denominator
  )
}
