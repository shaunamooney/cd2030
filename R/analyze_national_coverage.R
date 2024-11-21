#' Analyze National Immunization Coverage Data
#'
#' This function, `analyze_national_coverage`, calculates and combines national
#' immunization coverage data for a specified country, indicator, and denominator.
#' It integrates internal data sources (`survey_data`, `wuenic_data`)and processes
#' the provided data source (`.data`).
#'
#' @param .data A data frame of type `cd_data` containing immunization coverage data.
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
#' @param un_estimates description
#' @param survey_data description
#' @param wuenic_data description
#' @param sbr Numeric. The stillbirth rate.
#' @param nmr Numeric. Neonatal mortality rate.
#' @param pnmr Numeric. Post-neonatal mortality rate.
#' @param anc1survey Numeric. Survey coverage rate for ANC-1.
#' @param dpt1survey Numeric. Survey coverage rate for Penta-1 (DPT1).
#' @param twin Numeric. Twin birth rate, default is 0.014.
#' @param preg_loss Numeric. Pregnancy loss rate, default is 0.03.
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
#' analyze_national_coverage(.data = my_data, indicator = "penta3",
#'                           denominator = "penta1")
#'}
#'
#' @export
analyze_national_coverage <- function(.data,
                                     indicator = c('bcg', "anc1", "pcv3", "opv1", "opv2", "opv3",
                                                   "penta2", "pcv1", "pcv2", "penta1", "penta3", "measles1",
                                                   "rota1", "rota2", "instdeliveries", "measles2", "ipv1", "ipv2",
                                                   "undervax", "dropout_penta13", "zerodose", "dropout_measles12", "dropout_penta3mcv1"),
                                     denominator = c("dhis2", "anc1", "penta1"),
                                     un_estimates,
                                     survey_data,
                                     wuenic_data,
                                     sbr = 0.02,
                                     nmr = 0.025,
                                     pnmr = 0.024,
                                     anc1survey = 0.98,
                                     dpt1survey = 0.97,
                                     twin = 0.015,
                                     preg_loss = 0.03) {

  iso = year = value = NULL

  check_cd_data(.data)
  check_un_estimates_data(un_estimates)
  check_wuenic_data(wuenic_data)
  check_survey_data(survey_data)

  indicator <- arg_match(indicator)
  denominator <- arg_match(denominator)

  country_iso <- attr(.data, 'iso3')

  survey_estimate_col <- paste0("r_", indicator)
  lower_ci_col <- paste0("ll_", indicator)
  upper_ci_col <- paste0("ul_", indicator)
  wuenic_col <- paste0("cov_", indicator, "_wuenic")
  dhis2_col <- paste0("cov_", indicator, "_", denominator)

  # Calculate national data and include ISO3 code
  national_data <- calculate_indicator_coverage(.data,
                                                un_estimates = un_estimates,
                                                sbr = sbr, nmr = nmr, pnmr = pnmr,
                                                anc1survey = anc1survey,
                                                dpt1survey = dpt1survey, twin = twin,
                                                preg_loss = preg_loss) %>%
    mutate(iso = country_iso) %>%
    select(iso, year, contains(dhis2_col))

  # Prepare survey data with renaming and filtering
  survey_data <- survey_data %>%
    select(iso, year, contains(survey_estimate_col), contains(lower_ci_col), contains(upper_ci_col))

  wuenic_data <- wuenic_data %>%
    select(iso, year, contains(wuenic_col))

  combined_data <- national_data %>%
    left_join(survey_data, by = c('iso', 'year')) %>%
    left_join(wuenic_data, by = c('year', 'iso')) %>%
    pivot_longer(cols = contains(indicator), names_to = 'estimates') %>%
    pivot_wider(names_from = year, values_from = value) %>%
    mutate(
      estimates = case_when(
        str_ends(estimates, denominator) ~ paste0(denominator, ' estimates'),
        str_ends(estimates, '_wuenic') ~ 'WUENIC estimates',
        str_starts(estimates, 'r_') ~ 'Survey estimates',
        str_starts(estimates, 'll') ~ '95% CI LL',
        str_starts(estimates, 'ul') ~ '95% CI UL',
      )
    ) %>%
    select(-iso)

  new_tibble(
    combined_data,
    class = 'cd_national_coverage',
    denominator = denominator
  )
}
