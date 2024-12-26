#' Analyze National Immunization Coverage Data
#'
#' `analyze_coverage`, calculates and combines national immunization coverage data
#' for a specified country, indicator, and denominator. It integrates internal data
#' sources (`survey_data`, `wuenic_data`)and processes the provided data source (`.data`).
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
#' @param subnational_map description
#' @param sbr Numeric. The stillbirth rate.
#' @param nmr Numeric. Neonatal mortality rate.
#' @param pnmr Numeric. Post-neonatal mortality rate.
#' @param anc1survey Numeric. Survey coverage rate for ANC-1.
#' @param dpt1survey Numeric. Survey coverage rate for Penta-1 (DPT1).
#' @param twin Numeric. Twin birth rate, default is 0.014.
#' @param preg_loss Numeric. Pregnancy loss rate, default is 0.03.
#'
#' @return A data frame of class `cd_coverage`, containing year-wise
#'   immunization coverage estimates from DHIS2, WUENIC, and Survey data, along
#'   with confidence intervals if available. The table provides organized coverage
#'   metrics and includes a new `cd_coverage` class for enhanced functionality.
#'
#' @details
#' The function performs the following steps:
#' 1. Validates `.data` and verifies required columns.
#' 2. Extracts the appropriate data columns from national, survey, and WUENIC data.
#' 3. Combines all relevant data into a single long-format table, including
#'    estimates and confidence intervals.
#' 4. Creates a new tibble with class `cd_coverage`, adding metadata
#'    such as the chosen denominator.
#'
#' This function provides a flexible way to analyze immunization coverage by
#' offering year-wise summaries and estimate categories. The final table format
#' is ready for further analysis or visualization.
#'
#' @examples
#' \dontrun{
#' analyze_coverage(.data = my_data, indicator = "penta3",
#'                  denominator = "penta1")
#'}
#'
#' @export
analyze_coverage <- function(.data,
                             admin_level = c('national', 'admin_level_1', 'district'),
                             indicator = c('bcg', "anc1", "pcv3", "opv1", "opv2", "opv3",
                                           "penta2", "pcv1", "pcv2", "penta1", "penta3", "measles1",
                                           "rota1", "rota2", "instdeliveries", "measles2", "ipv1", "ipv2",
                                           "undervax", "dropout_penta13", "zerodose", "dropout_measles12", "dropout_penta3mcv1"),
                             denominator = c("dhis2", "anc1", "penta1"),
                             un_estimates,
                             survey_data,
                             wuenic_data,
                             subnational_map = NULL,
                             region = NULL,
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
  admin_level <- arg_match(admin_level)

  if (admin_level == 'national' && !is.null(survey_data[['adminlevel_1']])) {
    cd_abort(
      c('x' = 'Regional survey data used in national level')
    )
  } else if (admin_level != 'national' && is.null(survey_data[['adminlevel_1']])) {
    cd_abort(
      c('x' = 'National survey data used in subnational level')
    )
  }

  if (admin_level != 'national' && is.null(region)) {
    cd_abort(
      c('x' = 'Missing region',
        '!' = '{.arg region} should not be null.')
    )
  }

  country_iso <- attr(.data, 'iso3')

  survey_estimate_col <- paste0("r_", indicator)
  lower_ci_col <- paste0("ll_", indicator)
  upper_ci_col <- paste0("ul_", indicator)
  wuenic_col <- paste0("cov_", indicator, "_wuenic")
  dhis2_col <- paste0("cov_", indicator, "_", denominator)

  # Calculate national data and include ISO3 code
  national_data <- calculate_indicator_coverage(.data,
                                                admin_level = admin_level,
                                                un_estimates = un_estimates,
                                                sbr = sbr, nmr = nmr, pnmr = pnmr,
                                                anc1survey = anc1survey,
                                                dpt1survey = dpt1survey, twin = twin,
                                                preg_loss = preg_loss) %>%
    mutate(iso = country_iso) %>%
    select(iso, year, contains(dhis2_col), any_of(c('adminlevel_1', 'district')))

  # Prepare survey data with renaming and filtering
  survey_data <- survey_data %>%
    select(iso, year, contains(survey_estimate_col), contains(lower_ci_col), contains(upper_ci_col), any_of(c('adminlevel_1', 'district')))

  if (admin_level != 'national') {
    survey_data <- if (is.null(subnational_map)) {
      survey_data %>%
        mutate(
          admin_level_1 = adminlevel_1
        )
    } else {
      switch (admin_level,
              admin_level_1 = survey_data %>% left_join(subnational_map, by = 'adminlevel_1'),
              district = survey_data %>% left_join(subnational_map, by = 'district')
      )
    }

    survey_data <- survey_data %>%
      select(-adminlevel_1) %>%
      rename(adminlevel_1 = admin_level_1)
  }

  wuenic_data <- wuenic_data %>%
    select(iso, year, contains(wuenic_col))

  join_by <- switch (admin_level,
    national = c('iso', 'year'),
    admin_level_1 = c('iso', 'year', 'adminlevel_1'),
    district = c('iso', 'year', 'district')
  )

  combined_data <- national_data %>%
    full_join(survey_data, by = join_by) %>%
    left_join(wuenic_data, by = c('year', 'iso'))

  if (admin_level != 'national') {
    combined_data <- switch (admin_level,
      admin_level_1 = combined_data %>% filter(adminlevel_1 == region),
      district = combined_data %>% filter(district == region)
    )
  }

  combined_data <- combined_data %>%
    mutate(
      !!dhis2_col := if(dhis2_col %in% colnames(.)) .[[dhis2_col]] else NA_real_,
      !!survey_estimate_col := if(survey_estimate_col %in% colnames(.)) .[[survey_estimate_col]] else NA_real_,
      !!wuenic_col := if(wuenic_col %in% colnames(.)) .[[wuenic_col]] else NA_real_,
      !!lower_ci_col := if(lower_ci_col %in% colnames(.)) .[[lower_ci_col]] else NA_real_,
      !!upper_ci_col := if(upper_ci_col %in% colnames(.)) .[[upper_ci_col]] else NA_real_
    ) %>%
    pivot_longer(cols = contains(indicator), names_to = 'estimates') %>%
    pivot_wider(names_from = year, values_from = value) %>%
    mutate(
      estimates = case_match(estimates,
                             dhis2_col ~ 'DHIS2 estimate',
                             wuenic_col ~ 'WUENIC estimates',
                             survey_estimate_col ~ 'Survey estimates',
                             lower_ci_col ~ '95% CI LL',
                             upper_ci_col ~ '95% CI UL',
      )
    ) %>%
    select(-any_of(c('iso', 'adminlevel_1', 'admin_level_1')))

  new_tibble(
    combined_data,
    class = 'cd_coverage',
    admin_level = admin_level,
    region = region,
    denominator = denominator
  )
}
