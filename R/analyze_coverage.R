#' Analyze National Immunization Coverage Data
#'
#' `analyze_coverage` calculates and combines national immunization coverage data
#' for a specified country, indicator, and denominator. It integrates local survey
#' data and WUENIC (WHO-UNICEF estimates) and processes the provided data source,
#' which is precomputed as a `cd_indicator_coverage` data frame.
#'
#' @param .data A precomputed data frame of type `cd_indicator_coverage` containing
#'   immunization coverage data. This dataset should include calculated metrics such
#'   as coverage indicators.
#' @param admin_level Character. Specifies the administrative level for analysis.
#'  Options include: `national`, `adminlevel_1` and `district`. Default is 'national'.
#' @param indicator Character. The immunization indicator to analyze. Must be one
#'   of the supported values: `bcg`, `anc1`, `pcv3`, `opv1`, `opv2`, `opv3`, `penta2`,
#'   `penta1`, `penta3`, `measles1`, `measles2`, `rota1`, `rota2`, `ipv1`, `ipv2`,
#'   `undervax`, `dropout_penta13`, `zerodose`, and`dropout_measles12`, `dropout_penta3mcv1`
#' @param denominator Character. The denominator used for calculating coverage estimates.
#'   Must be one of the following: `dhis2`, `anc1`, and`penta1`.
#' @param survey_data A data frame containing survey estimates for immunization coverage.
#' @param wuenic_data A data frame containing WUENIC (WHO-UNICEF estimates) data
#'   for immunization coverage. This should include columns specific to WUENIC-based
#'   coverage estimates.
#' @param subnational_map A data frame used to link subnational regions (e.g.,
#'   districts or administrative level 1 areas) to their respective parent regions.
#'   This is required for subnational analyses. Default is `NULL`.
#' @param region Character. Specifies the region of interest for subnational analyses.
#'   Required if `admin_level` is not 'national'. Default is `NULL`.
#'
#' @return A data frame of class `cd_coverage`, containing year-wise immunization
#'   coverage estimates from DHIS2, WUENIC, and survey data, along with confidence
#'   intervals where available. The resulting table is formatted for further analysis
#'   or visualization, with metadata describing the chosen administrative level, region,
#'   and denominator.
#'
#' @details
#' This function provides a flexible way to analyze immunization coverage by
#' combining data from multiple sources (survey data, WUENIC, and DHIS2). It supports
#' analyses at different administrative levels and is designed for integration into
#' dashboards, reports, or further data exploration workflows.
#'
#' @examples
#' \dontrun{
#' analyze_coverage(
#'   .data = precomputed_data,
#'   admin_level = 'national',
#'   indicator = 'penta3',
#'   denominator = 'penta1',
#'   survey_data = survey_df,
#'   wuenic_data = wuenic_df
#' )
#' }
#'
#' @export
analyze_coverage <- function(.data,
                             admin_level = c('national', 'adminlevel_1', 'district'),
                             indicator = c('bcg', 'anc1', 'pcv3', 'opv1', 'opv2', 'opv3',
                                           'penta2', 'pcv1', 'pcv2', 'penta1', 'penta3', 'measles1',
                                           'rota1', 'rota2', 'instdeliveries', 'measles2', 'ipv1', 'ipv2',
                                           'undervax', 'dropout_penta13', 'zerodose', 'dropout_measles12', 'dropout_penta3mcv1'),
                             denominator = c('dhis2', 'anc1', 'penta1'),
                             survey_data,
                             wuenic_data,
                             subnational_map = NULL,
                             region = NULL) {

  iso = year = value = NULL

  # Validate inputs
  check_cd_indicator_coverage(.data)
  check_wuenic_data(wuenic_data)
  check_survey_data(survey_data, admin_level)

  admin_level <- arg_match(admin_level)
  indicator <- arg_match(indicator)
  denominator <- arg_match(denominator)

  if (admin_level != 'national' && is.null(region)) {
    cd_abort(c('x' = 'Missing region', '!' = '{.arg region} should not be null.'))
  }

  # Prepare column names
  country_iso <- attr(.data, 'iso3')
  dhis2_col <- paste0('cov_', indicator, '_', denominator)
  survey_estimate_col <- paste0('r_', indicator)
  lower_ci_col <- paste0('ll_', indicator)
  upper_ci_col <- paste0('ul_', indicator)
  wuenic_col <- paste0('cov_', indicator, '_wuenic')

  # Prepare DHIS2 data
  .data <- .data %>%
    select(year, contains(dhis2_col), any_of(c('adminlevel_1', 'district')))

  # Prepare survey data
  survey_data <- survey_data %>%
    select(year, contains(survey_estimate_col), contains(lower_ci_col), contains(upper_ci_col), any_of(c('adminlevel_1', 'district'))) %>%
    join_subnational_map(admin_level, subnational_map) %>%
    check_district_column(admin_level, .data)

  # Prepare WUENIC data
  wuenic_data <- wuenic_data %>%
    select(year, contains(wuenic_col))

  join_column <- switch(admin_level,
                        national = 'year',
                        adminlevel_1 = c('year', 'adminlevel_1'),
                        district = c('year', 'adminlevel_1', 'district'))

  # Join and Transform data
  combined_data <- .data %>%
    full_join(survey_data, by = join_column, relationship = 'many-to-many') %>%
    left_join(wuenic_data, by = 'year') %>%
    filter(if (admin_level == 'national') TRUE else !!sym(admin_level) == region) %>% # Filter for region if applicable
    # Transform data
    mutate(
      !!dhis2_col := validate_column_existence(., dhis2_col),
      !!survey_estimate_col := validate_column_existence(., survey_estimate_col),
      !!wuenic_col := validate_column_existence(., wuenic_col),
      !!lower_ci_col := validate_column_existence(., lower_ci_col),
      !!upper_ci_col := validate_column_existence(., upper_ci_col)
    ) %>%
    pivot_longer(cols = contains(indicator), names_to = 'estimates') %>%
    arrange(year) %>%
    pivot_wider(names_from = year, values_from = value) %>%
    mutate(
      estimates = case_match(estimates,
                             dhis2_col ~ 'DHIS2 estimate',
                             survey_estimate_col ~ 'Survey estimates',
                             wuenic_col ~ 'WUENIC estimates',
                             lower_ci_col ~ '95% CI LL',
                             upper_ci_col ~ '95% CI UL',
      )
    ) %>%
    select(-any_of(c('adminlevel_1', 'admin_level_1', 'district')))

  # Return result
  new_tibble(
    combined_data,
    class = 'cd_coverage',
    admin_level = admin_level,
    region = region,
    denominator = denominator
  )
}

validate_column_existence <- function(data, column) {
  if (!column %in% colnames(data)) {
    cd_warn(c('!' = 'Column {.field {column}} not found in the data.'))
    return(NA_real_)
  }
  data[[column]]
}

join_subnational_map <- function(data, admin_level, map) {
  if (admin_level != 'national') {
    if (!is.null(map)) {
      data <- data %>%
        left_join(map, by = admin_level) %>%
        select(-adminlevel_1) %>%
        rename(adminlevel_1 = admin_level_1)
    }
  }

  data
}

check_district_column <- function(data, admin_level, dhis2_data) {
  if (admin_level == 'district' && !'district' %in% colnames(data)) {
    data <- dhis2_data %>%
      distinct(adminlevel_1, district) %>%
      left_join(data, by = 'adminlevel_1', relationship = 'many-to-many')
  }
  data
}
