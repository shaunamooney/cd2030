#' Combine Immunization Coverage Data
#'
#' `combine_coverage` integrates immunization coverage data from three sources:
#' DHIS2 (District Health Information Software), survey-based estimates, and
#' WUENIC (WHO-UNICEF estimates). The combined dataset is prepared for analysis
#' at various administrative levels.
#'
#' @param .data A `cd_indicator_coverage` data frame with DHIS2 coverage metrics.
#' @param survey_data A data frame containing survey-based immunization estimates.
#' @param wuenic_data A data frame containing WHO-UNICEF (WUENIC) coverage estimates.
#' @param subnational_map (Optional) A data frame mapping subnational regions to
#'   parent regions, required for subnational-level analyses. Default is `NULL`.
#'
#' @return A `cd_coverage` data frame containing harmonized coverage estimates
#'   for each year from DHIS2, WUENIC, and survey data. Includes metadata for
#'   administrative levels, regions, and denominators.
#'
#' @examples
#' \dontrun{
#'   combine_coverage(precomputed_data, survey_df, wuenic_df)
#' }
#' @export
combine_coverage  <- function(.data,
                              survey_data,
                              wuenic_data,
                              subnational_map = NULL) {

  year = NULL

  admin_level <- attr(.data, 'admin_level')

  # Validate inputs
  check_cd_indicator_coverage(.data)
  check_wuenic_data(wuenic_data)
  check_survey_data(survey_data, admin_level)

  # Prepare DHIS2 data
  dhis2_data <- .data %>%
    select(year, any_of(c('adminlevel_1', 'district')), matches('^cov_'))

  # Prepare survey data
  survey_data <- survey_data %>%
    select(year, any_of(c('adminlevel_1', 'district')), matches('^ll_|^ul|^r_')) %>%
    join_subnational_map(admin_level, subnational_map) %>%
    check_district_column(admin_level, dhis2_data)

  # Prepare WUENIC data
  wuenic_data <- wuenic_data %>%
    select(year, matches('^cov_'))

  join_keys <- switch(
    admin_level,
    national = 'year',
    adminlevel_1 = c('year', 'adminlevel_1'),
    district = c('year', 'adminlevel_1', 'district')
  )

  # Join and Transform data
  combined_data <- dhis2_data %>%
    full_join(survey_data, by = join_keys, relationship = 'many-to-many') %>%
    left_join(wuenic_data, by = 'year') %>%
    select(-any_of(c('admin_level_1')))

  # Return result
  new_tibble(
    combined_data,
    class = 'cd_coverage',
    admin_level = admin_level
  )
}


#' Filter and Reshape Coverage Data
#'
#' `filter_coverage` extracts specific coverage indicators and denominators from a
#' combined coverage dataset, applies regional filtering, and reshapes the data
#' for analysis and visualisation.
#'
#' @param .data A `cd_coverage` data frame with combined immunization coverage data.
#' @param indicator Coverage indicators to include (e.g., `"penta3"`, `"measles1"`).
#' @param denominator Denominator sources for coverage calculations (e.g., `"dhis2"`).
#' @param region (Optional) Region for subnational filtering (e.g., `"Central Province"`).
#'   Required for subnational analyses.
#'
#' @return A reshaped data frame with filtered indicators and denominators.
#'
#' @examples
#' \dontrun{
#'   filter_coverage(combined_data, c("penta3", "measles1"), "dhis2", "Central Province")
#' }
#' @export
filter_coverage <- function(.data,
                            indicator = c('anc1', 'bcg', 'dropout_measles12', 'dropout_penta13',
                                          'dropout_penta3mcv1', 'instdeliveries', 'ipv1', 'ipv2',
                                          'measles1', 'measles2', 'opv1', 'opv2', 'opv3', 'pcv1',
                                          'pcv2', 'pcv3', 'penta1', 'penta2', 'penta3', 'rota1',
                                          'rota2', 'undervax', 'zerodose'),
                            denominator = c('dhis2', 'anc1', 'penta1'),
                            region = NULL) {

  admin_level <- attr(.data, 'admin_level')

  # check_cd_coverage(,data)
  indicator <- arg_match(indicator)
  denominator <- arg_match(denominator)

  if (admin_level != 'national' && is.null(region)) {
    cd_abort(c('x' = 'Region required for subnational analyses.'))
  }

  dhis2_col <- paste0('cov_', indicator, '_', denominator)
  survey_estimate_col <- paste0('r_', indicator)
  lower_ci_col <- paste0('ll_', indicator)
  upper_ci_col <- paste0('ul_', indicator)
  wuenic_col <- paste0('cov_', indicator, '_wuenic')

  .data %>%
    select(year, any_of(c('adminlevel_1', 'district', dhis2_col, survey_estimate_col, lower_ci_col, upper_ci_col, wuenic_col))) %>%
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
    select(-any_of(c('adminlevel_1', 'district')))
}


validate_column_existence <- function(.data, column) {
  if (!column %in% colnames(.data)) {
    cd_warn(c('!' = 'Column {.field {column}} not found in the data.'))
    return(NA_real_)
  }
  .data[[column]]
}

join_subnational_map <- function(.data, admin_level, map) {

  adminlevel_1 = admin_level_1 = NULL

  if (admin_level != 'national') {
    if (!is.null(map)) {
      .data <- .data %>%
        left_join(map, by = admin_level) %>%
        select(-adminlevel_1) %>%
        rename(adminlevel_1 = admin_level_1)
    }
  }

  .data
}

check_district_column <- function(.data, admin_level, dhis2_data) {

  adminlevel_1 = district = NULL

  if (admin_level == 'district' && !'district' %in% colnames(.data)) {
    .data <- dhis2_data %>%
      distinct(adminlevel_1, district) %>%
      left_join(.data, by = 'adminlevel_1', relationship = 'many-to-many')
  }
  .data
}
