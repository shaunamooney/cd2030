#' Load UN Estimates
#'
#' Loads and processes UN estimates from a `.dta` file or in-memory data.
#' Filters by country and year, renames columns, and assigns a custom class.
#'
#' @param path Optional. File path to a `.dta` file.
#' @param .data Optional. A preloaded data frame.
#' @param country_iso Character. ISO3 code of the country.
#' @param start_year, end_year Integers. Year range to include.
#'
#' @return A tibble of class `cd_un_estimates`.
#' @export
load_un_estimates <- function(path = NULL, .data = NULL, country_iso, start_year, end_year) {

  year = iso3 = country = countrycode = iso2 = NULL

  check_required(country_iso)
  check_required(start_year)
  check_required(end_year)

  data <- load_data_or_file(path, .data)
  data %>%
    filter(year >= start_year & year <= end_year, iso3 == country_iso) %>%
    rename_with(~ paste0('un_', .), -c(country, year, countrycode, iso3, iso2)) %>%
    new_tibble(class = 'cd_un_estimates')
}

#' Load and Process Survey Data
#'
#' Loads national or subnational survey data, cleans labels and codes, and applies
#' country-specific adjustments for admin-level naming.
#'
#' @param path Optional. File path to a `.dta` file.
#' @param .data Optional. A preloaded data frame.
#' @param country_iso Character. Country ISO3 code.
#' @param admin_level Character. One of `'national'` or `'adminlevel_1'`.
#'
#' @return A tibble of class `cd_survey_data`.
#' @export
load_survey_data <- function(path = NULL, .data = NULL, country_iso, admin_level = c('national', 'adminlevel_1')) {

  year = iso = adminlevel_1 = admin1_code = NULL

  check_required(country_iso)
  data <- load_data_or_file(path, .data, country_iso)
  admin_level <- arg_match(admin_level)

  data <- data %>%
    select(-contains('24_35'))

  if (admin_level == 'adminlevel_1') {
    data <- data %>%
      # mutate(across(matches('^r_|^se_|^ll_|^ul_'), ~ .)) %>%
      select(-any_of(c('adminlevel_1', 'admin1_code'))) %>%
      separate_wider_regex(cols = level, patterns = c("[0-9]*", "\\s*", adminlevel_1 = ".*")) %>%
      filter(!(iso3 == 'TZA' & adminlevel_1 %in% c('Kaskazini Unguja','Pemba North',
                                                 'Kusini Pemba','Pemba South','Pemba',
                                                 'Kusini Unguja','Zanzibar North',
                                                 'Mjini Magharibi','Zanzibar South',
                                                 'Rest Zanzibar','Town West'))) %>%
      mutate(
        # admin1_code = as.integer(admin1_code),
        adminlevel_1 = if_else(iso3 == 'KEN', str_replace(adminlevel_1, '/', ''), adminlevel_1),
        adminlevel_1 = if_else(iso3 == 'KEN', str_replace(adminlevel_1,  '-', ' '), adminlevel_1),
        adminlevel_1 = if_else(iso3 == 'KEN', str_replace(adminlevel_1,  ' City', ''), adminlevel_1)
      )
  }

  new_tibble(
    data,
    class = 'cd_survey_data'
  )
}

#' Load and Filter Equity Data
#'
#' Loads survey equity data, removes unnecessary variables, and filters by country.
#'
#' @param path Optional. File path to a `.dta` file.
#' @param .data Optional. A preloaded data frame.
#' @param country_iso Character. ISO3 country code to filter by.
#'
#' @return A tibble of class `cd_equity_data`.
#' @export
load_equity_data <- function(path = NULL, .data = NULL, country_iso) {
  year = NULL

  check_required(country_iso)

  data <- load_data_or_file(path, .data, country_iso)

  data %>%
    separate_wider_regex(cols = level, patterns = c("[0-9]*", "\\s*", level = ".*")) %>%
    select(-contains('24_35')) %>%
    new_tibble(class = 'cd_equity_data')
}

#' Load WUENIC Immunization Data
#'
#' Loads and processes WUENIC estimates, computes dropout and zero-dose indicators,
#' and standardizes naming conventions.
#'
#' @param path Optional. File path to a `.dta` file.
#' @param .data Optional. A preloaded data frame.
#' @param country_iso Character. ISO3 country code.
#'
#' @return A tibble of class `cd_wuenic_data`.
#' @export
load_wuenic_data <- function(path = NULL, .data = NULL, country_iso) {
  check_required(country_iso)

  data <- load_data_or_file(path, .data)

  data %>%
    filter(iso == country_iso) %>%
    # Standardize column names
    rename_with(~ str_replace(., '^cov_cov', 'cov')) %>%
    rename_with(~ str_replace(., '^wuenic_(.*)', '\\1_wuenic')) %>%
    rename_with(~ str_replace(., '^(.*)_wuenic$', 'cov_\\1_wuenic')) %>%
    rename_with(~ str_replace(., '^cov_cov', 'cov')) %>%

    # Drop existing derived columns
    select(-matches('undervax|dropout|zerodose|cov_dropout_(measles|penta|penta3mcv1|penta1mcv1)_wuenic')) %>%

    # Compute immunization indicators
    mutate(
      # Zero-dose indicator
      cov_zerodose_wuenic = 100 - cov_penta1_wuenic,

      # Undervax indicator
      cov_undervax_wuenic = 100 - cov_penta3_wuenic,

      # Dropout indicators
      cov_dropout_penta13_wuenic = ((cov_penta1_wuenic - cov_penta3_wuenic) / cov_penta1_wuenic) * 100,
      cov_dropout_measles12_wuenic = ((cov_measles1_wuenic - cov_measles2_wuenic) / cov_measles1_wuenic) * 100,
      cov_dropout_penta3mcv1_wuenic = ((cov_penta3_wuenic - cov_measles1_wuenic) / cov_penta3_wuenic) * 100,
      cov_dropout_penta1mcv1_wuenic = ((cov_penta1_wuenic - cov_measles1_wuenic) / cov_penta1_wuenic) * 100
    ) %>%
    new_tibble(class = 'cd_wuenic_data')
}

#' Load Data from File or Use Provided Data
#'
#' Loads data from a `.dta` file or uses provided data directly. Optionally
#' assigns a custom class to the returned tibble.
#'
#' @param path Optional. File path to a `.dta` file.
#' @param .data Optional. A preloaded data frame or tibble.
#' @param country_iso Optional. Character. ISO3 for the country.
#'
#' @return A tibble, optionally with custom class.
#' @noRd
load_data_or_file <- function(path = NULL, .data = NULL, country_iso = NULL) {
  if (!is.null(path)) {
    check_file_path(path)
    ext <- tools::file_ext(path)
    if (tolower(ext) != 'dta') {
      cd_abort(c('x' = 'Only {.val .dta} files are supported.'))
    }
    .data <- haven::read_dta(path)
    if (!is.null(country_iso)) {
      .data <- .data %>% mutate(iso3 = country_iso)
    }
  }

  check_required(.data)

  .data %>%
    filter(if (is.null(country_iso)) TRUE else iso3 == country_iso)
}
