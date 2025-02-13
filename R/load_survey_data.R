#' Load UN Estimates
#'
#' This function loads and processes UN Estimate data from a specified file path.
#' It performs data cleaning, and renaming.
#'
#' @param path Character. File path to the UN Estimates data file (DTA format).
#' @param country_iso Character. ISO code of the country.
#' @param start_year Integer. The year to start filtering the data
#' @param end_year Integer. The year to start filtering the data
#'
#' @return A tibble of class `cd_un_estimates` containing cleaned and processed
#'   UN Estimate data.
#'
#' @export
load_un_estimates <- function(path, country_iso, start_year, end_year) {

  year = iso3 = country = countrycode = iso2 = NULL

  check_file_path(path)
  check_required(country_iso)
  check_required(start_year)
  check_required(end_year)

  file_extension <- tools::file_ext(path)
  un_estimates <- switch(file_extension,
                       'dta' = read_dta(path),
                       cd_abort(
                         'x' = 'Unsupported file format: please provide a DTA file.')
  )

  un_estimates <- un_estimates %>%
    filter(year >= start_year & year <= end_year, iso3 == country_iso) %>%
    rename_with(~ paste0("un_", .), -c(country, year, countrycode, iso3, iso2))

  new_tibble(
    un_estimates,
    class = 'cd_un_estimates'
  )
}

#' Load and Process Country-Specific Survey Data
#'
#' This function loads and processes survey data from a specified file path.
#' It performs data cleaning, renaming, and standardization while allowing for
#' the alignment of state codes.
#'
#' @param path Character. File path to the survey data file (DTA format).
#' @param country_iso Character. ISO code of the country.
#' @param admin_level description
#'
#' @return A tibble containing cleaned and processed survey data, with aligned state codes.
#'
#' @export
load_survey_data <- function(path, country_iso, admin_level = c('national', 'adminlevel_1')) {

  year = iso = adminlevel_1 = admin1_code = NULL

  check_file_path(path)
  check_required(country_iso)

  admin_level <- arg_match(admin_level)

  file_extension <- tools::file_ext(path)
  survdata <- switch(file_extension,
                       'dta' = read_dta(path),
                       cd_abort(
                         'x' = 'Unsupported file format: please provide a DTA file.')
  )

  survdata <- survdata %>%
    mutate(iso = country_iso) %>%
    select(-contains('24_35'))

  if (admin_level == 'adminlevel_1') {
    survdata <- survdata %>%
      # mutate(across(matches('^r_|^se_|^ll_|^ul_'), ~ .)) %>%
      select(-any_of(c('adminlevel_1', 'admin1_code'))) %>%
      separate_wider_delim(cols = 'level', delim = ' ', names = c('admin1_code', 'adminlevel_1'), too_many = 'merge', too_few = 'align_end') %>%
      filter(!(iso == 'TZA' & adminlevel_1 %in% c("Kaskazini Unguja","Pemba North",
                                                 "Kusini Pemba","Pemba South","Pemba",
                                                 "Kusini Unguja","Zanzibar North",
                                                 "Mjini Magharibi","Zanzibar South",
                                                 "Rest Zanzibar","Town West"))) %>%
      mutate(
        # admin1_code = as.integer(admin1_code),
        adminlevel_1 = if_else(iso == 'KEN', str_replace(adminlevel_1, '/', ''), adminlevel_1),
        adminlevel_1 = if_else(iso == 'KEN', str_replace(adminlevel_1,  '-', ' '), adminlevel_1),
        adminlevel_1 = if_else(iso == 'KEN', str_replace(adminlevel_1,  ' City', ''), adminlevel_1)
      )
  }

  new_tibble(
    survdata,
    class = 'cd_survey_data'
  )
}

#' Load and Process Country-Specific Survey Equity Data
#'
#' `load_equity_data` loads and processes survey data from a specified file path.
#' It performs data cleaning, renaming, and standardization while allowing for
#' the alignment of state codes.
#'
#' @param path Character. File path to the survey data file (DTA format).
#'
#' @return A tibble containing cleaned and processed survey data.
#'
#' @export
load_equity_data <- function(path) {

  year = NULL

  check_file_path(path)

  file_extension <- tools::file_ext(path)
  survdata <- switch(file_extension,
                     'dta' = read_dta(path),
                     cd_abort(
                       'x' = 'Unsupported file format: please provide a DTA file.')
  )

  survdata <- survdata %>%
    select(-contains("24_35"))

  new_tibble(
    survdata,
    class = 'cd_equity_data'
  )
}

#' Load WUENIC Data
#'
#' This function loads and processes WUENIC data from a specified file path.
#' It performs data cleaning, renaming, and computation of zero-dose, undervax,
#' and dropout indicators.
#'
#' @param path Character. File path to the WUENIC data file (DTA format).
#' @param country_iso Character. ISO code of the country.
#'
#' @return A tibble of class `wuenic_data` containing cleaned and processed WUENIC data.
#' @export
load_wuenic_data <- function(path, country_iso) {

  cov_penta1_wuenic = cov_penta3_wuenic = cov_measles1_wuenic = cov_measles2_wuenic =
    iso = NULL

  check_file_path(path)
  check_required(country_iso)

  # Read data based on file extension
  file_extension <- tools::file_ext(path)
  wuenic_data <- switch(
    file_extension,
    'dta' = haven::read_dta(path),
    cd_abort(
      'x' = 'Unsupported file format: please provide a DTA file.')
  )

  # Process data
  wuenic_data <- wuenic_data %>%
    # Rename columns
    rename_with(~ str_replace(., '^cov_cov', 'cov')) %>%
    rename_with(~ str_replace(., '^wuenic_(.*)', '\\1_wuenic')) %>%
    rename_with(~ str_replace(., '^(.*)_wuenic$', 'cov_\\1_wuenic')) %>%
    rename_with(~ str_replace(., '^cov_cov', 'cov')) %>%

    # Drop unnecessary columns
    select(-matches('.*undervax_wuenic|.*dropout_penta_wuenic|.*zerodose_wuenic|cov_dropout_measles_wuenic|cov_dropout_penta3mcv1_wuenic|cov_dropout_penta1mcv1_wuenic')) %>%
    # Compute indicators
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
    filter(iso == country_iso)

  # Return tibble with custom class
  new_tibble(
    wuenic_data,
    class = 'cd_wuenic_data'
  )
}
