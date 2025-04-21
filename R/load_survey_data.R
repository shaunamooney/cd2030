#' Load UN Estimates
#'
#' This function reads and processes UN estimates from a DTA file, filters them
#' by country and year, and returns a cleaned tibble with a specific class.
#'
#' @param path Character. File path to the UN estimates `.dta` file.
#' @param country_iso Character. ISO3 code for the country of interest.
#' @param start_year Integer. Minimum year to include.
#' @param end_year Integer. Maximum year to include.
#'
#' @return A tibble of class `cd_un_estimates` containing cleaned UN estimate data
#'   for the specified country and year range.
#'
#' @export
load_un_estimates <- function(path, country_iso, start_year, end_year) {

  year = iso3 = country = countrycode = iso2 = NULL

  # Validate file path and extension
  check_file_path(path)

  file_ext <- tools::file_ext(path)

  # Only support .dta files
  if (tolower(file_ext) != 'dta') {
    cd_abort(c('x' = 'Unsupported file format. Only {.val .dta} files are allowed.'))
  }

  # Read the Stata file
  raw_data <- haven::read_dta(path)

  # Process the data
  create_un_estimates(raw_data, country_iso, start_year, end_year)
}

#' Create UN Estimates Tibble
#'
#' Filters, cleans, and renames columns for UN estimate data and assigns a custom
#'class.
#'
#' @param .data A data frame read from the UN estimates file.
#' @param country_iso Character. ISO3 country code.
#' @param start_year Integer. Starting year of data to keep.
#' @param end_year Integer. Ending year of data to keep.
#'
#' @return A tibble of class `cd_un_estimates`.
#'
#' @export
create_un_estimates <- function(.data, country_iso, start_year, end_year) {
  # Validate inputs
  check_required(.data)
  check_required(country_iso)
  check_required(start_year)
  check_required(end_year)

  # Clean and filter
  un_estimates <- .data %>%
    filter(year >= start_year & year <= end_year, iso3 == country_iso) %>%
    rename_with(~ paste0('un_', .), -c(country, year, countrycode, iso3, iso2))

  # Return new tibble with custom class
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
      filter(!(iso == 'TZA' & adminlevel_1 %in% c('Kaskazini Unguja','Pemba North',
                                                 'Kusini Pemba','Pemba South','Pemba',
                                                 'Kusini Unguja','Zanzibar North',
                                                 'Mjini Magharibi','Zanzibar South',
                                                 'Rest Zanzibar','Town West'))) %>%
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
    select(-contains('24_35'))

  new_tibble(
    survdata,
    class = 'cd_equity_data'
  )
}

##' Load WUENIC Data
#'
#' Loads and processes WUENIC data from a `.dta` file for a specific country.
#' This includes standardizing column names, removing unnecessary columns, and
#' computing indicators such as zero-dose, undervax, and dropout rates.
#'
#' @param path Character. File path to the `.dta` WUENIC data file.
#' @param country_iso Character. ISO3 country code.
#'
#' @return A tibble of class `cd_wuenic_data` with cleaned and computed WUENIC indicators.
#'
#' @export
load_wuenic_data <- function(path, country_iso) {
  check_file_path(path)

  ext <- tools::file_ext(path)

  if (tolower(ext) != 'dta') {
    cd_abort(c('x' = 'Unsupported file format. Only {.val .dta} is supported.'))
  }

  raw <- haven::read_dta(path)

  create_wuenic_estimates(raw, country_iso)
}

#' Create WUENIC Estimates Tibble
#'
#' Filters and transforms raw WUENIC data for a given country, computes key
#' immunization indicators, and assigns a custom class.
#'
#' @param .data A raw data frame from WUENIC source.
#' @param country_iso Character. ISO3 code for filtering.
#'
#' @return A tibble of class `cd_wuenic_data`.
#'
#' @export
create_wuenic_estimates <- function(.data, country_iso) {
  check_required(.data)
  check_required(country_iso)

  # Process data
  wuenic_data <- .data %>%
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
    )


  # Return tibble with custom class
  new_tibble(
    wuenic_data,
    class = 'cd_wuenic_data'
  )
}
