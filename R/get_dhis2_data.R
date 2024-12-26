#' Retrieve DHIS2 Health Facility Data
#'
#' `get_dhis2_hfd` retrieves health facility data (HFD) from the DHIS2 API for a
#' specific country and date range. It collects completeness, service, population,
#' and administrative data.
#'
#' @param country_iso3 Character. ISO3 code of the country.
#' @param start_date Character. Start date in "YYYY-MM-DD" format.
#' @param end_date Character. End date in "YYYY-MM-DD" format.
#' @param timeout Numeric. Timeout for API calls in seconds. Default is 60.
#'
#' @return A list of class `cd_dhis2_hfd` containing:
#'
#' - **`completeness`**: Data frame of completeness data.
#' - **`service`**: Data frame of service data.
#' - **`population`**: Data frame of population data.
#' - **`admin`**: Data frame of administrative data.
#'
#' @examples
#' \dontrun{
#' # Retrieve data for Kenya from January 1, 2020, to January 1, 2024
#' hfd_data <- get_dhis2_hfd("KEN", "2020-01-01", "2024-01-01")
#' }
#'
#' @export
get_dhis2_hfd <- function(country_iso3, start_date, end_date, timeout = 60) {
  completeness_data <- get_completeness_data(country_iso3, start_date, end_date, timeout)
  service_data <- get_service_data(country_iso3, start_date, end_date, timeout)
  population_data <- get_population_data(country_iso3, start_date, end_date, timeout)

  admin_data <- data_elements %>%
    filter(hfd_sheet == 'Admin_data', iso3 == country_iso3)

  structure(
    list(
      completeness = completeness_data,
      service = service_data,
      population = population_data,
      admin = admin_data
    ),
    class = 'cd_dhis2_hfd'
  )
}

#' Retrieve Service Data from DHIS2
#'
#' `get_service_data` retrieves service data from the DHIS2 API for a specified
#' country and date range.
#'
#' @param country_iso3 Character. ISO3 code of the country.
#' @param start_date Character. Start date in "YYYY-MM-DD" format.
#' @param end_date Character. End date in "YYYY-MM-DD" format.
#' @param timeout Numeric. Timeout for API calls in seconds. Default is 60.
#'
#' @return A data frame containing service data, with columns:
#' \describe{
#'   \item{district}{District name.}
#'   \item{year}{Year of the data.}
#'   \item{month}{Month of the data.}
#'   \item{hfd_id}{Health facility data identifier.}
#'   \item{hfd_title}{Title or description of the data element.}
#'   \item{hfd_sheet}{Category or sheet name for the data.}
#'   \item{value}{Value of the data element.}
#' }
#'
#' @examples
#' \dontrun{
#'   service_data <- get_service_data("KEN", "2020-01-01", "2024-01-01")
#' }
#'
#' @noRd
get_service_data <- function(country_iso3, start_date, end_date, timeout = 60) {
  service_data <- data_elements %>%
    filter(str_detect(hfd_sheet, '^Service_data'), iso3 == country_iso3)

  dt_element_ids <- service_data %>%
    filter(!is.na(element_id)) %>%
    pull(element_id)

  data <- get_analytics(
    dx %.d% dt_element_ids,
    pe %.d% 'all',
    ou %.d% 'LEVEL-2',
    startDate = start_date,
    endDate = end_date,
    timeout = timeout
  ) %>%
    left_join(service_data, by = c('dx' = 'element_id'), relationship = 'many-to-many') %>%
    left_join(organization_units, by = c('ou' = 'id', 'iso3')) %>%
    mutate(
      pe = ym(pe),
      year = year(pe),
      month = month(pe, label = TRUE, abbr = FALSE),
      year = as.integer(year),
      month = factor(month, levels = month.name)
    ) %>%
    select(-dx, -ou, -pe, -iso3, -element_name) %>%
    relocate(district, year, month, hfd_id, hfd_title, hfd_sheet) %>%
    arrange(year, district, month) %>%
    append_missing_columns(service_data)

  if (country_iso3 == 'KEN') {
    csection <- data %>%
      filter(hfd_id == 'Csection') %>%
      mutate(
        hfd_id = 'Instdelivery',
        hfd_title = 'Total number of  deliveries in health facilities'
      )

    data <- bind_rows(data, csection)
  }

  return(data)
}

#' Retrieve Reporting Completeness Data from DHIS2
#'
#' `get_completeness_data` retrieves completeness data from the DHIS2 API for a
#' specified country and date range.
#'
#' @param country_iso3 Character. ISO3 code of the country.
#' @param start_date Character. Start date in "YYYY-MM-DD" format.
#' @param end_date Character. End date in "YYYY-MM-DD" format.
#' @param timeout Numeric. Timeout for API calls in seconds. Default is 60.
#'
#' @return A data frame containing completeness data, with columns:
#' \describe{
#'   \item{district}{District name.}
#'   \item{year}{Year of the data.}
#'   \item{month}{Month of the data.}
#'   \item{hfd_id}{Health facility data identifier.}
#'   \item{hfd_title}{Title or description of the data element.}
#'   \item{hfd_sheet}{Category or sheet name for the data.}
#'   \item{hfd_subtitle}{Subtitle for the data column.}
#'   \item{value}{Value of the data element.}
#' }
#'
#' @examples
#' \dontrun{
#'   completeness_data <- get_completeness_data("KEN", "2020-01-01", "2024-01-01")
#' }
#'
#' @noRd
get_completeness_data <- function(country_iso3, start_date, end_date, timeout = 60) {
  completeness_data <- data_elements %>%
    filter(hfd_sheet == 'Reporting_completeness', iso3 == country_iso3)

  dt_element_ids <- completeness_data %>%
    filter(!is.na(element_id)) %>%
    pull(element_id)

  completeness_values <- c("REPORTING_RATE", "ACTUAL_REPORTS", "EXPECTED_REPORTS")
  completeness_values <- expand.grid(dt_element_ids, completeness_values) %>%
    mutate(combined = paste(Var1, Var2, sep = ".")) %>%
    pull(combined)

  periods <- format(seq(ymd(start_date), ymd(end_date), by = "month"),"%Y%m")

  data <- get_analytics(
    dx %.d% completeness_values,
    pe %.d% periods,
    ou %.d% 'LEVEL-2'
  ) %>%
    separate_wider_delim(dx, delim = '.', names = c('dx', 'dataset')) %>%
    left_join(completeness_data, by = c('dx' = 'element_id'), relationship = 'many-to-many') %>%
    left_join(organization_units, by = c('ou' = 'id', 'iso3')) %>%
    mutate(
      pe = ym(pe),
      year = year(pe),
      month = month(pe, label = TRUE, abbr = FALSE),
      year = as.integer(year),
      month = factor(month, levels = month.name)
    ) %>%
    select(-dx, -ou, -pe, -iso3, -element_name,) %>%
    relocate(district, year, month, hfd_id, hfd_title, hfd_sheet) %>%
    arrange(district, year, month) # %>%
  # append_missing_columns(completeness_data)

  if (country_iso3 == 'KEN') {
    # The generation in code for Reporting_completeness since ANC and instdelivery share common reporting
    anc <- data %>%
      filter(hfd_id == 'ANC') %>%
      mutate(
        hfd_id = 'Instdelivey',
        hfd_title = 'Institutional delivery reporting'
      )
    data <- bind_rows(data, anc)
  }

  data <- data %>%
    mutate(
      hfd_id = case_match(dataset,
                          'REPORTING_RATE' ~ paste0(hfd_id, '_reporting_rate'),
                          'ACTUAL_REPORTS' ~ paste0(hfd_id, '_reporting_received'),
                          'EXPECTED_REPORTS' ~ paste0(hfd_id, '_reporting_expected')),
      hfd_subtitle = case_match(dataset,
                                'REPORTING_RATE' ~ 'Reporting completeness rate (%)',
                                'ACTUAL_REPORTS' ~ 'Received number (#)',
                                'EXPECTED_REPORTS' ~ 'Expected number (#)')
    ) %>%
    select(-dataset)

  return(data)
}

#' Retrieve Population Data from DHIS2
#'
#' `get_population_data` retrieves population data from the DHIS2 API for a
#' specified country and date range.
#'
#' @param country_iso3 Character. ISO3 code of the country.
#' @param start_date Character. Start date in "YYYY-MM-DD" format.
#' @param end_date Character. End date in "YYYY-MM-DD" format.
#' @param timeout Numeric. Timeout for API calls in seconds. Default is 60.
#'
#' @return A data frame containing population data, with columns:
#' \describe{
#'   \item{district}{District name.}
#'   \item{year}{Year of the data.}
#'   \item{hfd_id}{Health facility data identifier.}
#'   \item{hfd_title}{Title or description of the data element.}
#'   \item{hfd_sheet}{Category or sheet name for the data.}
#'   \item{value}{Value of the data element.}
#' }
#'
#' @examples
#' \dontrun{
#'   population_data <- get_population_data("KEN", "2020-01-01", "2024-01-01")
#' }
#'
#' @noRd
get_population_data <- function(country_iso3, start_date, end_date, timeout = 60) {
  population_data <- data_elements %>%
    filter(hfd_sheet == 'Population_data', iso3 == country_iso3)

  pop_periods <- format(seq(ymd(start_date), ymd(end_date), by = 'year'), '%Y')

  dt_element_ids <- population_data %>%
    filter(!is.na(element_id)) %>%
    pull(element_id)

  data <- get_analytics(
    dx %.d% dt_element_ids,
    pe %.d% pop_periods,
    ou %.d% 'LEVEL-2'
  ) %>%
    right_join(population_data, by = c('dx' = 'element_id'), relationship = 'many-to-many') %>%
    left_join(organization_units, by = c('ou' = 'id', 'iso3')) %>%
    mutate(year = as.integer(pe)) %>%
    select(-dx, -ou, -pe, -iso3, -element_name) %>%
    relocate(district, year, hfd_id, hfd_title, hfd_sheet) %>%
    arrange(district, year) # %>%
  # append_missing_columns(population_data)

  return(data)
}

#' Append Missing Columns to a Dataset
#'
#' Adds missing columns to the dataset by aligning it with a reference dataset of
#' elements. This function ensures that all required health facility data (`hfd_id`)
#' columns are included in the provided data, even if they are missing. Missing
#' columns are filled with `NA` values.
#'
#' @param .data A data frame containing `hfd_id`, `district`, `year`, and `month`.
#' @param element_data A reference data frame containing all potential `hfd_id` values,
#'        along with their corresponding `hfd_title` and `hfd_sheet`.
#'
#' @details
#' The function performs the following steps:
#' 1. Identifies missing `hfd_id` columns by comparing `element_data` with `.data`.
#' 2. Creates new rows for each missing `hfd_id` value, preserving the structure of `.data`
#'    (e.g., `district`, `year`, `month`) and filling `value` with `NA`.
#' 3. Appends the missing rows to the original dataset.
#'
#' @return A data frame that includes all original rows from `.data` plus additional rows
#' for missing `hfd_id` values.
#'
#' @examples
#' \dontrun{
#'   # Example input data
#'   main_data <- tibble(
#'     district = c("District A", "District B"),
#'     year = c(2021, 2021),
#'     month = c("January", "January"),
#'     hfd_id = c("ANC", "ANC"),
#'     value = c(100, 200)
#'   )
#'
#'   reference_data <- tibble(
#'     hfd_id = c("ANC", "PNC", "Csection"),
#'     hfd_title = c("Antenatal Care", "Postnatal Care", "Caesarian Sections"),
#'     hfd_sheet = c("Service_data_1", "Service_data_1", "Service_data_1")
#'   )
#'
#'   # Use the function to append missing columns
#'   extended_data <- append_missing_columns(main_data, reference_data)
#' }
#'
#' @note
#' - The function assumes that the columns `district`, `year`, `month`, and `value`
#'   exist in `.data`.
#' - Ensures that the output dataset aligns with the full set of elements specified
#'   in `element_data`.
#'
#' @noRd
append_missing_columns <- function(.data, element_data) {

  dt_hfd_ids <- element_data %>%
    pull(hfd_id)
  missing_columns <- unique(.data$hfd_id)
  missing_columns <- dt_hfd_ids[!(dt_hfd_ids %in% missing_columns)]
  if (length(missing_columns) > 0) {
    columns_to_add <- element_data %>%
      filter(hfd_id %in% missing_columns) %>%
      select(hfd_id, hfd_title, hfd_sheet)

    data_items <- .data %>%
      distinct(district, year, month)

    extended_data_items <- columns_to_add %>%
      rowwise() %>%
      mutate(
        data = list(
          data_items %>%
            mutate(value = NA)
        )
      ) %>%
      unnest(data) %>%
      ungroup()

    bind_rows(.data, extended_data_items)
  }
}
