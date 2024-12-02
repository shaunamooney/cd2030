#' Save Processed Countdown 2030 Data to File
#'
#' `save_data` saves processed Countdown 2030 data to either an RDS or DTA file.
#' It checks that the input data is of class `cd_data` before saving.
#'
#' @param .data A tibble of class `cd_data` containing the processed data to be saved.
#' @param file A string. The path to the file where the data should be saved, with
#'   extension `.rds` or `.dta` depending on the desired format. Default is `master_dataset.dta`
#'
#' @return No return value. This function is called for its side effects (saving
#'   the data to an RDS or DTA file).
#'
#' @details
#' The function verifies that the `data` argument is of class `cd_data`. It then
#' saves the data in the specified file format based on the file extension.
#'
#' @examples
#' \dontrun{
#'   # Save processed Countdown 2030 data to an RDS file
#'   save_data(cd_data, "processed_cd2030_data.rds")
#'
#'   # Save processed Countdown 2030 data to a DTA file
#'   save_data(cd_data, "processed_cd2030_data.dta")
#' }
#'
#' @export
save_data <- function(.data, file = 'master_dataset.dta') {

  check_cd_data(.data)

  # Get file extension and determine save format
  file_extension <- file_ext(file)

  # Save based on the specified extension
  switch(
    file_extension,
    'rds' = saveRDS(.data, file = file),
    'dta' = haven::write_dta(.data, path = file),
    cd_abort(
      c('x' = 'Unsupported file format. Please use an ".rds" or ".dta" extension.')
    )
  )
}

#' Save DHIS2 Data to an Excel Workbook
#'
#' @param .data A list containing the structured DHIS2 data, including `admin`,
#'    `population`, `completeness`, and `service` datasets.
#' @param filename A string specifying the file path and name where the Excel
#'   workbook will be saved.
#'
#' @details
#' This function generates an Excel workbook containing multiple sheets for
#'   different types of DHIS2 data:
#' - **Admin Data**: Maps health districts to administrative units.
#' - **Population Data**: Contains official population data for denominators in
#'   health reporting.
#' - **Completeness Data**: Tracks reporting completeness metrics.
#' - **Service Data**: Records service delivery numbers.
#'
#' Each sheet is styled, with column headers, instructions, and frozen panes.
#' Instructions are added to guide users on how to input or interpret data.
#'
#' @return Saves the workbook to the specified filename.
#'
#' @examples
#' \dontrun{
#'   save_dhis2_excel(data, "dhis2_data.xlsx")
#' }
#'
#' @export
save_dhis2_excel <- function(.data, filename) {

  admin_instrunction <- 'This sheet serves to map Health districts (C) to administrative units (D) and other health system data. Please fill in the information for each column starting from Column C. Health division units name (districts) in Column C should be unique, while it is possible that a set of districts (C) correspond to the same administrative unit (D).
    CAUTION: The order and names of health division units (districts) entered in this sheet SHOULD BE the same across all the sheets. And ensure that health division units (districts) in column C match with the Coverage data into the sheets "Service_data", "Reporting_completeness" & "Population_data". Do not enter any data beyond the total number of health subnational units (districts) in your country. "Yellow cells" are "Drop-down list" options. Please use "PASTE SPECIAL" and paste "ONLY VALUES" when you copy/paste data. Do not create new columns, remove or displace existing columns.'
  service_instruction <- 'Enter the reported number of individuals that received the specific service for each month and by subnational unit (district).
    CAUTION: The name and order of districts SHOULD BE the same across all the sheets. Do not create a new column or displace an existing column. Do not enter any data beyond the total number of health subnational units in your country. Please use "PASTE SPECIAL" and paste "ONLY VALUES" in the appropriate cells when you copy/paste data.'
  population_instruction <- 'In this sheet, you will compile official population data used to produce denominators by the routine health information system in your country. These data are integrated into DHIS2 or other routine health data collection systems, and are usually derived from projections by the country\'s institute of statistics and/or demographic. Please enter data by district and year from year 2019 to 2023.
    WARNING: The order of the health districts MUST BE the same in all other sheets of this tool. Do not create new columns or move an existing column. Do not enter unnecessary data below the number of districts that exist in your country. Please use "PASTE SPECIAL" and paste "ONLY VALUES" in the appropriate cells when you copy/paste data.'

  wb <- createWorkbook()

  create_admin_sheet(
    wb = wb, .data$admin,  header_rows = c('district'), instruction = admin_instrunction
  )

  create_sheets(
    wb = wb, .data = .data$population, header_rows = c('district', 'year'),
    freeze_col = 3, instruction = population_instruction, instruction_row_height = 120
  )

  create_sheets(
    wb = wb, .data = .data$completeness, header_rows = c('district', 'year', 'month')
  )

  create_sheets(
    wb = wb, .data = .data$service, header_rows = c('district', 'year', 'month'),
    instruction = service_instruction, instruction_row_height = 140
  )

  saveWorkbook(wb, filename, overwrite = TRUE)
}

#' Create a Master Dataset from DHIS2 Data
#'
#' @param .data A list containing the structured DHIS2 data, including `admin`,
#'   `population`, `completeness`, and `service` datasets.
#'
#' @details
#' This function combines all DHIS2 data types into a single dataset. It pivots
#' data to a wide format, merges datasets on common keys, and ensures completeness
#' by filtering out rows with missing values in critical columns. The final dataset
#' is standardized for consistency and usability.
#'
#' @return A data frame representing the combined and cleaned DHIS2 dataset.
#'
#' @examples
#' \dontrun{
#'   master_dataset <- create_master_dataset(data)
#' }
#'
#' @export
save_dhis2_master_data <- function(.data) {
  service_data <- .data$service %>%
    select(-hfd_title, -hfd_sheet) %>%
    pivot_wider(names_from = hfd_id, values_from = value, values_fn = sum, values_fill = NA)

  completeness_data <- .data$completeness %>%
    select(-hfd_title, -hfd_sheet, -hfd_subtitle) %>%
    pivot_wider(names_from = hfd_id, values_from = value, values_fn = sum, values_fill = NA)

  population_data <- .data$population %>%
    select(-hfd_title, -hfd_sheet) %>%
    pivot_wider(names_from = hfd_id, values_from = value, values_fn = sum, values_fill = NA)

  merged_data <- service_data %>%
    left_join(completeness_data, by = c('district', 'year', 'month')) %>%
    left_join(population_data, by = c('district', 'year')) %>%
    left_join(admin_data, by = 'district', relationship = 'many-to-many') %>%
    rename_with(~ tolower(gsub(' ', '', .))) %>%
    standardize_data()

  new_countdown(merged_data)
}
