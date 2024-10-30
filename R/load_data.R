#' Load and Clean Countdown 2030 Excel Data
#'
#' `load_excel_data` loads and processes data from an Excel file formatted
#' for Countdown 2030. It reads specified sheets, checks for duplicates, merges
#' data, and performs additional cleaning operations like fixing column names
#' and handling reporting rates.
#'
#' @param filename A string. The path to the Excel file to be loaded.
#' @param admin_sheet_name A string. The name of the sheet containing administrative
#'   data. Default is "Admin_data".
#' @param population_sheet_name A string. The name of the sheet containing population
#'   data. Default is "Population_data".
#' @param reporting_sheet_name A string. The name of the sheet containing reporting
#'   completeness data. Default is "Reporting_completeness".
#' @param service_sheet_names A vector of strings. The names of the sheets containing
#'   service data. Default is `c("Service_data_1", "Service_data_2", "Service_data_3",
#'   "Vaccine_stock_data")`.
#'
#' @return A data frame of class `cd_data`, containing cleaned and processed data.
#' @details
#' This function is designed to handle Countdown 2030 data sheets, ensuring
#' that each sheet is properly loaded, standardized, and merged into a single data
#' frame. It includes robust handling for issues such as duplicate entries, missing
#' values in key columns, and inconsistent formatting across sheets.
#'
#' @examples
#' \dontrun{
#'   # Load and clean data from a Countdown 2030 Excel file
#'   data <- load_excel_data("countdown2030_data.xlsx")
#' }
#'
#' @export

load_excel_data <- function(filename,
                            admin_sheet_name = 'Admin_data',
                            population_sheet_name = 'Population_data',
                            reporting_sheet_name = 'Reporting_completeness',
                            service_sheet_names = c('Service_data_1', 'Service_data_2', 'Service_data_3', 'Vaccine_stock_data')) {

  service_sheet_names <- c(service_sheet_names, reporting_sheet_name)

  # Combine all sheet names
  sheet_names <- c(service_sheet_names, population_sheet_name, admin_sheet_name)

  sheet_ids <- list2(
    !!admin_sheet_name := 'district',
    !!population_sheet_name := c('district', 'year'),
    service_data = c('district', 'year', 'month')
  )

  # Standardize merged data
  data <- load_excel_sheets(filename, sheet_names, sheet_ids) %>%
    merge_data(sheet_names, sheet_ids) %>%
    standardize_data()

  new_countdown(data)
}

#' Load Processed Countdown 2030 Data from RDS or DTA File
#'
#' `load_data` loads processed Countdown 2030 data from a specified file, either in
#' RDS or DTA format, and returns a tibble of class `cd_data`.
#'
#' @param path A string. The path to the file containing the processed data
#'   in either RDS or DTA format.
#'
#' @return A tibble of class `cd_data` containing the loaded data.
#' @details
#' This function first checks the file extension to determine the format, then reads
#' the data accordingly. It supports both RDS and Stata (DTA) file formats commonly
#' used for saving processed datasets.
#'
#' @examples
#' \dontrun{
#'   # Load previously processed data from an RDS or DTA file
#'   data <- load_data('processed_cd2030_data.rds')
#'   data <- load_data('processed_cd2030_data.dta')
#' }
#'
#' @export
load_data <- function(path) {

  check_file_path(path)

  # Determine file extension and load accordingly
  file_extension <- file_ext(path)
  final_data <- switch(file_extension,
                       'rds' = readRDS(file = path),
                       'dta' = read_dta(path),
                       cd_abort(
                         'x' = 'Unsupported file format: please provide an RDS or DTA file.')
                       )

  new_countdown(final_data)
}

#' Create Countdown 2030 Data Object
#'
#' `new_countdown` creates a tibble of class `cd_data`, with additional checks and
#' validation for required indicator groups used in analysis.
#'
#' @param .data A tibble. The cleaned and processed data to be converted to
#'   `cd_data` class.
#' @param call The call environment
#'
#' @return A tibble of class `cd_data` with an `indicator_groups` attribute containing
#'   validated and available indicator groups for analysis.
#'
#' @details
#' This function adds metadata to the data for available indicator groups by
#' checking each group against the column names in the provided data.
#' The groups are categorized as follows:
#'   - `anc`: ANC indicators (`anc1`, `anc4`, `ipt2`)
#'   - `idelv`: Indicators for delivery, stillbirth, and maternal deaths (`ideliv`, `instlivebirths`)
#'   - `vacc`: Vaccination indicators (`penta1`, `penta3`, etc.)
#'
#' These groups are verified for availability and attached as an attribute.
#'
#' @examples
#' \dontrun{
#'   # Convert processed data to `cd_data` class
#'   cd_data <- new_countdown(final_data)
#' }
#' @export
new_countdown <- function(.data, call = caller_env()) {
  # indicator_groups <- list(
  #   anc = c("anc1", "anc4", "ipt2"),
  #   idelv = c("ideliv", "instlivebirths", "csection", "total_stillbirth", "stillbirth_f", "stillbirth_m", "maternal_deaths"),
  #   pnc = c("pnc48h"),
  #   vacc = c("penta1", "penta3", "measles1", "bcg"),
  #   opd = c("opd_total", "opd_under5"),
  #   ipd = c("ipd_total", "ipd_under5")
  # )

  indicator_groups <- list(
    anc = c("anc1"),
    idelv = c("ideliv", "instlivebirths"),
    vacc = c('bcg', 'ipv1', 'ipv2', 'opv1', 'opv3', 'pcv1', 'pcv3', "penta1", "penta3", 'rota1', 'rota2')
  )

  # Identify missing columns for each indicator group
  missing_cols <- list_c(imap(indicator_groups, ~ setdiff(c(.x, paste0(.y, '_rr')), colnames(.data))))

  # If any required columns are missing, abort with a detailed message
  if (length(missing_cols) > 0) {
    cd_abort(
      c('x' = 'The following required columns are missing from the data:',
        '!' = paste(missing_cols, collapse = ', ')),
      call = call
    )
  }

  # Attach indicator groups as an attribute and create the cd_data class
  new_tibble(
    .data,
    indicator_groups = indicator_groups,
    class = "cd_data"
  )
}

#' Load and Clean Excel Sheets
#'
#' `load_excel_sheets` loads specified sheets from an Excel file, verifies their
#' existence, and applies basic cleaning to each sheet. It performs operations
#' such as validating the presence of required sheets, and checking for missing
#' sheets.
#'
#' @param path A string. The path to the Excel file.
#' @param sheet_names A vector of strings. The names of sheets to load.
#' @param sheet_ids A named list specifying columns for duplicate checks for
#' each sheet.
#' @param call The call environment for error handling (default is the caller's
#' environment).
#'
#' @return A list of tibbles, where each tibble corresponds to a cleaned sheet.
#'
#' @details
#' **Internal Steps**:
#' 1. **Validation of Input**: Ensures `sheet_names` is not empty. If it is, the
#'    function throws an error.
#' 2. **Sheet Presence Check**: Loads the available sheet names from the Excel
#'    file at `path`. Then, it checks whether each `sheet_name` specified exists in
#'    the file, storing any missing sheet names. If any specified sheet is missing,
#'    an error is triggered, displaying the names of missing sheets.
#' 3. **Loading and Cleaning Sheets**: For each sheet in `sheet_names`, the
#'    function calls [read_and_clean_sheet()] to load and clean the sheet. This
#'    process includes renaming columns, removing unnecessary rows, and converting
#'    data types as needed. Each sheet's data is then stored in a list with the
#'    sheet names as keys.
#'
#' @examples
#' \dontrun{
#'   # Load and clean specified sheets from an Excel file
#'   sheets_data <- load_excel_sheets(path = "data.xlsx",
#'                                    sheet_names = c("Sheet1", "Sheet2"),
#'                                    sheet_ids = list(Sheet1 = "id"))
#' }
#'
#' @noRd
load_excel_sheets <- function(path, sheet_names, sheet_ids, call = caller_env()) {

  check_required(sheet_names, call = call)

  # Validate if sheet names are provided
  if (length(sheet_names) == 0) {
    cd_abort(
      c("x" = "The {.arg sheet_names} argument is required and cannot be empty. Provide at least one sheet name."),
      call = call
    )
  }

  # Check if specified sheets are present in the file
  available_sheets <- excel_sheets(path)
  missing_sheets <- setdiff(sheet_names, available_sheets)
  if (length(missing_sheets) > 0) {
    cd_abort(
      c('x' = 'Missing sheets in file {.val {path}}:',
        '!' = paste(missing_sheets, collapse = ', ')),
      call = call
    )
  }

  # Log and load each sheet with basic cleaning steps
  cd_info(c("i" = "Loading sheets from {.val {path}}"))
  data <- map(sheet_names, ~ suppressMessages(read_and_clean_sheet(path, .x, sheet_ids, call)))
  names(data) <- sheet_names

  cd_info(c('i' = 'Successfully loaded all specified sheets'),)

  return(data)
}

#' Helper function to read and clean a single Excel sheet
#'
#' `read_and_clean_sheet` reads a specified Excel sheet, applies several cleaning
#' steps to standardize the data, and checks for duplicate entries based on
#' specified columns.
#'
#' @param path A string. Path to the Excel file.
#' @param sheet_name A string. Name of the sheet to read and clean.
#' @param sheet_ids A named list specifying the columns for duplicate checks
#' for each sheet.
#' @param call The call environment for error handling (default is the caller's
#' environment).
#'
#' @return A cleaned tibble for the specified sheet.
#' @details
#' **Internal Steps**:
#' 1. **Path and Parameter Validation**: Checks if the file path is valid and
#'    that `sheet_name` and `sheet_ids` are provided. Throws an error if any
#'    are missing.
#' 2. **Data Loading and Column Cleaning**:
#'    - Reads the specified Excel sheet into a tibble.
#'    - Converts column names to lowercase and removes spaces.
#'    - Removes the first two rows, which may contain headers or empty rows.
#'    - Drops any columns starting with "..", which are usually unwanted
#'      blank columns.
#' 3. **Column Standardization**:
#'    - Renames the `district_name` column to `district` if it exists.
#'    - Converts all columns except `country`, `first_admin_level`,
#'      `district`, and `month` to numeric, suppressing warnings for any
#'      non-numeric values.
#'    - Filters out rows that are entirely missing in key columns, as defined
#'      by `required_columns`.
#' 4. **Duplicate Check**: Identifies columns used for duplicate checks based on
#'    `sheet_ids`. If duplicates are found, it throws an error with details.
#'
#' @examples
#' \dontrun{
#'   # Load and clean a single sheet from an Excel file
#'   cleaned_data <- read_and_clean_sheet("data.xlsx", sheet_name = "Sheet1",
#'                                        sheet_ids = list(Sheet1 = "id"))
#' }
#'
#' @noRd
read_and_clean_sheet <- function(path, sheet_name, sheet_ids, call = caller_env()) {

  check_file_path(path, call = call)
  check_required(sheet_name, call = call)
  check_required(sheet_ids, call = call)

  # Columns that are required to have data
  required_columns <- c('district', 'year', 'month', 'first_admin_level', 'total_number_health_facilities')

  data <- read_excel(path, sheet = sheet_name) %>%  # Read sheet
    rename_with(~ tolower(gsub(' ', '', .))) %>%    # Convert column names to lower case and remove spaces
    slice(-c(1,2)) %>%                              # Remove the first two rows (header rows)
    select(-starts_with('..')) %>%                  # Remove columns with names starting with ".." (usually blank or junk columns)
    rename(district = any_of('district_name')) %>%  # Rename 'district_name' to 'district' if exists
    mutate(across(-any_of(c('country', 'first_admin_level', 'district', 'month')), ~ suppressWarnings(as.numeric(.)))) %>% # Convert other columns to numeric
    filter(!if_any(any_of(required_columns), is.na))# Remove rows with key columns missing

  # Determine columns for duplicate checking
  ids <- sheet_ids[[sheet_name]] %||% sheet_ids[['service_data']]

  if (anyDuplicated(data[ids]) > 0) {
    cd_abort(
      c("x" = "Duplicate entries found.",
        "!" = paste("Sheet:", sheet_name),
        "!" = paste("Key columns:", paste(ids, collapse = ", "))),
      call = call
    )
  }

  return(data)
}

#' Merge Data Frames
#'
#' `merge_data` merges a list of cleaned data frames by performing a series of
#' left joins using specified key columns. It ensures a consistent data structure
#' for further processing.
#'
#' @param .data A list of cleaned data frames to be merged.
#' @param sheet_names A vector of sheet names specifying the order of joins.
#' @param sheet_ids A named list containing column names to be used as join keys
#' for each sheet.
#' @param call The environment for error handling (default is the caller's
#' environment).
#'
#' @return A single merged data frame containing all specified sheets.
#' @details
#' **Internal Steps**:
#' 1. **Parameter Validation**: Checks that `.data`, `sheet_names`, and
#'    `sheet_ids` are provided. Throws an error if any of these are missing.
#' 2. **Iterative Left Joins**: Starts with the first data frame in `.data`
#'    (corresponding to the first element in `sheet_names`) as the base. Then, for
#'    each subsequent sheet name, performs a left join with the respective data
#'    frame in `.data` using key columns from `sheet_ids`. If key columns are
#'    missing for a sheet, defaults to keys from `sheet_ids[['service_data']]`.
#' 3. **Reordering and Sorting**: After merging, reorders columns to ensure
#'    `district`, `year`, and `month` appear first, and arranges the rows by
#'    `district`, `year`, and `month` for consistency.
#'
#' @examples
#' \dontrun{
#'   # Assuming `sheets` is a list of cleaned data frames
#'   merged_df <- merge_data(sheets, sheet_names = c("Sheet1", "Sheet2"),
#'                           sheet_ids = list(Sheet1 = "id"))
#' }
#'
#' @noRd
merge_data <- function(.data, sheet_names, sheet_ids, call = caller_env()) {

  district = year = month = NULL

  check_required(.data, call = call)
  check_required(sheet_names, call = call)
  check_required(sheet_ids, call = call)

  # Merge datasets iteratively using left join and the specified key columns
  merged_data <- reduce(
    .x = sheet_names[-1],
    .f = ~ left_join(.x, .data[[.y]], by = sheet_ids[[.y]] %||% sheet_ids[['service_data']]),
    .init = .data[[sheet_names[1]]]
  )

  merged_data %>%
    relocate(district, year, month) %>%
    arrange(district, year, month)
}

#' Data Preparation
#'
#' `standardize_data` standardizes and cleans a merged data frame by performing
#' multiple transformations such as column standardization, reporting rate
#' calculations, rounding, and renaming.
#'
#' @param .data A merged data frame to clean and standardize.
#' @param call The call environment for error handling (default is the caller's
#' environment).
#'
#' @return A final cleaned and standardized data frame.
#' @details
#' **Internal Steps**:
#' 1. **Month Column Standardization**: The `month` column is cleaned by replacing
#'    special characters and standardizing month names (e.g., abbreviations or
#'    language-specific variants).
#' 2. **Stillbirth Total Calculation**: Calculates `stillbirth_total` as the sum
#'    of `stillbirth_fresh` and `stillbirth_macerated`, handling missing values.
#' 3. **Missing Reporting Rate Calculation**: For columns ending with
#'    `_reporting_rate`, computes the reporting rate as `(received / expected) * 100`
#'    if missing and if `expected` is not zero.
#' 4. **Rounding Numeric Columns**: Rounds selected numeric columns, including
#'    those ending in `_rate`, to one decimal place.
#' 5. **Population Growth Rate**: Calculates annual population growth rate per
#'    district, storing the mean growth rate in `pop_growth_rate`.
#' 6. **Dropping Columns**: Drops intermediary columns such as `_reporting_received`
#'    and `_reporting_expected` after calculations.
#' 7. **Renaming Columns**: Renames columns for consistency, such as converting
#'    `first_admin_level` to `adminlevel_1`, and any `_reporting_rate` suffix to `_rr`.
#' 8. **Empty Column Check**: Identifies columns that are entirely missing
#'    (all values `NA`) and logs a message listing them.
#'
#' @examples
#' \dontrun{
#'   # Standardize a merged data frame
#'   standardized_df <- standardize_data(merged_data)
#' }
#'
#' @noRd
standardize_data <- function(.data, call = caller_env()) {

  country = month = district = . = year = total_population = pop_growth_rate = popgrowthrate =
    meanpopgrowthrate = adminlevel_1 = first_admin_level = stillbirth_fresh =
    stillbirth_macerated = NULL

  check_required(.data, call = call)

  data <- .data %>%
    mutate(
      # Clean the 'Month' column by replacing special characters and standardizing month names
      month = str_to_lower(replace_special_chars(month)),
      month = case_when(
        str_detect(month, '^jan|^jav') ~ 'January',
        str_detect(month, '^fev|^feb') ~ 'February',
        str_detect(month, '^mar') ~ 'March',
        str_detect(month, '^avr|^abr|^apr') ~ 'April',
        str_detect(month, '^mai|^may') ~ 'May',
        str_detect(month, '^juin|^jun') ~ 'June',
        str_detect(month, '^juil|^jul') ~ 'July',
        str_detect(month, '^aou|^ago|^aug') ~ 'August',
        str_detect(month, '^set|^sep') ~ 'September',
        str_detect(month, '^out|^oct') ~ 'October',
        str_detect(month, '^nov') ~ 'November',
        str_detect(month, '^dec|^dez') ~ 'December',
        .ptype = factor(levels = month.name, ordered = TRUE)
      ),

      # Calculate stillbirth_total as the row-wise sum of stillbirth_fresh and stillbirth_macerated
      stillbirth_total = rowSums(select(., stillbirth_fresh, stillbirth_macerated), na.rm = TRUE),

      # Calculate missing reporting rates based on received/expected values
      across(
        ends_with('_reporting_rate'),
        ~ {
          var_prefix <- str_replace(cur_column(), '_reporting_rate', '')
          received_col <- paste0(var_prefix, '_reporting_received')
          expected_col <- paste0(var_prefix, '_reporting_expected')

          # Calculate the reporting rate only where values are missing
          if_else(
            is.na(.),
            if_else(
              get(expected_col) == 0,
              NA_real_,
              get(received_col) / get(expected_col) * 100
            ),
            .
          )
        }
      ),

      # Round selected columns and all columns ending with "_rate" to one decimal place
      across(
        any_of(c(ends_with('_rate'), 'year', 'total_population', 'population_under_5years',
          'population_under_1year', 'live_births', 'total_births')),
        round, 1
      )
    ) %>%
    mutate(
      # Calculate yearly population growth rate within each district
      popgrowthrate = ((total_population / lag(total_population))^(1 / (year - lag(year))) - 1) * 100,
      meanpopgrowthrate = mean(popgrowthrate, na.rm = TRUE),

      # Update pop_growth_rate with rounded mean if applicable
      pop_growth_rate = round(if_else(!is.na(meanpopgrowthrate), meanpopgrowthrate, pop_growth_rate), 1),

      # Apply the calculations by district
      .by = district
    ) %>%
    # Drop the specified columns and intermediate variables
    select(-all_of(grep('_reporting_received$|_reporting_expected$', ., value = TRUE))) %>%
    rename(
      adminlevel_1 = any_of('first_admin_level'),
      ideliv = any_of('instdelivery'),
      pnc48h = any_of('pnc_48h'),
      pop_rate = any_of('pop_growth_rate'),
      total_pop = any_of('total_population'),
      under5_pop = any_of('population_under_5years'),
      under1_pop = any_of('population_under_1year'),
      live_births = any_of('live_births'),
      total_births = any_of('total_births'),
      women15_49 = any_of('women_15_49_years'),
      total_hospitals = any_of('number_hospitals'),
      total_hcenters = any_of('number_hcenters'),
      total_facilities = any_of('total_number_health_facilities'),
      total_profit = any_of('number_pfacilities_profit'),
      total_nonprofit = any_of('number_pfacilities_nonprofit'),
      total_physicians = any_of('total_physicians'),
      total_nurses = any_of('total_nurses_midwives'),
      total_nonclinique_phys = any_of('total_nonclinique_physicians'),
      total_beds = any_of('number_hospital_beds'),
      total_stillbirth = any_of('stillbirth_total'),
      stillbirth_f = any_of('stillbirth_fresh'),
      stillbirth_m = any_of('stillbirth_macerated')
    ) %>%
    rename_with(~ gsub('_reporting_rate', '_rr', .x)) %>%       # Rename columns ending with '_reporting_rate' to '_rr'
    rename(idelv_rr = any_of('instdelivey_rr')) %>%
    relocate(country, adminlevel_1, district, year, month) %>%
    arrange(district, year, month
  )

  # Notify about empty columns
  empty_columns <- names(data)[colSums(is.na(data)) == nrow(data)]
  if (length(empty_columns) > 0) {
    cd_info(
      c('!' = 'The following columns are empty: {.field {paste(empty_columns, collapse = ", ")}}'),
      call = call
    )
  }

  return(data)
}

#' Replace Special Characters in Text
#'
#' `replace_special_chars` replaces special characters in a text string with
#' their standard ASCII equivalents, helping to normalize text containing
#' diacritics or symbols for easier processing or comparison.
#'
#' @param text A character string or vector of character strings containing
#'   special characters.
#'
#' @details
#' This function specifically targets and replaces diacritic characters commonly
#'   found in various languages, along with some other symbols, converting them
#'   to their simpler ASCII equivalents:
#' - **Characters replaced with 'a'**: `á`, `ã`, `à`, `Á`, `À`, `Ã`, `â`, `Â`
#' - **Characters replaced with 'e'**: `é`, `É`, `ê`, `è`, `È`, `Ê`, `&`
#' - **Characters replaced with 'i'**: `í`, `Í`, `ï`, `ì`, `Ï`, `Ì`
#' - **Characters replaced with 'o'**: `ó`, `ö`, `õ`, `ò`, `Ó`, `Õ`, `Ò`, `ô`, `Ô`, `Ö`, `¢`
#' - **Characters replaced with 'u'**: `ú`, `ù`, `û`, `ü`, `Ù`, `Ú`, `Ü`, `Û`
#' - **Character replaced with 'n'**: `ñ`
#'
#' @return A character string or vector of character strings with the special
#' characters replaced by their ASCII equivalents.
#'
#' @examples
#' replace_special_chars("áéíóúñÁÉÍÓÚÑ")  # Returns "aeiounAEIOUN"
#' replace_special_chars("Hëllò Wörld")   # Returns "Hello World"
#'
#' @noRd
replace_special_chars <- function(text) {
  str_replace_all(text, c(
    '[\u00E1\u00E3\u00E0\u00C1\u00C0\u00C3\u00E2\u00C2]' = 'a',  # áãàÁÀÃâÂ
    '[\u00E9\u00C9\u00EA\u00E8\u00C8\u00CA&]' = 'e',            # éÉêèÈÊ
    '[\u00ED\u00CD\u00EF\u00EC\u00CF\u00CC]' = 'i',            # íÍïìÏÌ
    '[\u00F3\u00F6\u00F5\u00F2\u00D3\u00D5\u00D2\u00F4\u00D4\u00D6\u00A2]' = 'o',  # óöõòÓÕÒôÔÖ¢
    '[\u00FA\u00F9\u00FB\u00FC\u00D9\u00DA\u00DC\u00DB]' = 'u',  # úùûüÙÚÜÛ
    '\u00F1' = 'n'  # ñ
  ))
}
