#' Load and Clean Countdown 2030 Excel Data
#'
#' This function loads and cleans data from an Excel file formatted for
#'   Countdown 2030. It processes the provided Excel sheets, checks for
#'   duplicates, merges data, and performs additional cleaning steps such as
#'   fixing column names, removing unnecessary columns, and handling reporting rates.
#'
#' @param filename A string. The path to the Excel file to be loaded.
#' @param admin_sheet_name A string. The name of the sheet containing administrative
#'   data. Default is 'Admin_data'.
#' @param population_sheet_name A string. The name of the sheet containing population
#'   data. Default is 'Population_data'.
#' @param reporting_sheet_name A string. The name of the sheet containing
#'   reporting completeness data. Default is 'Reporting_completeness'.
#' @param service_sheet_names A vector of strings. The names of the sheets containing
#'   service data. Default is `Service_data_1`, `Service_data_2`, `Service_data_3`,
#'     `Vaccine_stock_data`.
#'
#' @return A data of class `cd_data`.
#' @details The function performs several cleaning steps, including:
#'   - Removing header rows
#'   - Converting column types appropriately (e.g., converting characters to numeric)
#'   - Removing entirely missing rows and columns
#'   - Renaming columns (e.g., renaming 'District' or 'District_name' to 'district')
#'   - Checking for duplicate entries based on key columns
#'   - Calculating reporting rates if necessary using expected and received values
#'   - Replacing special characters in the 'Month' column and standardizing month names
#'
#' @examples
#' \dontrun{
#'   # Load and clean data from a Countdown 2030 Excel file
#'   data <- load_excel_data('countdown2030_data.xlsx')
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

  # Load data from Excel sheets
  extracted_data <- load_excel_sheets(filename, sheet_names)

  sheet_ids <- list2(
    !!admin_sheet_name := 'district',
    !!population_sheet_name := c('district', 'year'),
    service_data = c('district', 'year', 'month')
  )

  # Standardize merged data
  final_data <- extracted_data%>%
    clean_data() %>%
    check_for_duplicates(sheet_ids) %>%
    merge_data(sheet_names, sheet_ids) %>%
    data_preparation()

  check_indicator <- final_data %>%
    create_indicator()

  # Assign new class 'cd_data'
  structure(
    list(
      check_indicator = check_indicator,
      merged_data = final_data),
    class = 'cd_data')
}

#' Load Processed Countdown 2030 Data from RDS File
#'
#' This function loads processed Countdown 2030 data from an RDS file.
#'
#' @param path A string. The path to the RDS file containing the processed data.
#'
#' @return A list of class `cd_data` containing the merged data.
#'
#' @examples
#' \dontrun{
#'   # Load previously processed data from an RDS file
#'   data <- load_data('processed_cd2030_data.rds')
#' }
#'
#' @export
load_data <- function(path) {

  check_required(path)

  # validate path argument
  if (!file.exists(path)) {
    cd_abort(
      c("x" = "The specified file {.val {path}} does not exist. Please provide a valid file path."),
      call = call
    )
  }
  final_data <- readRDS(file = path)

  check_indicator <- final_data %>%
    create_indicator()

  # Assign new class 'cd_data'
  structure(
    list(
      check_indicator = check_indicator,
      merged_data = final_data),
    class = 'cd_data')
}

#' Load Excel Sheets
#'
#' @param path Path to the excel file.
#' @param sheet_names Vector of sheet names to load.
#' @param call The call environment.
#'
#' @return List of tibbles from each sheet.
#'
#' @noRd
load_excel_sheets <- function(path, sheet_names, call = caller_env()) {

  check_required(path, call = call)
  check_required(sheet_names, call = call)

  # validate path argument
  if (!file.exists(path)) {
    cd_abort(
      c("x" = "The specified file {.val {path}} does not exist. Please provide a valid file path."),
      call = call
    )
  }

  # Validate sheet_names argument
  if (length(sheet_names) == 0) {
    cd_abort(
      c("x" = "The {.arg sheet_names} argument is required and cannot be empty. Provide at least one sheet name."),
      call = call
    )
  }

  # Verify existence of specified sheets
  available_sheets <- excel_sheets(path)
  missing_sheets <- setdiff(sheet_names, available_sheets)
  if (length(missing_sheets) > 0) {
    cd_abort(
      c('x' = 'Missing sheets in file {.val {path}}:',
        '!' = paste(missing_sheets, collapse = ', ')),
      call = call
    )
  }

  # Log and load each sheet
  cd_info(c("i" = "Loading sheets from {.val {path}}"))
  data <- map(sheet_names, ~ suppressMessages(read_excel(path, sheet = .x)))
  names(data) <- sheet_names
  cd_info(
    c('i' = 'Successfully loaded all specified sheets'),
  )

  return(data)
}

#' Clean Data
#'
#' @param .data List of tibbles to clean.
#'
#' @return Cleaned list of tibbles.
#'
#' @noRd
clean_data <- function(.data, call = caller_env()) {

  check_required(.data, call = call)

  # Ensure input is a list of data frames
  if (!is.list(.data) || !all(purrr::map_lgl(.data, is.data.frame))) {
    cd_abort('The input data must be a list of data frames.')
  }

  # Apply cleaning steps to each data frame in the list
  dt <- map(.data, ~ {

    # Define required columns
    required_columns <- c('district', 'year', 'month', 'first_admin_level', 'total_number_health_facilities')

    .x %>%
      rename_with(tolower) %>%                                      # Convert column names to lower case
      slice(-c(1, 2)) %>%                                           # Remove the first two rows (header rows)
      select(-starts_with('..')) %>%                                # Remove columns with names starting with ".." (usually blank or junk columns)
      mutate(across(everything(), type.convert, as.is = TRUE)) %>%  # Convert column types appropriately (e.g., character to numeric)
      rename(district = any_of('district_name')) %>%                # Renames district_name to district if the variable exists.
      filter(
        !if_all(everything(), is.na),                               # Remove rows that are entirely NA
        if_any(all_of(intersect(required_columns, names(data))), ~ !is.na(.)) # Remove rows where any of these variables are empty
      )
  })

  return(dt)
}

#' Check for Duplicates
#'
#' @param .data List of data frames
#' @param sheet_ids The ids for checking duplicates
#' @param call The call environment.
#'
#' @return Data frames checked for duplicates.
#'
#' @noRd
check_for_duplicates <- function(.data, sheet_ids, call = caller_env()) {

  check_required(.data, call = call)
  check_required(sheet_ids, call = call)

  # Check for duplicates in the loaded data
  dt <- imap(.data, ~ {
    ids <- if (.y %in% names(sheet_ids)) sheet_ids[[.y]] else sheet_ids[['service_data']]
    if (anyDuplicated(.x[ids]) > 0) {
      cd_abort(
        c("x" = "Duplicate entries found.",
          "!" = paste("Sheet:", .y),
          "!" = paste("Key columns:", paste(ids, collapse = ", "))),
        call = call
      )
    }
    return(.x)
  })
  return(dt)
}

#' Merge Data Frames
#'
#' @param .data List of cleaned data frames.
#' @param sheet_names Vector of sheet names.
#' @param sheet_id The ids to be used in joining
#' @param call The call environment.
#'
#' @return Merged data frame.
#'
#' @noRd
merge_data <- function(.data, sheet_names, sheet_ids, call = caller_env()) {

  check_required(.data, call = call)
  check_required(sheet_names, call = call)
  check_required(sheet_ids, call = call)

  # Merge all datasets using purrr::reduce()
  merged_data <- reduce(sheet_names[-1], function(x, y) {
    merge_keys <- if (y %in% names(sheet_ids)) sheet_ids[[y]] else sheet_ids[['service_data']]
    left_join(x, .data[[y]], by = intersect(merge_keys, colnames(x)))
  }, .init = .data[[sheet_names[1]]]) # Start with the first service data sheet

  return(merged_data)
}

#' Data Preparation
#'
#' @param .data Merged data frame to clean and standardize.
#'
#' @return Final cleaned and standardized data frame.
#'
#' @noRd
data_preparation <- function(.data, call = caller_env()) {

  country = month = district = year = total_population = pop_growth_rate = popgrowthrate =
    meanpopgrowthrate = adminlevel_1 = first_admin_level = stillbirth_fresh =
    stillbirth_macerated = NULL

  check_required(.data, call = call)

  merged_data <- .data %>%
    # Clean the 'Month' column by replacing special characters and standardizing month names
    mutate(
      # Apply special character replacement and standardize month names
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
      )
    ) %>%
    # Calculate stillbirth_total separately to avoid issues
    mutate(
      stillbirth_total = if_all(c("stillbirth_fresh", "stillbirth_macerated"), ~ !is.na(.)) *
        rowSums(select(cur_data(), stillbirth_fresh, stillbirth_macerated), na.rm = TRUE)
    ) %>%
    rowwise() %>%
    # Calculate missing reporting rates if necessary, using the received/expected values
    mutate(across(
      ends_with('_reporting_rate'),
      ~ ifelse(is.na(.),
               ifelse(get(cur_column() %>% str_replace('_reporting_rate', '_reporting_expected')) == 0, NA_real_,
                      get(cur_column() %>% str_replace('_reporting_rate', '_reporting_received')) /
                        get(cur_column() %>% str_replace('_reporting_rate', '_reporting_expected')) * 100
               ),
               .
      )
    )) %>%
    ungroup()

  # Notify about empty columns
  empty_columns <- names(merged_data)[colSums(is.na(merged_data)) == nrow(merged_data)]

  if (length(empty_columns) > 0) {
    cd_info(
      c('!' = 'The following columns are empty: {.field {paste(empty_columns, collapse = ", ")}}'),
      call = call
    )
  }
  # Perform final data processing: numeric rounding, renaming, and reordering columns
  columns_to_drop <- c(
    grep('^_', names(merged_data), value = TRUE),
    grep('_reporting_received$', names(merged_data), value = TRUE),
    grep('_reporting_expected$', names(merged_data), value = TRUE)
  )

  # Round numeric variables and calculate population growth rates
  merged_data <- merged_data %>%
    mutate(
      # Calculate population growth rate for each district
      popgrowthrate = ((total_population / lag(total_population))^(1 / (as.numeric(year) - lag(as.numeric(year)))) - 1) * 100,
      meanpopgrowthrate = mean(popgrowthrate, na.rm = TRUE), # Mean population growth rate
      pop_growth_rate = ifelse(!is.na(meanpopgrowthrate), round(meanpopgrowthrate, 0.1), pop_growth_rate), # Replace pop_growth_rate if applicable
      .by = district # Group by district
    ) %>%
    # Remove unnecessary columns and rename the required ones
    select(-popgrowthrate, -meanpopgrowthrate, -pop_growth_rate, -all_of(columns_to_drop)) %>%
    mutate(across(-c(country, first_admin_level, district, year, month), ~ round(suppressWarnings(as.numeric(.)), 1))) %>%
    rename(
      adminlevel_1 = any_of('first_admin_level'),
      ideliv = any_of('instdelivery'),
      pnc48 = any_of('pnc_48h'),
      pop_rate = any_of('pop_growth_rate'),
      total_pop = any_of('total_population'),
      under5_pop = any_of('population_ under_5years'),
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
    arrange(district, year, month)
}

# Function to replace special characters with base letters
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

create_indicator <- function(.data, call = caller_env()) {

  district = year = NULL

  check_required(.data, call = call)

  # Define variable groups
  ancvargroup <- c('anc1')
  idelvvargroup <- c('ideliv', 'instlivebirths')
  vaccvargroup <- c('opv1', 'opv2', 'opv3', 'penta1', 'penta2', 'penta3', 'measles1', 'pcv1', 'pcv2', 'pcv3', 'measles2', 'bcg', 'rota1', 'rota2', 'ipv1', 'ipv2')
  allindicators <- c(ancvargroup, idelvvargroup, vaccvargroup)

  lastyear <- max(.data$year, na.rm = TRUE)

  outliers_summary <- .data %>%
    select(district, year, allindicators) %>%
    mutate(
      # Calculate `_mad` and `_med` for each indicator in `allindicators` up to `lastyear`
      across(allindicators, ~ mad(ifelse(year < lastyear, ., NA_real_), na.rm = TRUE),
             .names = "{.col}_mad"),
      across(allindicators, ~ median(ifelse(year < lastyear, ., NA_real_), na.rm = TRUE),
             .names = "{.col}_med"),

      # Calculate lower and upper bounds for outliers
      across(allindicators, ~ get(paste0(cur_column(), "_med")) - 5 * get(paste0(cur_column(), "_mad")),
             .names = "{.col}_outlb5std"),
      across(allindicators, ~ get(paste0(cur_column(), "_med")) + 5 * get(paste0(cur_column(), "_mad")),
             .names = "{.col}_outub5std"),

      # Flag values as outliers if they fall outside the calculated bounds
      across(allindicators, ~ ifelse(!is.na(.) & (. < get(paste0(cur_column(), "_outlb5std")) | . > get(paste0(cur_column(), "_outub5std"))), 1, 0),
             .names = "{.col}_outlier5std"),

      # Flag missing values
      across(allindicators, ~ ifelse(is.na(.), 1, 0),
             .names = "mis_{.col}"),

      .by = district
    )

  return(outliers_summary)
}
