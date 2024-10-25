#' Load and Clean Countdown 2030 Excel Data
#'
#' This function loads and cleans data from an Excel file formatted for
#'   Countdown 2030. It processes the provided Excel sheets, checks for
#'   duplicates, merges data, and performs additional cleaning steps such as
#'   fixing column names, removing unnecessary columns, and handling reporting rates.
#'
#' @param filename A string. The path to the Excel file to be loaded.
#' @param admin_data A string. The name of the sheet containing administrative
#'   data. Default is 'Admin_data'.
#' @param population_data A string. The name of the sheet containing population
#'   data. Default is 'Population_data'.
#' @param reporting_completeness A string. The name of the sheet containing
#'   reporting completeness data. Default is 'Reporting_completeness'.
#' @param service_data A vector of strings. The names of the sheets containing
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
                            admin_data = 'Admin_data',
                            population_data = 'Population_data',
                            reporting_completeness = 'Reporting_completeness',
                            service_data = c('Service_data_1', 'Service_data_2', 'Service_data_3', 'Vaccine_stock_data')) {

  Month = district = Year = Country = First_admin_level = Total_Population =
    popgrowthrate = meanpopgrowthrate = pop_growth_rate = adminlevel_1 = NULL

  # Check if file exists
  if (!file.exists(filename)) stop('File not found!')

  # Combine all sheet names
  sheet_names <- c(service_data, reporting_completeness, population_data, admin_data)

  # Read all sheets using purrr::map(), loading each sheet into a list
  data <- map(sheet_names, ~ read_excel(filename, sheet = .x))

  # Assign names to the data frames in the list, corresponding to the sheet names
  names(data) <- sheet_names

  # Clean and process each data frame in the list
  dt <- map(data, ~ {
    .x %>%
      slice(-c(1, 2)) %>% # Remove the first two rows (header rows)
      map_df(~ type.convert(.x, as.is = TRUE)) %>% # Convert column types appropriately (e.g., character to numeric)
      filter(!if_all(everything(), is.na)) %>% # Remove rows that are entirely NA
      select(-matches('^\\.\\.')) %>% # Remove columns whose names start with `..` (potentially junk columns)
      rename_with(~ ifelse(tolower(.) %in% c('district', 'district_name'), 'district', .)) %>% # Rename 'District' or 'District_name' to 'district'
      rename_with(~ ifelse(tolower(.) == 'year', 'year', .)) %>% # Rename 'Year' to 'year'
      rename_with(~ ifelse(tolower(.) == 'month', 'month', .)) %>% # Rename 'Month' to 'month'
      filter(!is.na(district)) # Keep rows with non-missing 'district' values
  })

  # Define the key columns for each sheet to check for duplicates
  sheet_ids <- list(
    admin_data = c('district'),
    population_data = c('district', 'year'),
    service_data = c('district', 'year', 'month')
  )

  # Check for duplicates in the loaded data
  dt <- purrr::imap(dt, ~ {
    ids <- if (.y %in% c(admin_data, population_data)) sheet_ids[[.y]] else sheet_ids[['service_data']]
    if (anyDuplicated(.x[, ids]) > 0) {
      stop(paste('ERROR - THERE ARE DUPLICATES IN TERMS OF', paste(ids, collapse = ' '), 'IN SHEET', .y))
    }
    return(.x)
  })

  # Merge all datasets using purrr::reduce()
  merged_data <- reduce(sheet_names[-1], function(x, y) {
    merge_keys <- if (y == admin_data) {
      sheet_ids[['admin_data']]
    } else if (y == population_data) {
      sheet_ids[['population_data']]
    } else {
      sheet_ids[['service_data']]
    }
    left_join(x, dt[[y]], by = intersect(merge_keys, colnames(x)))
  }, .init = dt[[sheet_names[1]]]) # Start with the first service data sheet

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

  # Clean the 'Month' column by replacing special characters and standardizing month names
  merged_data <- merged_data %>%
    mutate(
      # Apply special character replacement and standardize month names
      Month = str_to_lower(replace_special_chars(Month)),
      Month = case_when(
        str_detect(Month, '^jan|^jav') ~ 'January',
        str_detect(Month, '^fev|^feb') ~ 'February',
        str_detect(Month, '^mar') ~ 'March',
        str_detect(Month, '^avr|^abr|^apr') ~ 'April',
        str_detect(Month, '^mai|^may') ~ 'May',
        str_detect(Month, '^juin|^jun') ~ 'June',
        str_detect(Month, '^juil|^jul') ~ 'July',
        str_detect(Month, '^aou|^ago|^aug') ~ 'August',
        str_detect(Month, '^set|^sep') ~ 'September',
        str_detect(Month, '^out|^oct') ~ 'October',
        str_detect(Month, '^nov') ~ 'November',
        str_detect(Month, '^dec|^dez') ~ 'December',
        .ptype = factor(levels = month.name, ordered = TRUE)
      ),
      # Convert columns that contain '_reporting_' to numeric type
      across(matches('_reporting_'), as.numeric)
    ) %>%
    arrange(district, Year, Month) %>% # Arrange the data by district, year, and month
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

  #### Remove empty columns and notify ####
  empty_columns <- names(merged_data)[colSums(is.na(merged_data)) == nrow(merged_data)]

  if (length(empty_columns) > 0) {
    # Notify the user about the empty columns being removed
    message('The following columns are empty: ', paste(empty_columns, collapse = ', '))

    # Remove the empty columns
    merged_data <- merged_data %>% select(-all_of(empty_columns))
  }

  # Perform final data processing: numeric rounding, renaming, and reordering columns
  columns_to_drop <- c(
    grep('^_', names(merged_data), value = TRUE),
    grep('_reporting_received$', names(merged_data), value = TRUE),
    grep('_reporting_expected$', names(merged_data), value = TRUE)
  )

  # Round numeric variables and calculate population growth rates
  merged_data <- merged_data %>%
    mutate(across(-c(Country, First_admin_level, district, Year, Month), ~ round(as.numeric(.), 1))) %>%
    mutate(
      # Calculate population growth rate for each district
      popgrowthrate = ((Total_Population / lag(Total_Population))^(1 / (as.numeric(Year) - lag(as.numeric(Year)))) - 1) * 100,
      meanpopgrowthrate = mean(popgrowthrate, na.rm = TRUE), # Mean population growth rate
      pop_growth_rate = ifelse(!is.na(meanpopgrowthrate), round(meanpopgrowthrate, 0.1), pop_growth_rate), # Replace pop_growth_rate if applicable
      .by = district # Group by district
    ) %>%
    # Remove unnecessary columns and rename the required ones
    select(-popgrowthrate, -meanpopgrowthrate, -pop_growth_rate, -all_of(columns_to_drop)) %>%
    rename(
      adminlevel_1 = any_of('First_admin_level'),
      ideliv = any_of('Instdelivery'),
      pop_rate = any_of('Pop_growth_rate'),
      total_pop = any_of('Total_Population'),
      under5_pop = any_of('Population_ under_5years'),
      under1_pop = any_of('Population_under_1year'),
      live_births = any_of('Live_births'),
      total_births = any_of('Total_births'),
      women15_49 = any_of('Women_15_49_years')
    ) %>%
    # Rename columns ending with '_reporting_rate' to '_rr'
    rename_with(~ gsub('_reporting_rate', '_rr', .x)) %>%
    # Rename specific column
    rename(idelv_rr = any_of('Instdelivey_rr')) %>%
    # Reorder columns and convert column names to lowercase
    select(Country, adminlevel_1, district, Year, Month, everything()) %>%
    rename_with(tolower)

  # Assign new class 'cd_data'
  structure(list(extracted_data = dt, merged_data = merged_data), class = 'cd_data')
}

#' Load Processed Countdown 2030 Data from RDS File
#'
#' This function loads processed Countdown 2030 data from an RDS file.
#'
#' @param file A string. The path to the RDS file containing the processed data.
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
load_data <- function(file) {
  merged_data <- readRDS(file = file)
  structure(list(merged_data = merged_data), class = 'cd_data')
}
