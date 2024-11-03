#' Compute Population Metrics for DHIS-2 Data at Various Administrative Levels
#'
#' `prepare_population_metrics` processes DHIS-2 population data, and optionally
#' UN data, to calculate demographic metrics such as total population, live births,
#' growth rates, and related indicators. It supports calculation at different
#' administrative levels, including national, admin_level_1, or district levels.
#'
#' @param .data A `cd_data` tibble containing processed DHIS-2 health facility
#'   data with population indicators. This data is required for all administrative
#'   levels.
#' @param country_name A character string specifying the country for which
#'   demographic indicators are calculated.
#' @param admin_level A character string specifying the administrative level for
#'   calculation. Available options are:
#'   - `"national"`: Aggregates data at the country level, including comparisons
#'      with UN estimates.
#'   - `"admin_level_1"`: Aggregates data at the first administrative level,
#'      providing DHIS-2 projections only.
#'   - `"district"`: Aggregates data at the district level, providing DHIS-2
#'      projections only.
#' @param start_year The starting year for calculating demographic indicators
#'   (default is 2019).
#' @param end_year The ending year for calculations (default is 2023).
#'
#' @return A tibble of class `cd_population_metrics` containing demographic
#'   metrics for the specified country and years. This includes total population,
#'   live births, growth rates, and adjusted crude birth and death rates. For
#'   `national` data, both DHIS-2 and UN metrics are included; for `admin_level_1`
#'    and `district` levels, only DHIS-2 projections are calculated.
#'
#' @details
#' This function processes data as follows:
#'
#' 1. **DHIS-2 Data Preparation**:
#'    - Selects and renames essential demographic columns from DHIS-2 data.
#'    - Adds a `country` column based on `country_name`.
#'    - Ensures unique rows per combination of `district`, `adminlevel_1`, and
#'      `year`.
#'
#' 2. **Aggregate DHIS-2 Data**:
#'    - Summarizes population (`totpop_dhis2`) and live births (`totlivebirths_dhis2`)
#'      for each year at the specified administrative level, scaling values to thousands.
#'
#' 3. **UN Data Integration** (if `admin_level = "national"` only):
#'    - Filters UN data for `country_name` within the specified year range
#'      (`start_year` to `end_year`).
#'    - Renames columns with a `un_` prefix for differentiation from DHIS-2 metrics.
#'    - Merges DHIS-2 and UN data by `country` and `year` to provide national-level
#'      comparison.
#'
#' 4. **Output**:
#'    - The resulting tibble of class `cd_population_metrics` contains DHIS-2 and
#'      UN data (for national level), with calculated demographic metrics. For
#'      subnational levels, the tibble contains DHIS-2 projections only.
#'    - The tibble includes attributes indicating the administrative level used
#'      for aggregation.
#'
#' @examples
#' \dontrun{
#' # Calculate demographic metrics for Kenya at the national level
#' population_data <- prepare_population_metrics(
#'   .data = dhis2_data,
#'   country_name = "Kenya",
#'   admin_level = "national",
#'   start_year = 2019,
#'   end_year = 2023
#' )
#'
#' # Calculate demographic metrics for Kenya at the district level
#' population_data <- prepare_population_metrics(
#'   .data = dhis2_data,
#'   country_name = "Kenya",
#'   admin_level = "district",
#'   start_year = 2019,
#'   end_year = 2023
#' )
#' }
#'
#' @export
prepare_population_metrics <- function(.data,
                                       country_name,
                                       admin_level = c('national', 'admin_level_1', 'district'),
                                       start_year = 2019,
                                       end_year = 2023) {

  totlivebirths_dhis2 = total_pop = under5_pop = under1_pop = live_births =
    total_births = women15_49 = pop_rate = adminlevel_1 = district = year =
    pop_dhis2 = livebirths_dhis2 = totpop_dhis2 = totlivebirths_dhis2 = country =
    countrycode = iso3 = iso2 = un_population = un_births = un_popgrowth =
    un_cbr = un_cdr = NULL

  check_cd_data(.data)
  check_required(country_name)

  admin_level <- arg_match(admin_level)

  group_vars <- switch(admin_level,
                       national = c('country', 'year'),
                       admin_level_1 = c('country', 'adminlevel_1', 'year'),
                       district = c('country', 'adminlevel_1', 'district', 'year')
  )

  if (!is_scalar_character(country_name)) {
    cd_abort(c('x' = '{.arg country_name} should be a scalar string'))
  }

  columns <- c('district', 'adminlevel_1', 'year', 'total_pop', 'under5_pop', 'under1_pop', 'pop_rate', 'live_births', 'women15_49', 'total_births')

  # Prepare DHIS-2 Data
  dhis_data <- .data %>%
    select(any_of(columns)) %>%
    rename(
      pop_dhis2 = total_pop,
      under5_dhis2 = under5_pop,
      under1_dhis2 = under1_pop,
      livebirths_dhis2 = live_births,
      allbirths_dhis2 = total_births,
      wom15_49_dhis2 = women15_49,
      pop_rate_dhis2 = pop_rate
    ) %>%
    mutate(country = country_name) %>%
    arrange(adminlevel_1, district, year) %>%
    distinct(district, adminlevel_1, year, .keep_all = TRUE) %>%
    mutate(
      totpop_dhis2 = sum(pop_dhis2, na.rm = TRUE) / 1000,
      totlivebirths_dhis2 = sum(livebirths_dhis2, na.rm = TRUE) / 1000,
      .by = all_of(group_vars)
    ) %>%
    summarise(
      totpop_dhis2 = max(totpop_dhis2),
      totlivebirths_dhis2 = max(totlivebirths_dhis2),
      .by =all_of(group_vars)
    )

  if (admin_level == 'national') {
    # Prepare UN Data
    combined_data <- un_estimates %>%
      filter(year >= start_year & year <= end_year, country == country_name) %>%
      rename_with(~ paste0("un_", .), -c(country, year, countrycode, iso3, iso2)) %>%
      inner_join(dhis_data, by = c('country', 'year')) %>%
      arrange(country, year)
  } else {
    combined_data <- dhis_data
  }

  # Define the desired order for relocation
  desired_order <- c("country", "countrycode", "iso3", "iso2", "adminlevel_1",
                     "district", "year", "un_population", "totpop_dhis2", "un_births",
                     "totlivebirths_dhis2", "un_popgrowth", "un_cbr", "un_cdr")

  combined_data <- combined_data %>%
    relocate(any_of(desired_order))

  new_tibble(
    combined_data,
    class = 'cd_population_metrics',
    admin_level = admin_level
  )
}
