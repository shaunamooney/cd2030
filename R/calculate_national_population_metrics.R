#' Compute National Population Metrics for DHIS-2 and UN Data
#'
#' `calculate_national_population_metrics` processes DHIS-2 and UN population datasets to
#' calculate national-level population metrics, including total population, live births,
#' growth rates, and related demographic indicators.
#'
#' @param .data A `cd_data` tibble containing processed DHIS-2 health facility data
#'   with population indicators.
#' @param country_name A character string specifying the country for which demographic
#'   indicators are calculated.
#' @param start_year The starting year for calculating demographic indicators (default is 2019).
#' @param end_year The ending year for calculations (default is 2023).
#'
#' @return A tibble of class `cd_national_denominators` containing merged data from DHIS-2
#'   and UN population estimates, with calculated demographic metrics such as total population,
#'   live births, growth rates, and adjusted crude birth and death rates.
#'
#' @details This function performs the following steps:
#'   1. **Prepare DHIS-2 Data**:
#'      - Selects and renames key demographic columns from DHIS-2 data.
#'      - Adds a `country` column based on the specified `country_name`.
#'      - Retains only the first row for each unique combination of `district`,
#'        `adminlevel_1`, and `year` to avoid duplicates.
#'
#'   2. **Compute DHIS-2 National Totals**:
#'      - Summarizes population (`totpop_dhis2`) and live births (`totlivebirths_dhis2`)
#'        at the national level for each year, scaling values to thousands.
#'
#'   3. **Prepare and Merge UN Data**:
#'      - Filters UN data for `country_name` and specified years (`start_year` to `end_year`).
#'      - Renames columns in UN data with a `un_` prefix to distinguish them from DHIS-2 metrics.
#'      - Merges DHIS-2 and UN data by `country` and `year`, creating a unified dataset.
#'
#'   4. **Return Output**:
#'      - The output is a `cd_national_denominators` tibble containing DHIS-2 and UN
#'        metrics for the specified country and years, ready for further analysis.
#'
#' @examples
#' \dontrun{
#' # Calculate national demographic metrics for Kenya
#' national_population_data <- compute_national_population_metrics(
#'   .data = dhis2_data,
#'   country_name = "Kenya",
#'   start_year = 2019,
#'   end_year = 2023
#' )
#' }
#'
#' @export
calculate_national_population_metrics <- function(.data, country_name, start_year = 2019, end_year = 2023) {

  totlivebirths_dhis2 = total_pop = under5_pop = under1_pop = live_births =
    total_births = women15_49 = pop_rate = adminlevel_1 = district = year =
    pop_dhis2 = livebirths_dhis2 = totpop_dhis2 = totlivebirths_dhis2 = country =
    countrycode = iso3 = iso2 = un_population = un_births = un_popgrowth =
    un_cbr = un_cdr = NULL

  check_cd_data(.data)
  check_required(country_name)

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
    filter(row_number() == 1, .by = c(district, adminlevel_1, year))

  # Summarize and scale population metrics
  national_data <- dhis_data %>%
    mutate(
      totpop_dhis2 = sum(pop_dhis2, na.rm = TRUE) / 1000,
      totlivebirths_dhis2 = sum(livebirths_dhis2, na.rm = TRUE) / 1000,
      .by = year
    ) %>%
    summarise(
      totpop_dhis2 = max(totpop_dhis2),
      totlivebirths_dhis2 = max(totlivebirths_dhis2),
      .by = c(country, year)
    ) # %>%
    # mutate(
    #   totpop_dhis2 = labelled::var_label(totpop_dhis2, "Tot Pop - Projection (in 1000s)"),
    #   totlivebirths_dhis2 = labelled::var_label(totlivebirths_dhis2, "Tot Live Births - Projection (in 1000s)")
    # )

  # Prepare UN Data
  combined_data <- un_estimates %>%
    filter(year >= start_year & year <= end_year, country == country_name) %>%
    rename_with(~ paste0("un_", .), -c(country, year, countrycode, iso3, iso2)) %>%
    left_join(national_data, by = c('country', 'year')) %>%
    arrange(country, year) %>%
    relocate(country, countrycode, iso3,iso2, year, un_population, totpop_dhis2, un_births, totlivebirths_dhis2, un_popgrowth, un_cbr, un_cdr)

  new_tibble(
    combined_data,
    class = 'cd_national_denominators'
  )
}


compute_denominator <- function(.data) {

  pop_dhis2 = under5_dhis2 = under1_dhis2 = livebirths_dhis2 = allbirths_dhis2 =
    wom15_49_dhis2 = year = un_under5y = un_population = un_under1y = un_wom15_49 =
    tot_under5_dhis2 = tot_pop_dhis2 = tot_under1_dhis2 = tot_wom15_49_dhis2 =
    tot_livebirths_dhis2 = tot_allbirths_dhis2 = national = totcbr_dhis2 =
    totpopgrowth = country = adminlevel_1 = district = totpop_dhis2 = totunder5_dhis2 =
    totunder1_dhis2 = totlivebirths_dhis2 = un_births = totwom15_49_dhis2 =
    un_popgrowth = un_cbr = NULL

  .data %>%
    mutate(
      tot_pop_dhis2 = sum(pop_dhis2, na.rm = TRUE) / 1000,
      tot_under5_dhis2 = sum(under5_dhis2, na.rm = TRUE) / 1000,
      tot_under1_dhis2 = sum(under1_dhis2, na.rm = TRUE) / 1000,
      tot_livebirths_dhis2 = sum(livebirths_dhis2, na.rm = TRUE) / 1000,
      tot_allbirths_dhis2 = sum(allbirths_dhis2, na.rm = TRUE) / 1000,
      tot_wom15_49_dhis2 = sum(wom15_49_dhis2, na.rm = TRUE) / 1000,
      .by = year
    ) %>%

    # Calculate Demographic Indicators from UN and DHIS-2 Data
    mutate(
      un_percent_under5 = 100 * un_under5y / un_population,
      un_percent_under1 = 100 * un_under1y / un_population,
      un_percent_wom15_49 = 100 * un_wom15_49 / un_population,
      totpercent_under5 = 100 * tot_under5_dhis2 / tot_pop_dhis2,
      totpercent_under1 = 100 * tot_under1_dhis2 / tot_pop_dhis2,
      totpercent_wom15_49 = 100 * tot_wom15_49_dhis2 / tot_pop_dhis2,
      totcbr_dhis2 = 1000 * tot_livebirths_dhis2 / tot_pop_dhis2,
      totsbr_dhis2 = 1000 * (tot_allbirths_dhis2 - tot_livebirths_dhis2) / tot_allbirths_dhis2
    ) %>%

    # Assign National Flag for First Row by Year and Calculate Growth
    arrange(year) %>%
    mutate(national = ifelse(row_number() == 1, 1, NA_real_), .by = year) %>%

    # Calculate Population Growth Rate Only for National Rows
    arrange(national, year) %>%
    mutate(
      totpopgrowth = ifelse(
        national == 1 & !is.na(lag(tot_pop_dhis2)),
        100 * log(tot_pop_dhis2 / lag(tot_pop_dhis2)),
        NA_real_
      ),
      .by = national
    ) %>%

    # Calculate Adjusted Crude Death Rate
    mutate(
      totcdr_dhis2 = totcbr_dhis2 - 10 * totpopgrowth
    ) %>%
    arrange(country, adminlevel_1, district, year) %>%

    # Generate ratios
    mutate(
      ratio_totpop = 100 * totpop_dhis2 / un_population,
      ratio_totunder5 = 100 * totunder5_dhis2 / un_under5y,
      ratio_totunder1 = 100 * totunder1_dhis2 / un_under1y,
      ratio_totlivebirths = 100 * totlivebirths_dhis2 / un_births,
      ratio_totwom15_49 = 100 * totwom15_49_dhis2 / un_wom15_49,
      ratio_totpopgrowth = 100 * totpopgrowth / un_popgrowth,
      ratio_cbr = 100 * totcbr_dhis2 / un_cbr
    )
}
