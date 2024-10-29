#' Compute Demographic Denominators and Indicators
#'
#' This function calculates population totals, demographic indicators, and growth rates
#' for a specified country by merging DHIS-2 data and UN population estimates.
#'
#' @param .data A data of type `cd_data`.
#' @param country_name A scalar string specifying the name of the country for which to
#' calculate demographic indicators.
#'
#' @return A tibble containing merged data from DHIS-2 and UN sources with calculated
#' demographic totals, percentages, growth rates, and adjusted crude death rate.
#'
#' @details The function performs the following steps:
#'   1. **Prepare DHIS-2 Data**:
#'      - Selects key demographic columns from DHIS-2 data and renames them for consistency.
#'      - Adds a `country` column set to `country_name`.
#'      - Filters to keep only the first row for each unique combination of `district`,
#'        `adminlevel_1`, and `year`.
#'
#'   2. **Prepare UN Data**:
#'      - Filters the UN estimates to include only records for `country_name` and years
#'        between 2019 and 2023.
#'      - Renames columns in the UN data to add a `un_` prefix, distinguishing them
#'        from DHIS-2 variables.
#'
#'   3. **Merge and Calculate Totals for DHIS-2 Data**:
#'      - Merges `dhis_data` and `un_data` by `country` and `year`.
#'      - Calculates yearly totals for selected DHIS-2 variables, scaling by dividing by 1,000.
#'
#'   4. **Calculate Demographic Indicators**:
#'      - Calculates demographic percentages and crude birth/death rates for both UN and
#'        DHIS-2 data.
#'
#'   5. **Set National Flag and Calculate Population Growth**:
#'      - Flags the first row of each `year` as `national` to identify year-level aggregate rows.
#'      - Calculates `totpopgrowth` as the logarithmic year-over-year growth rate
#'        for rows where `national == 1`.
#'
#'   6. **Calculate Adjusted Crude Death Rate**:
#'      - Adjusts the crude birth rate (`totcbr_dhis2`) based on the calculated growth rate
#'        to yield `totcdr_dhis2`.
#'
#'   7. **Generate Ratios**:
#'
#' @examples
#' \dontrun{
#'   # Assuming `dhis2_data` and `un_data` are preloaded dataframes:
#'   calculate_population_metrics(
#'     .data = list(merged_data = dhis2_data),
#'     country_name = "Kenya"
#'   )
#' }
#'
#' @export
calculate_population_metrics <- function(.data, country_name) {

  check_cd_data(.data)
  check_required(country_name)

  if (!is_scalar_character(country_name)) {
    cd_abort(c('x' = '{.arg country_name} should be a scalar string'))
  }

  # Prepare DHIS-2 Data
  dhis_data <- .data$merged_data %>%
    select(district, adminlevel_1, year, total_pop, under5_pop, under1_pop, live_births, women15_49, total_births) %>%
    rename(
      pop_dhis2 = total_pop,
      under5_dhis2 = under5_pop,
      under1_dhis2 = under1_pop,
      livebirths_dhis2 = live_births,
      allbirths_dhis2 = total_births,
      wom15_49_dhis2 = women15_49
    ) %>%
    mutate(country = country_name) %>%
    arrange(adminlevel_1, district, year) %>%
    filter(row_number() == 1, .by = c(district, adminlevel_1, year))

  # Prepare UN Data
  un_data <- un_estimates %>%
    filter(year >= 2019 & year <= 2023, country == country_name) %>%
    rename_with(~ paste0("un_", .), -c(country, year, countrycode, iso3, iso2))

  # Merge and Calculate Totals and Demographics
  un_data %>%
    left_join(dhis_data, by = c('country', 'year')) %>%
    relocate(country, adminlevel_1, district, year) %>%
    arrange(country, adminlevel_1, district, year) %>%
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
