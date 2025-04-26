#' Generate Coverage Data with Derived Denominators
#'
#' Calculates trend-adjusted and subnationally-redistributed coverage estimates
#' using the DTP1-derived denominator logic described in CD2030. It estimates
#' coverage over time based on changes in DHIS2 population counts, while preserving
#' subnational proportions from the base year.
#'
#' This allows estimating:
#' - Trends over time in coverage
#' - Subnational inequities
#'
#' @param .data A `cd_population_metrics` object containing indicator values and DHIS2 population.
#' @param indicator A character string specifying the indicator to calculate coverage for.
#' @param base_year Integer. The year from which denominator proportions are derived.
#'
#' @return A `cd_derived_coverage` tibble with columns for old and new coverage estimates.
#'
#' @examples
#' calculate_derived_coverage(dhis_data, 'penta1', 2019)
#'
#' @export
calculate_derived_coverage <- function(.data, indicator, base_year) {
  check_cd_indicator_coverage(.data)

  # Get admin level attribute (national, adminlevel_1, district)
  admin_level <- attr_or_abort(.data, 'admin_level')

  # Match and reference the selected indicator
  indicator <- arg_match(indicator, get_all_indicators())
  indicator_col <- sym(indicator)

  # Get relevant population column for DHIS2 (e.g., totpop_dhis2)
  dhis2_pop <- get_population_column(indicator, 'dhis2')
  dhis2_pop_col <- sym(dhis2_pop)

  # Determine grouping columns based on admin level
  group_cols <- get_admin_columns(admin_level)

  # If subnational, aggregate to national level first
  nat_data <- if (!is.null(group_cols)) {
    .data %>%
      summarise(
        across(-any_of(c(group_cols, 'year')), ~ sum(.x, na.rm = TRUE)),
        .by = year
      )
  } else {
    .data
  }

  # Keep only necessary columns and convert DHIS2 pop to count (×1000)
  nat_data <- nat_data %>%
    select(year, all_of(c(indicator, dhis2_pop)), totinftpenta_penta1) %>%
    mutate(!!dhis2_pop_col := !!dhis2_pop_col * 1000)

  # Ensure base year is not earlier than first year in data
  base_year <- robust_max(c(base_year,  min(nat_data$year, na.rm = TRUE)), 2024)

  # Extract base year values for national denominator and population
  base_row <- nat_data %>%
    filter(year == base_year)

  base_value <- base_row %>% pull(!!dhis2_pop_col)        # national base population
  base_denom <- base_row %>% pull(totinftpenta_penta1)    # national base DTP1-derived denominator

  # ---- NATIONAL-LEVEL DERIVED DENOMINATOR TRENDS ----
  national <- nat_data %>%
    mutate(
      # Step 1: Compute percent change in DHIS2 population over time from base
      percent_change = (!!dhis2_pop_col - base_value) / base_value,

      # Step 2: Apply percent change to national base denominator
      derived_denom = base_denom * (1 + percent_change),

      # Step 3: Calculate traditional and derived coverage values
      coverage_old = (!!indicator_col / totinftpenta_national_penta1) * 100,
      coverage_new = (!!indicator_col / derived_denom) * 100
    ) %>%
    select(
      year,
      !!indicator_col,
      !!dhis2_pop_col,
      totinftpenta_penta1,
      derived_denom,
      percent_change,
      coverage_old,
      coverage_new
    )

  # ---- SUBNATIONAL: DISTRIBUTE DERIVED NATIONAL DENOMINATOR ----
  data_joined <- if (!is.null(group_cols)) {

    # Prepare national population and derived_denom for merge
    national_sel <- national %>%
      select(year, derived_denom, !!dhis2_pop_col) %>%
      rename(national_pop = !!dhis2_pop_col)

    # Step 1: Aggregate subnational indicator, DHIS2 population, and DTP1 denom
    .data %>%
      summarise(
        !!indicator_col := sum(!!indicator_col, na.rm = TRUE),
        !!dhis2_pop_col := sum(!!dhis2_pop_col * 1000, na.rm = TRUE),
        totinftpenta_penta1 = sum(totinftpenta_penta1, na.rm = TRUE),
        .by = c(year, group_cols)
      ) %>%
      left_join(national_sel, by = 'year') %>%
      mutate(
        # Step 2: Compute subnational DHIS2 share of national population
        nat_pro = !!dhis2_pop_col / national_pop,

        # Step 3: Apply that share to national derived denominator
        derived_denom = derived_denom * nat_pro,

        # Step 4: Recalculate coverage
        coverage_old = (!!indicator_col / totinftpenta_penta1) * 100,
        coverage_new = (!!indicator_col / derived_denom) * 100
      ) %>%
      select(-national_pop)
  } else {
    # If admin level is national, use already-computed national data
    national
  }

  # Return final tibble tagged with admin level
  new_tibble(
    data_joined,
    class = 'cd_derived_coverage',
    admin_level = admin_level,
    indicator = indicator
  )
}
