#' Analyze Subnational Health Coverage Data with Disparity Metrics
#'
#' Calculates health coverage and disparity metrics, such as Mean Absolute
#' Difference to the Mean (MADM), weighted MADM, Mean Relative Difference to the
#' Mean (MRDM), and weighted MRDM, for a specified subnational level and health
#' indicator. The function returns a `cd_subnational_coverage` object with
#' calculated metrics for plotting and summarizing.
#'
#' @param .data A data frame containing the subnational health coverage data.
#' @param country_name A character string specifying the country for analysis.
#' @param level A character string specifying the administrative level to analyze
#'   (either "admin_level_1" or "district").
#' @param indicator A character string specifying the health indicator to analyze
#'   (e.g., "bcg", "measles1").
#' @param denominator A character string specifying the denominator to use for
#'   coverage calculations (e.g., "dhis2", "anc1", "penta1").
#'
#' @return A `cd_subnational_coverage` object containing calculated metrics such
#'   as coverage, MADM, MRDM, and population shares.
#'
#' @examples
#' \dontrun{
#' data <- analyze_subnational_coverage(.data, "Kenya", level = "district",
#'      indicator = "measles1", denominator = "penta1")
#' }
#'
#' @export
analyze_subnational_coverage <- function(.data,
                                         country_name,
                                         level = c('admin_level_1', 'district'),
                                         indicator = c('bcg', 'anc1', 'pcv3', 'opv1', 'opv2', 'opv3',
                                                       'penta2', 'pcv1', 'pcv2', 'penta1', 'penta3', 'measles1',
                                                       'rota1', 'rota2', 'instdeliveries', 'measles2', 'ipv1', 'ipv2',
                                                       'undervax', 'dropout_penta13', 'zerodose', 'dropout_measles12', 'dropout_penta3mcv1'),
                                         denominator = c('dhis2', 'anc1', 'penta1')) {

  country = year = tot = national_mean = popshare = madm = madmpop = rd_max = NULL

  check_cd_data(.data)
  level <-  arg_match(level)
  indicator <-  arg_match(indicator)
  denominator <- arg_match(denominator)

  level_name <- switch (level,
    admin_level_1 = 'adminlevel_1',
    district = 'district'
  )

  population <- case_match(indicator,
                           c('anc1', 'anc4') ~ 'totpreg',
                           c('bcg', 'instlivebirths') ~ 'totlbirths',
                           c('penta1', 'penta3', 'pcv1', 'penta2', 'pcv2', 'pcv3', 'rota1',
                             'rota2', 'ipv1', 'ipv2', 'opv1', 'opv2', 'opv3') ~ 'totinftpenta',
                           'measles1' ~ 'totinftmeasles',
                           'measles2' ~ 'totmeasles2'
                )

  dhis2_col <- paste0('cov_', indicator, '_', denominator)
  pop_col <- paste0(population, '_', denominator)

  national_data <- calculate_populations(.data, country_name) %>%
    # mutate(!!sym(level_name) := 'National') %>%
    select(country, year, dhis2_col, pop_col) %>%
    rename(national_mean = !!sym(dhis2_col),
           tot = !!sym(pop_col))

  subnational_data <- calculate_populations(.data, country_name, level) %>%
    select(country, year, level_name, dhis2_col, pop_col)

  combined_data <- subnational_data %>%
    left_join(national_data, by = c('country', 'year')) %>%
    mutate(
      popshare = !!sym(pop_col) / tot,
      diff = abs(!!sym(dhis2_col) - national_mean),
    ) %>%
    mutate(
      # Calculate the mean absolute difference to the mean (MADM) and weighted MADM
      madm = mean(diff, na.rm = TRUE),
      madmpop = weighted.mean(diff, popshare, na.rm = TRUE),
      #
      # Calculate mean relative difference to the mean (MRDM) and weighted MRDM
      mrdm = (madm / mean(national_mean)) * 100,
      mrdmpop = (madmpop / mean(national_mean)) * 100,

      .by = year
    ) %>%
    mutate(
      rd_max = round(max(!!sym(dhis2_col), na.rm = TRUE), -1),
      rd_max = if_else(rd_max < max(!!sym(dhis2_col), na.rm = TRUE), rd_max + 10, rd_max),
      rd_max = rd_max + 10,
      rd_max = if_else(rd_max %% 20 != 0, rd_max + 10, rd_max)
    )

  new_tibble(
    combined_data,
    class = 'cd_subnational_coverage',
    indicator = indicator,
    denominator = denominator,
    level = level_name
  )
}
