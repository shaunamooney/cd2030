#' Analyze Subnational Health Coverage Data with Disparity Metrics
#'
#' `analyze_inequality` calculates health coverage and disparity metrics, such as:
#' - **Mean Absolute Difference to the Mean (MADM)**: Average absolute deviation
#'   from the national mean.
#' - **Weighted MADM**: MADM adjusted for population share.
#' - **Mean Relative Difference to the Mean (MRDM)**: MADM expressed as a
#'   percentage of the national mean.
#' - **Weighted MRDM**: MRDM adjusted for population share.
#'
#' The function processes subnational health coverage data at specified administrative
#' levels and calculates metrics for disparities in health coverage. It supports
#' multiple denominators and health indicators.
#'
#' @param .data A data frame containing health coverage data for subnational units.
#' @param admin_level A character string specifying the administrative level for analysis.
#'   Options: `"adminlevel_1"` or `"district"`.
#' @param indicator A character string specifying the health indicator to analyze.
#'   Supported indicators include `"bcg"`, `"measles1"`, `"penta3"`, etc.
#' @param denominator A character string specifying the denominator for coverage
#'   calculations.Options: `"dhis2"`, `"anc1"`, or `"penta1"`.
#' @param un_estimates Optional. A data frame containing UN population estimates
#'   for national-level calculations.
#' @param sbr Numeric. Stillbirth rate (default: 0.02).
#' @param nmr Numeric. Neonatal mortality rate (default: 0.025).
#' @param pnmr Numeric. Post-neonatal mortality rate (default: 0.024).
#' @param anc1survey Numeric. Survey coverage rate for ANC-1 (default: 0.98).
#' @param dpt1survey Numeric. Survey coverage rate for Penta-1 (default: 0.97).
#' @param twin Numeric. Twin birth rate (default: 0.015).
#' @param preg_loss Numeric. Pregnancy loss rate (default: 0.03).
#'
#' @return A `cd_inequality` object containing subnational coverage metrics,
#'   population shares, MADM, MRDM, and other disparity metrics, grouped by year.
#'
#' @examples
#' \dontrun{
#'   inequality_metrics <- analyze_inequality(
#'     .data = health_data,
#'     admin_level = "district",
#'     indicator = "measles1",
#'     denominator = "penta1",
#'     un_estimates = un_data
#'   )
#' }
#'
#' @export
analyze_inequality <- function(.data,
                               admin_level = c('adminlevel_1', 'district'),
                               indicator = c('anc1', 'bcg', 'measles1', 'measles2', 'opv1', 'opv2', 'opv3',
                                             'pcv1', 'pcv2', 'pcv3', 'penta1', 'penta2', 'penta3',
                                             'rota1', 'rota2', 'ipv1', 'ipv2'),
                               denominator = c('dhis2', 'anc1', 'penta1'),
                               un_estimates,
                               sbr = 0.02,
                               nmr = 0.025,
                               pnmr = 0.024,
                               anc1survey = 0.98,
                               dpt1survey = 0.97,
                               twin = 0.015,
                               preg_loss = 0.03) {

  year = tot = national_mean = popshare = madm = madmpop = rd_max = NULL

  # Validation
  check_cd_data(.data)
  admin_level <-  arg_match(admin_level)
  indicator <-  arg_match(indicator)
  denominator <- arg_match(denominator)

  # Determine population column
  population <- case_match(
    indicator,
    c('anc1', 'anc4') ~ 'totpreg',
    c('bcg', 'instlivebirths') ~ if_else(denominator == 'dhis2', 'totbirths', 'totlbirths'),
    c('penta1', 'penta2', 'penta3', 'pcv1', 'pcv2', 'pcv3', 'rota1',
      'rota2', 'ipv1', 'ipv2', 'opv1', 'opv2', 'opv3') ~ 'totinftpenta',
    'measles1' ~ 'totinftmeasles',
    'measles2' ~ 'totmeasles2'
  )

  # Define column names for coverage and population
  dhis2_col <- paste0('cov_', indicator, '_', denominator)
  pop_col <- paste0(population, '_', denominator)

  # National-level data
  national_data <- calculate_populations(.data,
                                         admin_level = 'national',
                                         un_estimates = un_estimates,
                                         sbr = sbr, nmr = nmr, pnmr = pnmr,
                                         anc1survey = anc1survey, dpt1survey = dpt1survey,
                                         twin = twin, preg_loss = preg_loss) %>%
    select(year, any_of(c(dhis2_col, pop_col))) %>%
    rename(national_mean = !!sym(dhis2_col),
           tot = !!sym(pop_col))

  # Subnational-level data
  subnational_data <- calculate_populations(.data,
                                            admin_level = admin_level,
                                            sbr = sbr, nmr = nmr, pnmr = pnmr,
                                            anc1survey = anc1survey, dpt1survey = dpt1survey,
                                            twin = twin, preg_loss = preg_loss) %>%
    select(year, any_of(c(admin_level, dhis2_col, pop_col)))


  # Combine and calculate metrics
  combined_data <- subnational_data %>%
    left_join(national_data, by = 'year') %>%
    mutate(
      popshare = !!sym(pop_col) / tot,
      diff = abs(!!sym(dhis2_col) - national_mean),
    ) %>%
    mutate(
      # Calculate the mean absolute difference to the mean (MADM) and weighted MADM
      madm = mean(diff, na.rm = TRUE),
      madmpop = stats::weighted.mean(diff, popshare, na.rm = TRUE),
      #
      # Calculate mean relative difference to the mean (MRDM) and weighted MRDM
      mrdm = (madm / mean(national_mean)) * 100,
      mrdmpop = (madmpop / mean(national_mean)) * 100,

      .by = year
    ) %>%
    mutate(
      rd_max = round(robust_max(!!sym(dhis2_col), fallback = 100), -1),
      rd_max = if_else(rd_max < max(!!sym(dhis2_col), na.rm = TRUE), rd_max + 10, rd_max),
      rd_max = rd_max * 1.05,
      rd_max = if_else(rd_max %% 20 != 0, rd_max + 10, rd_max)
    )

  new_tibble(
    combined_data,
    class = 'cd_inequality',
    indicator = indicator,
    denominator = denominator,
    admin_level = admin_level
  )
}
