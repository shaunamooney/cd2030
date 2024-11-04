#' Calculate Yearly Indicator Ratios Summary with Expected Ratios
#'
#' This function computes yearly summaries for specified indicator ratios,
#' assessing trends across years and providing both actual and expected ratios
#' for each indicator. It evaluates specified indicator pairs to calculate
#' ratios and compares actual values with expected ratios, based on predefined
#' survey coverage values. The summary facilitates monitoring consistency of
#' key health indicators over time.
#'
#' @param .data A data frame containing yearly data for specified indicators.
#'   Expected columns include `district`, `year`, and additional numeric columns
#'   for health indicators, such as `anc1` and `penta1`.
#' @param survey_coverage A named vector of assumed coverage rates for each indicator.
#'   Default is `c(anc1 = 0.98, penta1 = 0.97, penta3 = 0.89, opv1 = 0.97, opv3 = 0.78, pcv1 = 0.97, rota1 = 0.96)`.
#' @param anc1_penta1_mortality A numeric multiplier applied specifically to the `"ratioAP"`
#'   ratio of `anc1` to `penta1`, to account for assumed mortality between `anc1` and `penta1`.
#'   Default is `1.07`.
#' @param ratio_pairs A named list specifying indicator pairs for which ratios
#'   should be calculated. Format: `list("ratioName" = c("numerator", "denominator"))`.
#'   Default pairs include:
#'   - `"ratioAP"`: Ratio of `anc1` to `penta1`, adjusted with `anc1_penta1_mortality`.
#'   - `"ratioPP"`: Ratio of `penta1` to `penta3`.
#'   - `"ratioOO"`: Ratio of `opv1` to `opv3`.
#'   - `"ratioPPcv"`: Ratio of `penta1` to `pcv1`.
#'   - `"ratioPR"`: Ratio of `penta1` to `rota1`.
#' @param adequate_range A numeric vector of length 2 defining the acceptable
#'   range for adequacy checks. Ratios within this range are marked adequate;
#'   outside this range, inadequate. Default is `c(1, 1.5)`.
#'
#' @return A `cd_ratios_summary` object, a data frame containing:
#'   - **`year`**: Year of each calculated ratio, with an additional row `"Expected Ratio"`
#'     to represent expected values for each indicator ratio based on `survey_coverage`.
#'   - **Indicator Ratios**: Columns for each ratio, showing actual yearly values
#'     and an expected value row based on `survey_coverage`.
#'   - **Additional Columns**: Health indicator values for each year.
#'
#' @details
#' The function provides insights into indicator relationships over time and evaluates
#' deviations from expected values. It enables data quality checks and trend analysis
#' by summarizing ratios yearly and including expected values for comparison. The `"Expected Ratio"`
#' row incorporates predefined survey coverage and serves as a reference for assessing
#' the adequacy of each indicator's ratio.
#'
#' @examples
#' \dontrun{
#'   # Basic usage with default parameters
#'   calculate_ratios_summary(cd_data)
#'
#'   # Custom survey coverage and mortality adjustments for "ratioAP"
#'   calculate_ratios_summary(cd_data,
#'                            survey_coverage = c(anc1 = 0.95, penta1 = 0.92, penta3 = 0.85),
#'                            anc1_penta1_mortality = 1.05)
#' }
#'
#' @export
calculate_ratios_summary <- function(.data,
                                     survey_coverage = c(anc1 = 0.98, penta1 = 0.97, penta3 = 0.89, opv1 = 0.97, opv3 = 0.78, pcv1 = 0.97, rota1 = 0.96),
                                     anc1_penta1_mortality = 1.07,
                                     ratio_pairs = list(
                                       "ratioAP" = c("anc1", "penta1"),
                                       "ratioPP" = c("penta1", "penta3"),
                                       "ratioOO" = c("opv1", "opv3"),
                                       'ratioPPcv' = c('penta1', 'pcv1'),
                                       'ratioPR' = c('penta1', 'rota1')
                                     ),
                                     adequate_range = c(1, 1.5)) {

  year = NULL

  diff <- setdiff(unique(list_c(ratio_pairs)), names(survey_coverage))
  if (length(diff) > 0) {
    cd_abort(
      c('x' = 'Please provide the survey coverage for all indicators in the {.arg ratio_pairs}')
    )
  }

  ratios <- map(names(ratio_pairs), ~ {
    pair <- ratio_pairs[[.x]]
    numerator <- pair[1]
    denominator <- pair[2]

    ratio <- survey_coverage[numerator] / survey_coverage[denominator]
    if (.x == 'ratioAP') {
      ratio = ratio * anc1_penta1_mortality
    }

    return(ratio)
  })

  names(ratios) <- names(ratio_pairs)

  data_summary <- calculate_ratios_and_adequacy(.data, ratio_pairs, adequate_range) %>%
    select(-starts_with('adeq_')) %>%
    mutate(
      year = as.character(year)
    ) %>%
    bind_rows(
      c(year = 'Expected Ratio', survey_coverage, ratios) %>%
        as_tibble()
    ) %>%
    rename_with(
      ~ map_chr(.x, function(name) {
        pair <- ratio_pairs[[name]]
        paste0('Ratio ', pair[1], '/', pair[2])
      }),
      starts_with("ratio")
    )

  new_tibble(data_summary, class = 'cd_ratios_summary')
}

#' Calculate District Adequacy Summary
#'
#' This function calculates and summarizes the adequacy of specified indicator
#' ratios by year, evaluating the consistency of key health indicators across
#' districts. It computes the percentage of districts meeting the adequacy criteria
#' for each indicator ratio, based on the specified range.
#'
#' @param .data A data frame containing indicator data by district and year.
#'   The data frame should include all indicators specified in `ratio_pairs`.
#' @param ratio_pairs A named list where each element represents a pair of indicators
#'   for ratio calculation. The default pairs are:
#'   - `"ratioAP"`: ANC1 to PENTA1 (i.e., `anc1/penta1`)
#'   - `"ratioPP"`: PENTA1 to PENTA3 (i.e., `penta1/penta3`)
#'   - `"ratioOO"`: OPV1 to OPV3 (i.e., `opv1/opv3`)
#' @param adequate_range A numeric vector of length 2 that specifies the lower
#'   and upper bounds for adequacy. Ratios within this range will be flagged as adequate.
#'   Default is `c(1, 1.5)`.
#'
#' @return A tibble of class `cd_district_ratios_summary`, containing the summary of
#'   adequacy checks by year. Each column represents the percentage of districts
#'   meeting the adequacy criteria for the specified ratio, with overall metrics
#'   included for data interpretation and quality monitoring.
#'
#' @details
#' - The function dynamically calculates ratios between the specified indicator pairs.
#' - For each year, it computes the percentage of districts where the calculated ratio
#'   for each indicator pair falls within the adequate range.
#' - The summary table aids in monitoring indicator consistency across districts and
#'   over time, providing insights into areas where health service delivery may vary.
#'
#' @examples
#' \dontrun{
#'   # Calculate adequacy summary for ANC1/Penta1 and other indicator ratios
#'   calculate_district_ratio_summary(cd_data)
#' }
#'
#' @export
calculate_district_ratios_summary <- function(.data,
                                                ratio_pairs = list(
                                                  "ratioAP" = c("anc1", "penta1"),
                                                  "ratioPP" = c("penta1", "penta3"),
                                                  "ratioOO" = c("opv1", "opv3"),
                                                  'ratioPPcv' = c('penta1', 'pcv1'),
                                                  'ratioPR' = c('penta1', 'rota1')
                                                ),
                                                adequate_range = c(1, 1.5)) {

  data_summary <- calculate_ratios_and_adequacy(.data, ratio_pairs, adequate_range) %>%
    select(-starts_with('ratio')) %>%
    rename_with(
      ~ map_chr(.x, function(name) {
        ratio_name <- str_replace(name, '^adeq_', '')
        pair <- ratio_pairs[[ratio_name]]
        paste0('Ratio ', pair[1], '/', pair[2])
      }),
      starts_with("adeq_")
    )

  new_tibble(data_summary, class = 'cd_district_ratios_summary')
}

