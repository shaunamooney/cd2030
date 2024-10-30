#' Calculate Yearly Indicator Ratios Summary
#'
#' This function computes yearly summaries for specified indicator ratios,
#' evaluating trends across years. It uses dynamically specified indicator pairs
#' to calculate ratios, providing a clear view of how key health indicators relate
#' over time.
#'
#' @param .data A data frame containing yearly data for specified indicators,
#'   with columns for `district` and `year` and additional numeric columns
#'   representing health indicators (e.g., `anc1`, `penta1`).
#' @param ratio_pairs A named list of indicator pairs for which ratios should be
#'   calculated, with the format: `list("ratioName" = c("numerator", "denominator"))`.
#'   Each list entry represents one ratio, where the first element is the numerator
#'   and the second element is the denominator. Default pairs include:
#'   - `"ratioAP"`: Ratio of `anc1` to `penta1`
#'   - `"ratioPP"`: Ratio of `penta1` to `penta3`
#'   - `"ratioOO"`: Ratio of `opv1` to `opv3`
#' @param adequate_range A numeric vector of length 2 specifying the acceptable
#'   range for adequacy checks. Ratios that fall within this range are flagged as
#'   adequate, while those outside are flagged as inadequate. Default is `c(1, 1.5)`.
#'
#' @return A `cd_ratios_summary` object, which is a data frame containing the
#'   following columns:
#'   - **`year`**: Yearly breakdown for calculated indicator ratios.
#'   - **Indicator Ratios**: Each column represents one ratio calculated as the
#'      sum of the specified numerator divided by the sum of the specified
#'      denominator for each year.
#'
#' @details
#' This function is used to monitor internal consistency between key indicators
#' over time by computing meaningful ratios. By summarizing ratios at the yearly
#' level, this function enables quick identification of trends and deviations,
#' aiding in data quality checks and trend analysis. Adequate ratios provide
#' insight into adherence to expected ranges for key indicators, especially
#' in monitoring healthcare intervention effectiveness.
#'
#' @examples
#' \dontrun{
#'   # Calculate ratios summary using default pairs
#'   calculate_ratios_summary(cd_data)
#'
#'   # Custom ratio pairs with a wider adequacy range
#'   calculate_ratios_summary(cd_data,
#'                            ratio_pairs = list(
#'                           "ratioCustom" = c("custom_indicator1", "custom_indicator2")),
#'                            adequate_range = c(0.8, 1.2))
#' }
#'
#' @export
calculate_ratios_summary <- function(.data,
                                     ratio_pairs = list(
                                       "ratioAP" = c("anc1", "penta1"),
                                       "ratioPP" = c("penta1", "penta3"),
                                       "ratioOO" = c("opv1", "opv3")
                                     ),
                                     adequate_range = c(1, 1.5)) {

  data_summary <- calculate_ratios_and_adequacy(.data, ratio_pairs, adequate_range) %>%
    select(-starts_with('adeq_')) %>%
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
                                                  "ratioOO" = c("opv1", "opv3")
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

