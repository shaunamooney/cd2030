#' Calculate Overall Quality Score for Data Quality Metrics
#'
#' This function calculates an overall quality score based on various data
#' quality metrics. It summarizes completeness, outlier presence, and
#' consistency of reporting in immunization health facility data.
#'
#' @param .data A data frame of type `cd_data` containing facility data
#'   including annual reporting rates, completeness, and consistency indicators.
#' @param threshold The data reporting rate threshold.
#' @param ratio_pairs description
#'
#' @details
#' `calculate_overall_score` processes multiple data quality indicators:
#'  - **Completeness metrics**: Percentage of expected reports, districts with
#'    complete reporting, and districts with no missing values.
#'  - **Outlier metrics**: Percentage of monthly values and districts without
#'    extreme outliers.
#'  - **Consistency ratios**: Ratios between different immunization indicators
#'    to ensure internal consistency.
#'
#' The function calculates averages for the selected metrics and includes
#' a row summarizing the annual data quality score.
#'
#' @return A tibble with calculated scores for each metric, including
#'   a summary row for the annual quality score. The result is ordered by
#'   metric codes and ready for reporting in tabular or graphical form.
#'
#' @examples
#' \dontrun{
#' calculate_overall_score(.data = my_data)
#' }
#'
#' @export
calculate_overall_score <- function(.data,
                                    threshold,
                                    ratio_pairs = list(ratioAP = c("anc1", "penta1"), ratioPP = c("penta1", "penta3"))) {

  year = mean_rr = low_mean_rr = mean_mis_vacc_tracer = mean_out_vacc_tracer =
    value = `Data Quality Metrics` = value = no = NULL

  check_cd_data(.data)

  avg_reporting_rate <- calculate_reporting_rate(.data) %>%
    select(year, mean_rr) %>%
    pivot_wider(names_from = year, values_from = mean_rr) %>%
    mutate(
      `Data Quality Metrics` = '% of expected monthly facility reports (national)',
      no = '1a'
    )

  district_reporting_rate <- calculate_district_reporting_rate(.data, threshold = threshold) %>%
    select(year, low_mean_rr) %>%
    pivot_wider(names_from = year, values_from = low_mean_rr) %>%
    mutate(
      `Data Quality Metrics` = paste0('% of districts with completeness of facility reporting >= ', threshold),
      no = '1b'
    )

  district_completeness <- calculate_district_completeness_summary(.data) %>%
    select(year, mean_mis_vacc_tracer) %>%
    pivot_wider(names_from = year, values_from = mean_mis_vacc_tracer) %>%
    mutate(
      `Data Quality Metrics` = '% of districts with no missing values (mean for common vaccines)',
      no = '1c'
    )

  outliers <- calculate_outliers_summary(.data) %>%
    select(year,mean_out_vacc_tracer) %>%
    pivot_wider(names_from = year, values_from = mean_out_vacc_tracer) %>%
    mutate(
      `Data Quality Metrics` = '% of monthly values that are not extreme outliers (national)',
      no = '2a'
    )

  outliersd <- calculate_district_outlier_summary(.data) %>%
    select(year, mean_out_vacc_tracer) %>%
    pivot_wider(names_from = year, values_from = mean_out_vacc_tracer) %>%
    mutate(
      `Data Quality Metrics` = '% of districts with no extreme outliers in the year',
      no = '2b'
    )

  adeqratiosd <- calculate_ratios_and_adequacy(.data, ratio_pairs = ratio_pairs) %>%
    select(year, starts_with("Ratio"), starts_with("% district with")) %>%
    pivot_longer(-year, names_to = 'Data Quality Metrics', values_to = 'value') %>%
    pivot_wider(names_from = year, values_from = value) %>%
    mutate(
      no = case_match(`Data Quality Metrics`,
                      'Ratio anc1/penta1'~ '3a',
                      'Ratio penta1/penta3' ~ '3b',
                      'Ratio opv1/opv3' ~ '3c',
                      'Ratio penta1/pcv1' ~ '3d',
                      'Ratio penta1/rota1' ~ '3e',
                      '% district with anc1/penta1 in expected ranged' ~ '3f',
                      '% district with penta1/penta3 in expected ranged' ~ '3g',
                      '% district with opv1/opv3 in expected ranged' ~ '3h',
                      '% district with penta1/pcv1 in expected ranged' ~ '3i',
                      '% district with penta1/rota1 in expected ranged' ~ '3j')
    )

  final_data <- bind_rows(
    avg_reporting_rate,
    district_reporting_rate,
    district_completeness,
    outliers,
    outliersd,
    adeqratiosd
  ) %>%
    relocate(no, `Data Quality Metrics`)

  mean_row <- final_data %>%
    filter(no %in% c("1a", "1b", "1c", "2a", "2b", "3f", "3g", "3h")) %>%
    summarise(across(starts_with("20"), mean, na.rm = TRUE)) %>%
    mutate(
      `Data Quality Metrics` = "Annual data quality score",
      no = "4"
    )

  final_data <- final_data %>%
    bind_rows(mean_row) %>%
    arrange(no)

  return(final_data)
}
