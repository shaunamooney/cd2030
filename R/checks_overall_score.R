#' Calculate Overall Quality Score
#'
#' @param .data A data frame of type `cd_data`.
#'
#' @export
calculate_overall_score <- function(.data) {

  year = NULL

  missingsd <- calculate_district_completeness_summary(.data)
  outliers <- calculate_outliers_summary(.data)
  outliersd <- calculate_district_outlier_summary(.data)
  perc_dist_lowrr <- calculate_district_reporting_rate(.data)
  low_rr_national <- calculate_average_reporting_rate(.data)
  adeqratiosd <- calculate_district_ratios_summary(.data) %>% select(starts_with('Ratio'))

  combined <- cbind(
    missingsd[, 1:20],        # Columns 1 to 20 from missingsd
    outliers[, 2:20],         # Columns 2 to 20 from outliers
    outliersd[, 2:20],        # Columns 2 to 20 from outliersd
    perc_dist_lowrr[, 2:5],   # Columns 2 to 4 from perc_dist_lowrr
    low_rr_national[, 2:5],   # Columns 2 to 4 from low_rr_national
    adeqratiosd[, 1:4]       # Columns 2 to 4 from adeqratiosd
  )

  # Calculate the overall mean across rows and truncate (round down) each mean value
  overall <- combined %>%
    as_tibble(.name_repair = 'unique') %>%
    rowwise(year) %>%
    summarise(overall = mean(c_across(everything()), na.rm = TRUE)) %>%
    ungroup()

  return(overall)
}
