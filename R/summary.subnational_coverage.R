#' Summarize Subnational Health Coverage Analysis
#'
#' Produces a summary table of health coverage data by district and year, as well
#' as metrics such as MADM, weighted MADM, MRDM, and weighted MRDM. The summary
#' includes percentages of units above specified coverage thresholds (80% and 90%).
#'
#' @param object A `cd_subnational_coverage` object returned by `analyze_subnational_coverage`.
#' @param ... Additional arguments (not currently used).
#'
#' @return A data frame summarizing coverage by district and year, including
#'   calculated metrics for each year.
#'
#' @examples
#' \dontrun{
#' data <- analyze_subnational_coverage(.data, "Kenya", level = "district",
#'   indicator = "measles1", denominator = "penta1")
#' summary(data)
#' }
#'
#' @export
summary.cd_subnational_coverage <- function(object, ...) {

  year = madm = madmpop = mrdm = mrdmpop = perc_above80 = perc_above90 = value = NULL

  indicator <- attr(object, 'indicator')
  denominator <- attr(object, 'denominator')
  level <- attr(object, 'level')

  indicator_col <- paste0('cov_', indicator, '_', denominator)

  districts <- object %>%
    arrange(year, !!sym(level)) %>%
    select(year, !!sym(level), !!sym(indicator_col)) %>%
    pivot_wider(names_from = year, values_from = !!sym(indicator_col))

  metrics <- object %>%
    summarise(
      madm = mean(madm, na.rm = TRUE),
      madmpop = mean(madmpop, na.rm = TRUE),
      mrdm = mean(mrdm, na.rm = TRUE),
      mrdmpop = mean(mrdmpop, na.rm = TRUE),

      perc_above80 = mean(!!sym(indicator_col) > 80, na.rm = TRUE) * 100,
      perc_above90 = mean(!!sym(indicator_col) > 90, na.rm = TRUE) * 100,

      .by = year
    ) %>%
    rename(
      `Mean absolute difference to the mean (MADM)` = madm,
      `Weighted MADM` = madmpop,
      `Mean relative difference to the mean (MRDM)` = mrdm,
      `Weighted MRDM` = mrdmpop,
      `% above 80%` = perc_above80,
      `% above 90%` = perc_above90
    ) %>%
    pivot_longer(cols = -year, names_to = level) %>%
    pivot_wider(names_from = year, values_from = value)

  bind_rows(districts, metrics)
}
