#' Check Indicator Ratios
#'
#' This function calculates various ratios between indicators
#'   (e.g., ANC1/Penta1, Penta1/Penta3) and checks if they fall within an
#'   adequate range.
#'
#' @param data A data frame containing indicator data for districts and years.
#' @return A data frame summarizing the calculated ratios and adequacy checks by
#'   year.
#' @examples
#' \dontrun{
#'
#'   check_ratios(data)
#' }
#' @export
check_ratios <- function(data) {

  district = year = anc1 = penta1 = penta3 = opv1 = opv3 = pcv1 = pcv3 = rota1 =
    rota2 = ipv1 = ipv2 = bcg = instlivebirths = ratioAP = ratioPP = ratioPPcv =
    ratioRR = ratioII = ratioPenta1Rota1 = ratioPenta1PCV1 = ratiobcgbirth =
    ratioopv3ipv = ratioOO = adeq_ratioAP = adeq_ratioPP = adeq_ratioPPcv =
    adeq_ratioRR = adeq_ratioII = adeq_ratioPenta1Rota1 = adeq_ratioPenta1PCV1 =
    adeq_ratiobcgbirth = adeq_ratioopv3ipv = NULL

  data_summary <- data$merged_data %>%
    summarise(
      across(c(anc1,bcg, instlivebirths, penta1, penta3, pcv1, pcv3, opv1, opv3, rota1, rota2, ipv1,ipv2), mean, na.rm = TRUE),
      .by = c(district, year)
    ) %>%
    mutate(
      ratioAP = anc1 / penta1,
      ratioPP = penta1 / penta3,
      ratioOO = opv1 / opv3,
      ratioPPcv = pcv1 / pcv3,
      ratioRR = rota1 / rota2,
      ratioII = ipv1 / ipv2,
      ratioPenta1Rota1 = penta1 / rota1,
      ratioPenta1PCV1 = penta1 / pcv1,
      ratiobcgbirth = bcg / instlivebirths,
      ratioopv3ipv = opv3 / ipv1,

      # Adequacy checks
      adeq_ratioAP = as.integer(ratioAP >= 1 & ratioAP <= 1.5),
      adeq_ratioPP = as.integer(ratioPP >= 1 & ratioPP <= 1.5),
      adeq_ratioPPcv = as.integer(ratioPPcv >= 1 & ratioPPcv <= 1.5),
      adeq_ratioRR = as.integer(ratioRR >= 1 & ratioRR <= 1.5),
      adeq_ratioII = as.integer(ratioII >= 1 & ratioII <= 1.5),
      adeq_ratioPenta1Rota1 = as.integer(ratioPenta1Rota1 >= 1 & ratioPenta1Rota1 <= 1.5),
      adeq_ratioPenta1PCV1 = as.integer(ratioPenta1PCV1 >= 1 & ratioPenta1PCV1 <= 1.5),
      adeq_ratiobcgbirth = as.integer(ratiobcgbirth >= 1 & ratiobcgbirth <= 1.5),
      adeq_ratioopv3ipv = as.integer(ratioopv3ipv >= 1 & ratioopv3ipv <= 1.5)
    )

  # Collapse data by year and summarise
  data_collapsed <- data_summary %>%
    group_by(year) %>%
    summarise(
      across(c(anc1, penta1, penta3, opv1, opv3, pcv1, pcv3, rota1, rota2, ipv1, ipv2, bcg, instlivebirths), sum, na.rm = TRUE),
      ratioAP = round(mean(ratioAP, na.rm = TRUE) * 100, 1),
      ratioPP = mean(ratioPP, na.rm = TRUE),
      ratioOO = mean(ratioOO, na.rm = TRUE),
      ratioPPcv = mean(ratioPPcv, na.rm = TRUE),
      ratioPenta1Rota1 = mean(ratioPenta1Rota1, na.rm = TRUE),
      ratioPenta1PCV1 = mean(ratioPenta1PCV1, na.rm = TRUE),
      ratiobcgbirth = mean(ratiobcgbirth, na.rm = TRUE),
      ratioopv3ipv = mean(ratioopv3ipv, na.rm = TRUE),
      adeq_ratioAP = mean(adeq_ratioAP, na.rm = TRUE),
      adeq_ratioPP = mean(adeq_ratioPP, na.rm = TRUE),
      adeq_ratioPPcv = mean(adeq_ratioPPcv, na.rm = TRUE),
      adeq_ratioRR = mean(adeq_ratioRR, na.rm = TRUE),
      adeq_ratioII = mean(adeq_ratioII, na.rm = TRUE),
      adeq_ratioPenta1Rota1 = mean(adeq_ratioPenta1Rota1, na.rm = TRUE),
      adeq_ratioPenta1PCV1 = mean(adeq_ratioPenta1PCV1, na.rm = TRUE),
      adeq_ratiobcgbirth = mean(adeq_ratiobcgbirth, na.rm = TRUE),
      adeq_ratioopv3ipv = mean(adeq_ratioopv3ipv, na.rm = TRUE)
    ) %>%
    mutate(
      across(starts_with('adeq_'), ~ round(.x * 100, 1)),
      across(c(anc1, penta1, penta3, opv1, opv3, pcv1, pcv3, rota1, rota2, ipv1, ipv2, bcg), round, 1)
    ) %>%
    select(-starts_with('adeq_'))

  new_tibble(
    data_collapsed,
    class = 'cd_check_ratios'
  )
}
