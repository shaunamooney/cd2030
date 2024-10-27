#' Check Indicator Ratios Dynamically
#'
#' This function calculates ratios dynamically between specified indicators and
#'   checks if they fall within an adequate range.
#'
#' @param .data A data frame containing indicator data for districts and years.
#' @param ratio_pairs A list of pairs of indicators for which ratios should be
#'   calculated (default: common pairs).
#' @param adequate_range A numeric vector of length two specifying the lower and
#'   upper bounds for the adequate range (default: c(1, 1.5)).
#'
#' @return A data frame summarizing the calculated ratios and adequacy checks by
#'   year.
#'
#' @examples
#' \dontrun{
#'   check_ratios(data)
#' }
#'
#' @export
check_ratios <- function(.data,
                         ratio_pairs = list(
                           "ratioAP" = c("anc1", "penta1"),
                           "ratioPP" = c("penta1", "penta3"),
                           "ratioOO" = c("opv1", "opv3"),
                           "ratioPPcv" = c("pcv1", "pcv3"),
                           "ratioRR" = c("rota1", "rota2"),
                           "ratioII" = c("ipv1", "ipv2"),
                           "ratioPenta1Rota1" = c("penta1", "rota1"),
                           "ratioPenta1PCV1" = c("penta1", "pcv1"),
                           "ratiobcgbirth" = c("bcg", "instlivebirths"),
                           "ratioopv3ipv" = c("opv3", "ipv1")
                         ),
                         adequate_range = c(1, 1.5)) {

  district = year = NULL

  check_cd_data(.data)
  check_ratio_pairs(ratio_pairs)
  # Check that adequate_range is a numeric vector of length 2
  if (!is.numeric(adequate_range) || length(adequate_range) != 2){
    cd_abort(c('x' = '{.arg caller_arg(adequate_range)} is invalid.'), call = call)
  }

  data_summary <- .data$merged_data %>%
    # Calculate the average of indicators by district and year
    summarise(
      across(unname(unlist(ratio_pairs)), mean, na.rm = TRUE),
      .by = c(district, year)
    ) %>%
    bind_cols(
      imap_dfc(ratio_pairs, ~ data_summary[[.x[1]]] / data_summary[[.x[2]]] %>% set_names(.y))
    ) %>%
    # Calculate adequacy checks
    mutate(across(names(ratio_pairs), ~ as.integer(.x >= adequate_range[1] & .x <= adequate_range[2]), .names = "adeq_{.col}")) %>%
    # Summarize adequacy checks by year
    summarise(across(starts_with('adeq_'), mean, na.rm = TRUE), .by = year) %>%
    mutate(across(starts_with('adeq_'), ~ round(.x * 100, 1))) %>%
    # Rename columns dynamically
    rename_with(
      ~ map_chr(.x, function(name) {
        ratio_name <- str_replace(name, '^adeq_', '')
        pair <- ratio_pairs[[ratio_name]]
        paste0('Ratio ', pair[1], '/', pair[2])
      }),
      starts_with("adeq_")
    )

  new_tibble(
    data_summary,
    class = 'cd_check_ratios'
  )
}

#' Summary for cd_check_ratios
#'
#' Provides a custom summary for the `cd_check_ratios` object, displaying
#' a message indicating the data shows Percentage of districts with adequate ratios
#' by year for all indicators.
#'
#' @param x A `cd_check_ratios` object containing reporting rate data.
#' @param ... Additional arguments for compatibility with S3 method.
#'
#' @return A character vector summarizing the content and purpose of the data.
#'
#' @export
tbl_sum.cd_check_ratios <- function(x, ...) {
  c(
    'Table' = 'Percentage of districts with adequate ratios (between 1.0 and 1.5)',
    NextMethod()
  )
}
