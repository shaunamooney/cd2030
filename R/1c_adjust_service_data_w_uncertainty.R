#' Adjust Service Data with Uncertainty
#'
#' The `adjust_with_uncertainty` function performs the adjustment of DHIS2 data
#' using a Monte-Carlo sampling based approach.
#'
#' @param data A preprocessed dataframe that includes:
#'   - Columns ending in `_rr` (reporting rates)
#'   - Columns ending in `_k_value` (k-factor values per indicator)
#'   - A logical column `impute_rr` to flag where reporting rates have been imputed
#' @param n_samples Integer. Number of samples to draw per observation. Default is `500`.
#'
#' @export
#' @importFrom truncnorm rtruncnorm
adjust_with_uncertainty <- function(data, n_samples = 10){

  indicator_groups = get_indicator_groups()
  all_indicators <- get_all_indicators()

  data_samples <- data %>%
    slice(rep(1:n(), times = n_samples)) %>%
    mutate(sample_id = rep(1:n_samples, each = nrow(data)))

  adjusted_data_samples <- data_samples %>%
    mutate(
      across(
        ends_with("_k_value"),
        ~ truncnorm::rtruncnorm(1, a = 0, b = 1, mean = ., sd = 0.05),
        .names = "{.col}_sample"
      )
    )

  adjusted_data_samples <- adjusted_data_samples %>%
    mutate(
      across(
        ends_with("_rr"),
        ~ if_else(
          impute_rr & !is.na(.),
          truncnorm::rtruncnorm(1, a = 0, b = 1, mean = ., sd = 0.1),
          .
        ),
        .names = "{.col}_sample"
      )
    )

  adjusted_data_final <- adjusted_data_samples %>%
    mutate(across(
        all_of(all_indicators),
        ~ {
          # Identify the main indicator group for the current sub-indicator
          group <- names(keep(indicator_groups, ~ cur_column() %in% .x))

          # Retrieve the rate column for the current group directly within cur_data()
          rate <- get(paste0(group, '_rr_sample'))

          # Retrieve the k-value from the k_defaults list based on the group
          k_value_sample <- get(paste0(cur_column(), '_k_value_sample'))

          # Apply the adjustment formula if the rate is not missing or zero
          if_else(
            !is.na(rate) & rate != 0,
            . * (1 + (1 / (rate / 100) - 1) * k_value_sample), 0,
            .
          )
        }
      ))

  impute_data <- adjusted_data_final %>%
    add_outlier5std_column(all_indicators) %>%
    mutate(
      across(
        all_of(all_indicators),
        ~ {
          outlier <- get(paste0(cur_column(), '_outlier5std'))
          med <- round(median(if_else(outlier != 1, ., NA_real_), na.rm = TRUE), 0)
          ifelse(outlier == 1, truncnorm::rtruncnorm(1, a = 0, mean = robust_max(med), sd = 100), .)
        }
      ),

      across(
        all_of(all_indicators),
        ~ {
          med <- round(median(if_else(!is.na(.), ., NA_real_), na.rm = TRUE), 0)
          max_med <- robust_max(med)
          if_else(
            is.na(.) & !is.na(max_med), truncnorm::rtruncnorm(1, mean = max_med, a = 0, sd = 100),
            .
          )
        }
      ),

      .by = c(district, year, sample_id)
    ) %>%
    select(-any_of(paste0(all_indicators, '_rr')))


  # TO DO - get back the cols lost when summarising eg. tot_pop_ etc. as
  # losing these causes issues in the calculate_coverage function
  summary_data <- impute_data %>%
    group_by(country, adminlevel_1, district, month, year) %>%
    summarise(
      anc_rr = first(anc_rr),
      idelv_rr = first(idelv_rr),
      vacc_rr = first(vacc_rr),
      across(
        all_of(all_indicators),
        list(
          lower = ~ quantile(., 0.025, na.rm = TRUE),
          upper = ~ quantile(., 0.975, na.rm = TRUE),
          median = ~ median(., na.rm = TRUE),
          mean = ~ mean(., na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    ) %>%
    rename_with(
      ~ sub("_mean$", "", .),  # remove _mean suffix from col names
      ends_with("_mean")
    )


  return(summary_data)
}
