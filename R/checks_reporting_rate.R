#' Calculate Average Reporting Rate by Year
#'
#' `calculate_average_reporting_rate` computes the average reporting rate for
#' each indicator across all years available in the dataset. This function
#' generates a tibble with annual averages for each reporting indicator and
#' an overall mean reporting rate per year.
#'
#' @param .data A data frame of class `cd_data` that contains reporting rate
#'   information by year.
#'
#' @return A data frame of class `cd_average_reporting_rate`, which includes
#'   average reporting rates for each indicator along with an overall mean
#'   reporting rate for each year.
#'
#' @details This function produces a summary of reporting completeness across
#'   multiple indicators. The results provide insights on the annual average
#'   completeness and can help identify trends over time in facility reporting,
#'   supporting routine data quality assessments as recommended in Countdown 2030.
#'
#' @examples
#' \dontrun{
#'   # Calculate average reporting rate by year
#'   avg_report_rate <- calculate_average_reporting_rate(data)
#' }
#'
#' @export
calculate_average_reporting_rate <- function(.data) {

  year = . = NULL

  check_cd_data(.data)

  indicator_groups <- attr(.data, 'indicator_groups')
  indicator_groups <- paste0(names(indicator_groups), '_rr')

  reporting_rate <- .data %>%
    summarise(across(indicator_groups, mean, na.rm = TRUE), .by = year) %>%
    mutate(mean_rr = rowMeans(select(., indicator_groups), na.rm = TRUE)) %>%
    mutate(across(ends_with('_rr'), round, 0))

  new_tibble(
    reporting_rate,
    class = 'cd_average_reporting_rate'
  )
}

#' Calculate District-Level Reporting Rates by Year
#'
#' `calculate_district_reporting_rate` calculates the percentage of districts with
#' reporting rates above a specified threshold for each year across various indicators.
#' The function provides a summary of the percentage of districts meeting the threshold
#' for each indicator and includes a mean reporting rate for each year.
#'
#' @param .data A data frame of class `cd_data` containing district-level
#'   reporting rates.
#' @param threshold A numeric value representing the minimum acceptable reporting
#'   rate threshold (default is 90).
#'
#' @return A data frame of class `cd_district_reporting_rate`, containing the
#'   percentage of districts with reporting rates above the specified threshold
#'   for each indicator and a mean reporting rate.
#'
#' @details This function supports subnational data quality assessments by
#' identifying districts with low reporting completeness for defined indicators.
#' Calculating the percentage of districts with acceptable reporting rates
#' provides insight into data quality gaps and helps target data quality
#' improvements at the district level, as outlined in Countdown 2030 guidance.
#'
#' @examples
#' \dontrun{
#'   # Calculate district-level reporting rates with a threshold of 90%
#'   district_summary <- calculate_district_reporting_rate(data, threshold = 90)
#' }
#'
#' @export
calculate_district_reporting_rate <- function(.data, threshold = 90) {

  year = district = . = NULL

  check_cd_data(.data)

  indicator_groups <- attr(.data, 'indicator_groups')
  indicator_groups <- paste0(names(indicator_groups), '_rr')

  reporting_rate <- .data %>%
    summarise(across(indicator_groups, mean, na.rm = TRUE), .by = c(district, year)) %>%
    summarise(across(indicator_groups, ~ round(mean(.x >= threshold, na.rm = TRUE) * 100, 1), .names = 'low_{.col}'), .by = year) %>%
    mutate(
      low_mean_rr = round(rowMeans(select(., starts_with('low_')), na.rm = TRUE), 0)
    )

  new_tibble(
    reporting_rate,
    class = 'cd_district_reporting_rate',
    threshold = threshold
  )
}

#' Identify Administrative Units with Low Reporting Rates
#'
#' `subnational_low_reporting` identifies administrative regions where reporting
#' rates fall below a specified threshold.
#'
#' @param .data A tibble of class `cd_reporting_rate` containing reporting rates.
#' @param year Integer. The year for which to filter the data.
#'
#' @return A tibble containing administrative units and years where reporting rates for any
#'   indicator group fall below the specified threshold.
#'
#' @examples
#' \dontrun{
#'   # Calculate district-level reporting rates with a threshold of 90%
#'   district_summary <- subnational_low_reporting(data, year = 2024, threshold = 90)
#' }
#'
#' @export
subnational_low_reporting <- function(.data, year) {

  # Input validation
  check_cd_reporting_rate(.data)
  check_scalar_integerish(year)

  indicator <- attr(.data, 'indicator')
  admin_level <- attr(.data, 'admin_level')
  threshold <- attr(.data, 'threshold')

  reporting_rate <- .data %>%
    filter(year == !!year, !!sym(indicator) < threshold) %>%
    arrange(desc(!!sym(indicator)), desc(total_pop)) %>%
    select(-total_pop)

  return(reporting_rate)
}

#' `calculate_reporting_rate` calculates reporting rate by administrative region
#' for a specified indicator
#'
#' @param .data A tibble of class `cd_data` containing reporting rates.
#' @param indicator Character. The indicator to be evaluated, Must be one of
#'   `"anc_rr"`, `"idelv_rr"`, or `"vacc_rr"`
#' @param admin_level Character. The administrative level at which to assess reporting rates.
#'   Must be one of `"adminlevel_1"` or `"district"`.
#' @param threshold Numeric. The reporting rate threshold below which administrative units
#'   will be flagged. Defaults to 90.
#'
#' @return A tibble containing administrative units and years where reporting rates for any
#'   indicator group fall below the specified threshold.
#'
#' @export
calculate_reporting_rate <- function(.data,
                                  indicator,
                                  admin_level = c('adminlevel_1', 'district'),
                                  threshold = 90) {
  check_cd_data(.data)
  check_scalar_integerish(threshold)
  admin_level <- arg_match(admin_level)
  admin_level_cols <- switch(
    admin_level,
    adminlevel_1 = 'adminlevel_1',
    district = c('adminlevel_1', 'district')
  )

  indicator_groups <- attr(.data, 'indicator_groups')
  indicator_groups <- paste0(names(indicator_groups), '_rr')

  indicator <- arg_match(indicator, indicator_groups)

  reporting_rate <- .data %>%
    mutate(
      total_pop = total_pop / 12,
      total_pop = sum(total_pop, na.rm = TRUE),
      .by = c(admin_level_cols, year)
    ) %>%
    summarise(across(all_of(indicator), ~ round(mean(.x, na.rm = TRUE), 0)), .by = c(admin_level_cols, year, total_pop))

  new_tibble(
    reporting_rate,
    class = 'cd_reporting_rate',
    indicator = indicator,
    admin_level = admin_level,
    threshold = threshold
  )
}

#' @export
plot.cd_reporting_rate <- function(x,
                                   plot_type = c('heat_map', 'bar'),
                                   ...) {

  plot_type = arg_match(plot_type)
  indicator <- attr(x, 'indicator')
  threshold <- attr(x, 'threshold')
  admin_level <- attr(x, 'admin_level')

  if (plot_type == 'heat_map') {

  greater <- paste0('>= ', threshold)
  mid <- paste0(' >= 40 and < ', threshold)
  low <- '< 40'

  dt <- x %>%
    mutate(
      color_category = case_when(
        !!sym(indicator) >= threshold ~ greater,
        !!sym(indicator) >= 40 & !!sym(indicator) < threshold ~ mid,
        !!sym(indicator) < 40 ~ low,
        .ptype = factor(levels = c(low, mid, greater))
      )
    )

  ggplot(dt, aes(x = !!sym(admin_level), y = year, fill = color_category)) +
    geom_tile(color = 'white') +
    scale_fill_manual(
      values = set_names(
        c("forestgreen", "orange", "red"),
        c(greater, mid, low)
      ),
      name = "Value Category",
      drop = FALSE
    ) +
    geom_text(aes(label = !!sym(indicator)), color = 'black', size = 3, vjust = 0.5) +
    labs(title = NULL, x = if (admin_level == 'district') 'District' else 'Admin Level 1', y = 'Year', fill = 'Value') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, size = 9, hjust = 1))
  } else {
    min_rr <- min(x[[indicator]], na.rm = TRUE)
    low_threshold <- ifelse(min_rr < 80, min_rr, 70)

    breaks_vals <- c(low_threshold, 80, 90, 100)

    ggplot(x, aes(year, !!sym(indicator), fill = !!sym(indicator))) +
      geom_col() +
      facet_wrap(as.formula(paste0('~', admin_level))) +
      labs(title = paste0('Reporting rates by years and ', admin_level), x = 'Year', y = 'Reporting Rate') +
      scale_fill_gradientn(
        colors = c("red", "orange", "yellowgreen", "forestgreen"),
        values = scales::rescale(breaks_vals, to = c(0, 1)),
        breaks = breaks_vals,
        labels = breaks_vals,
        limits = c(low_threshold, 100)
      ) +
      theme(
        panel.background = element_blank(),
        strip.background = element_blank(),
        # strip.text = element_text(size = 12)
        panel.grid.major = element_line(colour = 'gray95'),
        axis.ticks = element_blank()
      )
  }
}
