#' Check Average Reporting Rate
#'
#' This function calculates the average reporting rate for each indicator across
#'   all years.
#'
#' @param data A data frame of class `cd_data`.
#' @return A data frame with the average reporting rates and a mean reporting rate
#'   for each year.
#' @examples
#' \dontrun{
#'
#'   check_average_reporting_rate(data)
#' }
#' @export
check_average_reporting_rate <- function(data) {

  year = district = . = NULL

  if (!inherits(data, "cd_data")) stop("The data object must be of class 'cd_data'.")

  reporting_rate <- data$merged_data %>%
    summarise(across(ends_with("_rr"), mean, na.rm = TRUE), .by = year) %>%
    mutate(mean_rr = rowMeans(select(., ends_with("_rr")), na.rm = TRUE)) %>%
    mutate(across(ends_with("_rr"), round, 2))

  return(reporting_rate)
}

#' Check Reporting Rates by District
#'
#' This function calculates the percentage of districts reporting rates above a
#' defined threshold for each by year.
#'
#' @param data A data frame containing reporting rates for multiple districts
#'   and years.
#' @param threshold Threshold to calculate the reporting rates
#' @return A data frame with the percentage of districts with reporting rates
#'   above the threshold for each indicator.
#' @examples
#' \dontrun{
#'
#'   check_reporting_rates(data)
#' }
#' @export
check_reporting_rates <- function(data, threshold = 90) {

  year = district = . = NULL

  if (!inherits(data, "cd_data")) stop("The data object must be of class 'cd_data'.")

  reporting_rate <- data$merged_data %>%
    # Step 1: Calculate the mean reporting rates by district and year
    summarise(across(ends_with("_rr"), ~ mean(as.numeric(.x), na.rm = TRUE)), .by = c(district, year)) %>%
    # Step 2: Calculate the percentage of districts with reporting rates above the threshold
    summarise(across(ends_with("_rr"), ~ (sum(.x >= threshold, na.rm = TRUE) / n()) * 100), .by = year) %>%
    # Step 3: Rename columns to have the "low_" prefix
    rename_with(~ paste0("low_", .x), .cols = ends_with("_rr")) %>%
    # Step 4: Calculate the row mean of the "low_" columns
    mutate(low_mean_rr = rowMeans(select(., starts_with("low")), na.rm = TRUE)) %>%
    # Step 5: Round all "low_" columns and the row mean to 2 decimal places
    mutate(across(ends_with("_rr"), round, 2))

  new_tibble(
    reporting_rate,
    class = 'cd_reporting_rate'
  )
}

#' Plot Reporting Rate Summary
#'
#' This function creates a bar plot of reporting rates, showing the percentage
#'   of districts with reporting rates for different indicators over the years.
#'
#' @param data A data frame containing reporting rate data that has already been
#'   processed by [check_reporting_rates()].
#' @return A ggplot object displaying reporting rates across indicators and years.
#' @examples
#' \dontrun{
#'
#'   print_reporting_rate(data)
#' }
#' @export
print_reporting_rate <- function(data) {

  year = value = NULL

  if (!inherits(data, "cd_reporting_rate")) stop("Reporting rate has not been checked.")

  # Invert the reporting rates and reshape for plotting
  data %>%
    mutate(across(starts_with("low_"), ~ 100 - ., .names = "inv_{col}")) %>%
    pivot_longer(cols = starts_with("inv_low_"), names_to = "indicator", values_to = "value") %>%
    # Define indicator names and corresponding titles
    mutate(title = case_when(
      indicator == "inv_low_anc_rr" ~ "Antenatal Care",
      indicator == "inv_low_idelv_rr" ~ "Institutional Delivery",
      indicator == "inv_low_vacc_rr" ~ "Vaccination"
    )) %>%
    # Create the plot with facet_wrap
    ggplot(aes(x = year, y = value, fill = as.factor(year))) +
    geom_col(position = "dodge") +
    geom_text(aes(label = round(value, 1)), position = position_dodge(width = 0.9), vjust = -0.25, color = "black", size = 3) +
    facet_wrap(~title, scales = "free_y") +
    labs(x = "Year", y = "Percentage") +
    theme_bw() +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
    scale_fill_manual(values = c("2019" = "darkgreen", "2020" = "darkorange", "2021" = "darkblue", "2022" = "red", "2023" = "green"))
}

#' Check for Outliers
#'
#' This function checks for outliers based on the Median Absolute Deviation (MAD)
#'   for a range of indicators. It flags outliers if values fall beyond five
#'   standard deviations from the median.
#'
#' @param data A data frame containing indicator data for districts and years.
#' @return A data frame summarizing the percentage of non-outliers for each
#'   indicator, by year.
#' @examples
#' \dontrun{
#'
#'   check_outliers(data)
#' }
#' @export
check_outliers <- function(data) {

  year = . = NULL

  if (!inherits(data, "cd_data")) stop("An object of class cd_data is required.")

  # Define variable groups
  ancvargroup <- c("anc1")
  idelvvargroup <- c("ideliv", "instlivebirths")
  vaccvargroup <- c("opv1", "opv2", "opv3", "penta1", "penta2", "penta3", "measles1", "pcv1", "pcv2", "pcv3", "measles2", "bcg", "rota1", "rota2", "ipv1", "ipv2")
  allindicators <- c(ancvargroup, idelvvargroup, vaccvargroup)

  # firstyear <- min(data$year, na.rm = TRUE)
  lastyear <- max(data$merged_data$year, na.rm = TRUE)

  outliers_summary <- map_df(allindicators, ~ {
    data$merged_data %>%
      mutate(
        !!paste0(.x, "_mad") := mad(if_else(year < lastyear, !!sym(.x), NA_real_), constant = 1, na.rm = TRUE),
        !!paste0(.x, "_med") := median(if_else(year < lastyear, !!sym(.x), NA_real_), na.rm = TRUE),
        !!paste0(.x, "_outlb5std") := !!sym(paste0(.x, "_med")) - 7.413 * !!sym(paste0(.x, "_mad")),
        !!paste0(.x, "_outub5std") := !!sym(paste0(.x, "_med")) + 7.413 * !!sym(paste0(.x, "_mad")),
        !!paste0(.x, "_outlier5std") := ifelse(!is.na(!!sym(.x)) & (!!sym(.x) < !!sym(paste0(.x, "_outlb5std")) | !!sym(.x) > !!sym(paste0(.x, "_outub5std"))), 1, 0),
        !!paste0("mis_", .x) := ifelse(is.na(!!sym(.x)), 1, 0),
        .by = district
      )
  }) %>%
    # Collapse data by year and calculate mean percentage of non-outliers
    summarise(
      across(ends_with("_outlier5std"), ~ mean(1 - .x, na.rm = TRUE) * 100),
      .by = year
    ) %>%
    mutate(
      across(contains("_outlier5std"), round, 2),
      # Calculate row mean of outliers if needed
      mean_outlier5std = round(rowMeans(select(., ends_with("_outlier5std")), na.rm = TRUE), 2)
    )

  new_tibble(
    outliers_summary,
    class = 'cd_outliers_summary'
  )
}

#' Check for Extreme Outliers
#'
#' This function identifies extreme outliers by calculating the maximum outlier
#'   value for each indicator in each district and year.
#'
#' @param data A data frame containing indicator data.
#' @return A data frame summarizing the percentage of districts without extreme
#'   outliers, by year.
#' @examples
#' \dontrun{
#'
#'   check_extreme_outlier(data)
#' }
#' @export
check_extreme_outlier <- function(data) {

  district = year = . = NULL

  if (!inherits(data, "cd_data")) stop("The data object must be of class 'cd_data'.")

  # Define variable groups
  ancvargroup <- c("anc1")
  idelvvargroup <- c("ideliv", "instlivebirths")
  vaccvargroup <- c("opv1", "opv2", "opv3", "penta1", "penta2", "penta3", "measles1", "pcv1", "pcv2", "pcv3", "measles2", "bcg", "rota1", "rota2", "ipv1", "ipv2")
  allindicators <- c(ancvargroup, idelvvargroup, vaccvargroup)

  lastyear <- max(data$merged_data$year, na.rm = TRUE)

  outliers_summary <- map_df(allindicators, ~ {
    data$merged_data %>%
      mutate(
        !!paste0(.x, "_mad") := mad(if_else(year < lastyear, !!sym(.x), NA_real_), constant = 1, na.rm = TRUE),
        !!paste0(.x, "_med") := median(if_else(year < lastyear, !!sym(.x), NA_real_), na.rm = TRUE),
        !!paste0(.x, "_outlb5std") := !!sym(paste0(.x, "_med")) - 7.413 * !!sym(paste0(.x, "_mad")),
        !!paste0(.x, "_outub5std") := !!sym(paste0(.x, "_med")) + 7.413 * !!sym(paste0(.x, "_mad")),
        !!paste0(.x, "_outlier5std") := ifelse(!is.na(!!sym(.x)) & (!!sym(.x) < !!sym(paste0(.x, "_outlb5std")) | !!sym(.x) > !!sym(paste0(.x, "_outub5std"))), 1, 0),
        !!paste0("mis_", .x) := ifelse(is.na(!!sym(.x)), 1, 0),
        .by = district
      )
    }) %>%
    summarise(
      across(
        ends_with("_outlier5std"),
        ~ ifelse(all(is.na(.x)), NA_real_, max(.x, na.rm = TRUE))
      ),
      .by = c(district, year)
    ) %>%
    # Collapse to mean per year across districts
    select(-district, matches("_outlier5std$")) %>%
    summarise(
      across(everything(), ~ round((1 - mean(.x, na.rm = TRUE)) * 100, 0.01)),
      .by = year
    ) %>%
    # Add a mean row for district data
    mutate(
      mean_outlier5std = round(rowMeans(select(., -year), na.rm = TRUE), 2)
    )

  new_tibble(
    outliers_summary,
    class = 'cd_extreme_outliers_summary'
  )
}

#' Plot Comparison between Two Indicators
#'
#' This function creates a scatter plot comparison between two indicators for
#'   each year, with a linear fit and R-squared value displayed.
#'
#' @param data A data frame containing the data to be plotted.
#' @param x_var A string specifying the column name for the x-axis variable.
#' @param y_var A string specifying the column name for the y-axis variable.
#' @param title The title of the plot.
#' @param x_label The label for the x-axis.
#' @param y_label The label for the y-axis.
#' @return A ggplot object representing the comparison plot.
#' @examples
#' \dontrun{
#'
#'   plot_comparison(cd_data, "anc1", "penta1", "Comparison of ANC1 and Penta1", "ANC1", "Penta1")
#' }
#' @export
plot_comparison <- function(data, x_var, y_var, title, x_label, y_label) {

  district = year = min_x = max_x = r_squared = NULL

  if (!inherits(data, "cd_data")) stop("The data object must be of class 'cd_data'.")

  df2 <- data$merged_data %>%
    reframe(
      across(c(!!sym(x_var), !!sym(y_var)), sum, na.rm = TRUE),
      .by = c(district, year)
    )

  # Dynamically calculate r-squared for all years
  r2_data <- df2 %>%
    summarise(
      r_squared = summary(lm(as.formula(paste(y_var, "~", x_var))))$r.squared,
      .by = year
    )

  # Add min and max for the diagonal line
  df2 <- df2 %>%
    mutate(
      min_x = min(!!sym(x_var), na.rm = TRUE),
      max_x = max(!!sym(x_var), na.rm = TRUE),
      .by = year
    )

  # Create the plot
  plot <- ggplot(df2, aes(x = !!sym(x_var), y = !!sym(y_var))) +
    geom_point(size = 1.5, colour = "navy") +
    geom_smooth(method = "lm", aes(color = "Linear fit"), se = FALSE) +
    geom_segment(aes(x = min_x, y = min_x, xend = max_x, yend = max_x, color = "Diagonale"),
      linetype = "dashed", linewidth = 1
    ) +
    facet_wrap(~year, scales = "fixed", ncol = 3) +
    scale_color_manual(name = "", values = c("District" = "navy", "Linear fit" = "black", "Diagonale" = "red")) +
    labs(x = x_label, y = y_label, title = title) +
    theme_bw() +
    theme(legend.position = "bottom", strip.text = element_text(size = 8)) +
    guides(color = guide_legend(title = "Legend"))

  # Add R-squared as text on each facet
  plot <- plot +
    geom_text(
      data = r2_data, aes(x = Inf, y = Inf, label = paste("R-squared:", round(r_squared, 4))),
      hjust = 1.1, vjust = 1.1, size = 3, color = "grey", inherit.aes = FALSE
    )

  return(plot)
}

#' Plot Comparison of Penta1 and ANC1
#'
#' This function compares Penta1 and ANC1 indicators across different years
#'   using a scatter plot with a linear fit.
#'
#' @param data A data frame containing Penta1 and ANC1 data.
#' @return A ggplot object comparing Penta1 and ANC1.
#' @examples
#' \dontrun{
#'
#'   plot_comparison_penta1_anc1(data)
#' }
#' @export
plot_comparison_penta1_anc1 <- function(data) {
  plot_comparison(
    data = data,
    x_var = "anc1",
    y_var = "penta1",
    title = "Comparison of ANC1 and Penta1 by Year",
    x_label = "ANC1",
    y_label = "Penta1"
  )
}

#' Plot Comparison of Penta1 and Penta2
#'
#' This function compares Penta1 and Penta2 indicators across different years
#'   using a scatter plot with a linear fit.
#'
#' @param data A data frame containing Penta1 and Penta2 data.
#' @return A ggplot object comparing Penta1 and Penta2
#' @examples
#' \dontrun{
#'
#'   plot_comparison_penta1_penta2(data)
#' }
#' @export
plot_comparison_penta1_penta2 <- function(data) {

  plot_comparison(
    data = data,
    x_var = "penta1",
    y_var = "penta3",
    title = "Comparison of Penta1 and Penta3 by Year",
    x_label = "Penta1",
    y_label = "Penta3"
  )
}

#' Plot Comparison of OPV1 and OPV3
#'
#' This function compares OPV1 and OPV3 indicators across different years
#'   using a scatter plot with a linear fit.
#'
#' @param data A data frame containing OPV1 and OPV3 data.
#' @return A ggplot object comparing OPV1 and OPV3
#' @examples
#' \dontrun{
#'
#'   plot_comparison_opv1_opv3(data)
#' }
#' @export
plot_comparison_opv1_opv3 <- function(data) {
  plot_comparison(
    data = data,
    x_var = "opv1",
    y_var = "opv3",
    title = "Comparison of OPV1 and OPV3 by Year",
    x_label = "OPV1",
    y_label = "OPV3"
  )
}

#' Plot Comparison of OPV1 and ANC1
#'
#' This function compares OPV1 and ANC1 indicators across different years
#'   using a scatter plot with a linear fit.
#'
#' @param data A data frame containing OPV1 and ANC1 data.
#' @return A ggplot object comparing OPV1 and ANC1
#' @examples
#' \dontrun{
#'
#'   plot_comparison_opv1_anc1(data)
#' }
#' @export
plot_comparison_opv1_anc1 <- function(data) {
  plot_comparison(
    data = data,
    x_var = "opv1",
    y_var = "anc1",
    title = "Comparison of OPV1 and ANC1 by Year",
    x_label = "OPV1",
    y_label = "ANC1"
  )
}

#' Plot Comparison of PCV1 and PCV3
#'
#' This function compares PCV1 and PCV3 indicators across different years
#'   using a scatter plot with a linear fit.
#'
#' @param data A data frame containing PCV1 and PCV3 data.
#' @return A ggplot object comparing PCV1 and PCV3
#' @examples
#' \dontrun{
#'
#'   plot_comparison_pcv1_pcv3(data)
#' }
#' @export
plot_comparison_pcv1_pcv3 <- function(data) {
  plot_comparison(
    data = data,
    x_var = "pcv1",
    y_var = "pcv3",
    title = "Comparison of PCV1 and PCV3 by Year",
    x_label = "PCV1",
    y_label = "PCV3"
  )
}

#' Plot Comparison of ROTA1 and ROTA2
#'
#' This function compares ROTA1 and ROTA2 indicators across different years
#'   using a scatter plot with a linear fit.
#'
#' @param data A data frame containing ROTA1 and ROTA2 data.
#' @return A ggplot object comparing ROTA1 and ROTA2
#' @examples
#' \dontrun{
#'
#'   plot_comparison_rota1_rota2(data)
#' }
#' @export
plot_comparison_rota1_rota2 <- function(data) {
  plot_comparison(
    data = data,
    x_var = "rota1",
    y_var = "rota2",
    title = "Comparison of Rota1 and Rota2 by Year",
    x_label = "Rota1",
    y_label = "Rota2"
  )
}

#' Plot Comparison of IPV1 and IPV2
#'
#' This function compares IPV1 and IPV2 indicators across different years
#'   using a scatter plot with a linear fit.
#'
#' @param data A data frame containing IPV1 and IPV2 data.
#' @return A ggplot object comparing IPV1 and IPV2
#' @examples
#' \dontrun{
#'
#'   plot_comparison_ipv1_ipv2(data)
#' }
#' @export
plot_comparison_ipv1_ipv2 <- function(data) {
  plot_comparison(
    data = data,
    x_var = "ipv1",
    y_var = "ipv2",
    title = "Comparison of IPV1 and IPV2 by Year",
    x_label = "IPV1",
    y_label = "IPV2"
  )
}

#' Plot Comparison of Penta1 and PCV1
#'
#' This function compares Penta1 and PCV1 indicators across different years
#'   using a scatter plot with a linear fit.
#'
#' @param data A data frame containing Penta1 and PCV1 data.
#' @return A ggplot object comparing Penta1 and PCV1
#' @examples
#' \dontrun{
#'
#'   plot_comparison_penta1_pcv1(data)
#' }
#' @export
plot_comparison_penta1_pcv1 <- function(data) {
  plot_comparison(
    data = data,
    x_var = "penta1",
    y_var = "pcv1",
    title = "Comparison of Penta1 and PCV1 by Year",
    x_label = "Penta1",
    y_label = "PCV1"
  )
}

#' Plot Comparison of PCV3 and Penta3
#'
#' This function compares PCV3 and Penta3 indicators across different years
#'   using a scatter plot with a linear fit.
#'
#' @param data A data frame containing PCV3 and Penta3 data.
#' @return A ggplot object comparing PCV3 and Penta3
#' @examples
#' \dontrun{
#'
#'   plot_comparison_pcv3_penta3(data)
#' }
#' @export
plot_comparison_pcv3_penta3 <- function(data) {
  plot_comparison(
    data = data,
    x_var = "pcv3",
    y_var = "penta3",
    title = "Comparison of PCV3 and Penta3 by Year",
    x_label = "PCV3",
    y_label = "Penta3"
  )
}

#' Plot Comparison of OPV1 and Penta1
#'
#' This function compares OPV1 and Penta1 indicators across different years
#'   using a scatter plot with a linear fit.
#'
#' @param data A data frame containing OPV1 and Penta1 data.
#' @return A ggplot object comparing OPV1 and Penta1
#' @examples
#' \dontrun{
#'
#'   plot_comparison_opv1_penta1(data)
#' }
#' @export
plot_comparison_opv1_penta1 <- function(data) {

  plot_comparison(
    data = data,
    x_var = "opv1",
    y_var = "penta1",
    title = "Comparison of OPV1 and Penta1 by Year",
    x_label = "OPV1",
    y_label = "Penta1"
  )
}

#' Plot Comparison of OPV3 and Penta3
#'
#' This function compares OPV3 and Penta3 indicators across different years
#'   using a scatter plot with a linear fit.
#'
#' @param data A data frame containing OPV3 and Penta3 data.
#' @return A ggplot object comparing OPV3 and Penta3
#' @examples
#' \dontrun{
#'
#'   plot_comparison_opv3_penta3(data)
#' }
#' @export
plot_comparison_opv3_penta3 <- function(data) {

  plot_comparison(
    data = data,
    x_var = "opv3",
    y_var = "penta3",
    title = "Comparison of OPV3 and Penta3 by Year",
    x_label = "OPV3",
    y_label = "Penta3"
  )
}

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
      across(starts_with("adeq_"), ~ round(.x * 100, 1)),
      across(c(anc1, penta1, penta3, opv1, opv3, pcv1, pcv3, rota1, rota2, ipv1, ipv2, bcg), round, 1)
    ) %>%
    select(-starts_with("adeq_"))

  new_tibble(
    data_collapsed,
    class = "cd_check_ratios"
  )
}

#' Check for Missing Values by Year
#'
#' This function calculates the percentage of districts with no missing values
#'   for each year.
#'
#' @param data A data frame containing missing value indicators for districts and
#'   years.
#' @return A data frame summarizing the percentage of districts with no missing
#'   values by year.
#' @examples
#' \dontrun{
#'
#'   check_no_missing_value(data)
#' }
#' @export
check_no_missing_value <- function(data) {

  year = district = . = NULL

  if (!inherits(data, "cd_data")) stop("The data object must be of class 'cd_data'.")

  # Step 1: Identify missing data indicators
  missing_data_vars <- names(data$merged_data)[grepl("^mis_", names(data$merged_data))]

  # Step 2: Calculate percentage of districts with no missing values by year
  districts_no_missing <- data$merged_data %>%
    select(year, district, all_of(missing_data_vars)) %>%
    summarise(
      across(everything(), ~ mean(1 - .x, na.rm = TRUE) * 100),
      .by = year
    ) %>%
    mutate(mis_mean = rowMeans(select(., starts_with("mis_")), na.rm = TRUE)) %>%
    mutate(across(starts_with("mis_"), round, 2))

  structure(c(data, districts_no_missing), class = "cd_check_no_missing_values")

  return(districts_no_missing)
}

#' Check for Missing Values in Each Year
#'
#' This function calculates the percentage of non-missing values for all
#'   indicators for each year.
#'
#' @param data A data frame containing missing value indicators for multiple years.
#' @return A data frame summarizing the percentage of non-missing values by year.
#' @examples
#' \dontrun{
#'
#'   check_no_missing_year(data)
#' }
#' @export
check_no_missing_year <- function(data) {

  year = . = NULL

  if (!inherits(data, "cd_data")) stop("The data object must be of class 'cd_data'.")

  # Mark Missing Values
  dt <- data$merged_data %>%
    mutate(
      !!paste0("mis_", var) := ifelse(is.na(!!sym(var)), 1, 0)
    )

  # Step 1: Identify missing data indicators
  missing_data_vars <- names(dt)[grepl("^mis_", names(dt))]

  # Calculate percentage of non-missing data by year
  mis_summary_by_year <- dt %>%
    select(year, all_of(missing_data_vars)) %>%
    mutate(across(all_of(missing_data_vars), as.numeric)) %>%
    summarise(
      across(everything(), ~ mean(1 - .x, na.rm = TRUE) * 100),
      .groups = "drop",
      .by = year
    ) %>%
    mutate(mis_mean = rowMeans(select(., starts_with("mis_")), na.rm = TRUE)) %>%
    mutate(across(everything(), round, 2))

  structure(c(data, mis_summary_by_year), class = "cd_check_no_missing_year")

  return(mis_summary_by_year)
}

#' Check for Missing Values by District
#'
#' This function calculates the percentage of districts with no missing values
#'   for each year.
#'
#' @param data A data frame containing missing value indicators for districts and
#'   years.
#' @return A data frame summarizing the percentage of districts with no missing
#'   values by year.
#' @examples
#' \dontrun{
#'
#'   check_no_missing_district(data)
#' }
#' @export
check_no_missing_district <- function(data) {

  year = district = . = NULL

  if (!inherits(data, "cd_data")) stop("The data object must be of class 'cd_data'.")

  # Step 1: Identify missing data indicators
  missing_data_vars <- names(data$merged_data)[grepl("^mis_", names(data$merged_data))]

  # Step 2: Calculate percentage of districts with no missing values by year
  districts_no_missing <- data$merged_data %>%
    select(year, district, all_of(missing_data_vars)) %>%
    summarise(
      across(everything(), ~ mean(1 - .x, na.rm = TRUE) * 100),
      .by = year
    ) %>%
    mutate(mis_mean = rowMeans(select(., starts_with("mis_")), na.rm = TRUE)) %>%
    mutate(across(starts_with("mis_"), round, 2))

  structure(c(data, districts_no_missing), class = "cd_check_no_missing_district")

  return(districts_no_missing)
}
