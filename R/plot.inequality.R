#' Plot Subnational Health Coverage Analysis
#'
#' Generates a plot to visualize health coverage data across subnational units,
#' distinguishing between the national mean and subnational coverage. The Mean
#' Absolute Difference to the Mean (MADM) is displayed as an indicator on the
#' y-axis. Titles, subtitles, and labels dynamically adjust based on the chosen
#' indicator, denominator, and level.
#'
#' @param x A `cd_inequality` object returned by [calculate_inequality()].
#' @param indicator Character. Indicator to plot
#' @param denominator Character. The denominator to use
#' @param ... Additional arguments passed to the plotting function.
#'
#' @return A `ggplot` object displaying the subnational health coverage plot.
#'
#' @examples
#' \dontrun{
#'   data <- calculate_inequality(.data, "Kenya", admin_level = "district",
#'                               indicator = "measles1", denominator = "penta1")
#'   plot(data)
#' }
#'
#' @export
plot.cd_inequality <- function(x,
                               indicator = c('anc1', 'bcg', 'measles1', 'measles2', 'opv1', 'opv2', 'opv3',
                                             'pcv1', 'pcv2', 'pcv3', 'penta1', 'penta2', 'penta3',
                                             'rota1', 'rota2', 'ipv1', 'ipv2', 'dropout_penta13', 'dropout_penta3mcv1'),
                               denominator = c('dhis2', 'anc1', 'penta1'),
                               ...) {

  year = nat = madm = NULL

  admin_level <- attr(x, 'admin_level')
  indicator <- arg_match(indicator)
  denominator <- arg_match(denominator)
  data <- filter_inequality(x, indicator, denominator)

  title <- switch (indicator,
                   anc1 = 'Antenatal care 1+ visits',
                   anc4 = 'Antenatal care 4+ visits',
                   instdeliveries = 'Institutional deliveries',
                   instlivebirths = 'Institutional live births',
                   bcg = 'BCG vaccine',
                   penta1 = 'Penta vaccine - 1st dose',
                   penta3 = 'Penta vaccine - 3rd dose',
                   measles1 = 'Measles vaccine - 1st dose',
                   measles2 = 'Measles vaccine - 2nd dose',
                   sba = 'Skilled attendant at birth',
                   opv1 = 'Polio vaccine - 1st dose',
                   opv2 = 'Polio vaccine - 2nd dose',
                   opv3 = 'Polio vaccine - 3rd dose',
                   penta2 = 'Penta vaccine - 2nd dose',
                   pcv1 = 'Pneumococcal vaccine - 1st dose',
                   pcv2 = 'Pneumococcal vaccine - 2nd dose',
                   pcv3 = 'Pneumococcal vaccine - 3rd dose',
                   rota1 = 'Rota vaccine - 1st dose',
                   rota2 = 'Rota vaccine - 2nd dose',
                   ipv1 = 'IPV vaccine - 1st dose',
                   ipv2 = 'IPV vaccine - 2nd dose'
  )

  subtitle <- switch (admin_level,
                      district = 'Subnational unit: district level',
                      adminlevel_1 = 'Subnational unit: admin 1 level')

  caption <- switch (denominator,
                     dhis2 = 'Denominators derived from projected live births (DHIS2)',
                     anc1 = 'Denominators derived from ANC1 estimates',
                     penta1 = 'Denominators derived from Penta 1 estimates')

  y_label <- ifelse(indicator == "low_bweight", "Prevalence (%)", "Coverage (%)")
  max_y <- robust_max(data$rd_max, 100)
  limits <- c(0, max_y)
  breaks <- scales::pretty_breaks(n = 11)(limits)
  second_last_break <- sort(breaks, decreasing = TRUE)[2]
  max_break <- robust_max(breaks, 0)

  ggplot(data) +
    geom_point(aes(x = year, y = !!sym(paste0("cov_", indicator, "_", denominator)), color = "Coverage at subnational unit"),
               size = 3) +
    geom_point(aes(x = year, y = nat, color = "National coverage"), size = 1.5, shape = 3, stroke = 1.5) +
    geom_text(aes(x = year, y = second_last_break, label = round(madm, 2)),
              color = "black", fontface = "bold", vjust = 0.5, size = 4) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "gray60") +
    labs(x = "Year", y = y_label,
         title = title, subtitle = subtitle,
         caption = caption) +
    scale_y_continuous(expand = c(0, 0),
                       limits = range(c(limits, max_break), na.rm = TRUE),
                       breaks = breaks,
                       labels = function(y) ifelse(y == second_last_break, "MADM", ifelse(y == max_break, '', as.character(y)))  # Replace max_y - 10 with "MADM"
    ) +
    scale_color_manual(values = c('Coverage at subnational unit' = 'skyblue3', 'National coverage' = 'red')) +
    cd_plot_theme() +
    theme(
      panel.border = element_blank(),
      panel.grid.major.y = element_line(colour = 'lightblue1', linetype = 'dashed'),
      panel.grid.major.x = element_line(colour = 'gray90', linetype = 'dashed'),

      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),

      axis.line = element_line(),
      legend.position = 'right',
      legend.background = element_blank()
    )
}
