#' Plot Subnational Health Coverage Analysis
#'
#' Generates a plot to visualize health coverage data across subnational units,
#' distinguishing between the national mean and subnational coverage. The Mean
#' Absolute Difference to the Mean (MADM) is displayed as an indicator on the
#' y-axis. Titles, subtitles, and labels dynamically adjust based on the chosen
#' indicator, denominator, and level.
#'
#' @param x A `cd_subnational_coverage` object returned by `analyze_subnational_coverage`.
#' @param ... Additional arguments passed to the plotting function.
#'
#' @return A `ggplot` object displaying the subnational health coverage plot.
#'
#' @examples
#' \dontrun{
#' data <- analyze_subnational_coverage(.data, "Kenya", level = "district",
#'   indicator = "measles1", denominator = "penta1")
#' plot(data)
#' }
#'
#' @export
plot.cd_subnational_coverage <- function(x, ...) {

  year = national_mean = madm = NULL

  indicator <- attr(x, 'indicator')
  denominator <- attr(x, 'denominator')
  level <- attr(x, 'level')

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

  subtitle <- switch (level,
                      district = 'Subnational unit: district level',
                      adminlevel_1 = 'Subnational unit: admin 1 level'
  )

  caption <- switch (denominator,
                     dhis2 = 'Denominators derived from projected live births (DHIS2)',
                     anc1 = 'Denominators derived from ANC1 estimates',
                     penta1 = 'Denominators derived from Penta 1 estimates'
  )

  y_label <- ifelse(indicator == "low_bweight", "Prevalence (%)", "Coverage (%)")
  max_y <- max(x$rd_max, na.rm = TRUE)

  ggplot(x) +
    geom_point(aes(x = year, y = !!sym(paste0("cov_", indicator, "_", denominator)), color = "Coverage at subnational unit"),
               position = position_jitter(width = 0.05), size = 3) +
    geom_point(aes(x = year, y = national_mean, color = "National coverage"), size = 1.5, shape = 3, stroke = 1.5) +
    geom_text(aes(x = year, y = max_y - 10, label = round(madm, 2)),
              color = "black", fontface = "bold", vjust = 0.5, size = 4) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "gray60") +
    labs(x = "Year", y = y_label,
         title = title, subtitle = subtitle,
         caption = caption) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, max_y),
                       breaks = c(scales::pretty_breaks(10)(0:max_y), max_y - 10),  # Add max_y - 10 as a custom break
                       labels = function(y) ifelse(y == max_y - 10, "MADM", y)  # Replace max_y - 10 with "MADM"
    ) +
    scale_color_manual(values = c('Coverage at subnational unit' = 'skyblue3', 'National coverage' = 'red')) +
    cd_plot_theme() +
    theme(
      panel.border = element_blank(),

      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),

      axis.line = element_line(),
      legend.position = 'right',
      legend.background = element_blank()
    )
}
