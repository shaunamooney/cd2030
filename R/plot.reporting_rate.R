#' @export
plot.cd_reporting_rate <- function(x,
                                   plot_type = c('heat_map', 'bar'),
                                   indicator = c('anc_rr', 'idelv_rr', 'vacc_rr'),
                                   threshold = 90,
                                   ...) {

  check_scalar_integerish(threshold)

  plot_type = arg_match(plot_type)
  indicator <- arg_match(indicator)
  admin_level <- attr(x, 'admin_level')

  if (admin_level == "national") {
    abort("`plot.cd_reporting_rate` does not support national-level data.")
  }

  all_years <- sort(unique(x$year))

  if (plot_type == 'heat_map') {

    greater <- paste0('>= ', threshold)
    mid <- paste0(' >= 70 and < ', threshold)
    low <- '< 70'

    dt <- x %>%
      mutate(
        year = factor(year, levels = all_years),
        color_category = case_when(
          !!sym(indicator) >= threshold ~ greater,
          !!sym(indicator) >= 70 & !!sym(indicator) < threshold ~ mid,
          !!sym(indicator) < 70 ~ low,
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
    low_value <- min(x[[indicator]], na.rm = TRUE)
    high_value <- max(x[[indicator]], na.rm = TRUE)

    color_vals <- c("red", "orange", "forestgreen")

    # Define breakpoints dynamically
    breaks_vals <- if (threshold >= 70) {
      c(low_value, 70, threshold, 100)
    } else {
      c(low_value, mean(low_value, threshold, na.rm = TRUE), threshold, 100)
    }

    ggplot(x, aes(year, !!sym(indicator), fill = !!sym(indicator))) +
      geom_col() +
      facet_wrap(as.formula(paste0('~', admin_level))) +
      scale_x_continuous(
        breaks = all_years,
        expand = expansion(mult = c(0, 0.05))
      ) +
      labs(title = paste0('Reporting rates by years and ', admin_level), x = 'Year', y = 'Reporting Rate') +
      scale_fill_gradientn(
        colors = color_vals,
        values = scales::rescale(breaks_vals, to = c(0, 1)),  # Ensures precise cutoffs
        breaks = scales::pretty_breaks(n = 5)(c(low_value, 100)),  # Uniformly spread breaks
        labels = scales::pretty_breaks(n = 5)(c(low_value, 100)),
        limits = c(low_value, 100),  # Ensure full scale is covered
        name = "Reporting Rate"
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
