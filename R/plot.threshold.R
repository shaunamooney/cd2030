#' @export
plot.cd_threshold <- function(x,
                              denominator = c('dhis2', 'anc1', 'penta1'),
                              ...) {

  denom <-  arg_match(denominator)
  indicator <-  attr(x, 'indicator')
  admin_level <-  attr(x, 'admin_level')
  admin_level <- switch (
    admin_level,
    adminlevel_1 = 'Admin Level 1',
    district = 'District'
  )

  title = if (indicator == 'coverage') {
    str_glue('Pecentage of {admin_level} with Vaccination Coverage > 90%')
  } else {
    str_glue('Percent of {admin_level} with Dropout Rate of < 10%')
  }

  x %>%
    filter(denominator == denom) %>%
    ggplot(aes(indicator, value, fill = factor(year))) +
    scale_y_continuous(breaks = scales::pretty_breaks(6), expand = c(0, 0)) +
    geom_col(position = 'dodge') +
    geom_hline(yintercept = 80, colour = 'red', size = 1.5) +
    labs(title = title,
         x = '',
         y = '') +
    scale_fill_manual(values = c('darkgreen', 'darkgoldenrod3', 'firebrick4', 'springgreen3', 'darkolivegreen3', 'steelblue2')) +
    cd_plot_theme()
}
