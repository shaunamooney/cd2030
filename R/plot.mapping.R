#' Plot Subnational Mapping Data
#'
#' This function generates a subnational mapping plot for a specified indicator,
#' using a gradient color scheme.
#'
#' @param x A data frame containing the subnational mapping data.
#' @param indicator A string specifying the indicator to visualize. Options include:
#'   - `"anc1"`
#'   - `"bcg"`
#'   - `"measles1"`
#'   - `"measles2"`
#'   - `"measles3"`
#'   - `"opv1"`
#'   - `"opv2"`
#'   - `"opv3"`
#'   - `"pcv1"`
#'   - `"pcv2"`
#'   - `"pcv3"`
#'   - `"penta1"`
#'   - `"penta2"`
#'   - `"penta2"`
#'   - `"rota1"`
#'   - `"rota2"`
#'   - `"instdeliveries"`
#'   - `"ipv1"`
#'   - `"ipv2"`
#'   - `"undervax"`
#'   - `"zerodose"`
#'   - `"dropout_measles12"`
#'   - `"dropout_penta13"`
#'   - `"dropout_penta3mcv1"`
#' @param denominator A string specifying the denominator used in the calculation. Options are:
#'   - `"dhis2"`
#'   - `"anc1"`
#'   - `"penta1"`
#' @param palette A string specifying the color palette. Options include:
#'   - `"Reds"`
#'   - `"Blues"`
#'   - `"Greens"`
#'   - `"Purples"`
#'   - `"YlGnBu"`
#' @param plot_year Optional. A numeric value or vector specifying the year(s) to
#'   include in the plot. If `NULL`, all years in the data are included.
#' @param title Optional. A string specifying the plot title. Defaults to
#'   `"Distribution of [indicator] by Regions"`.
#' @param ... Other params to pass to plot
#'
#' @return A `ggplot` object representing the subnational mapping plot.
#'
#' @examples
#' \dontrun{
#'   # Plot Penta1 coverage with DHIS2 denominator
#'   plot.cd_mapping(
#'     x = data,
#'     indicator = "penta1",
#'     denominator = "dhis2",
#'     palette = "Blues",
#'     plot_year = 2022
#'   )
#' }
#'
#' @export
plot.cd_mapping <- function(x,
                            indicator = c("anc1", "bcg", "measles1", "measles2", "measles3", "opv1", "opv2", "opv3",
                                          "pcv1", "pcv2", "pcv3", "penta1", "penta2", "penta3", "rota1", "rota2", "instdeliveries",
                                          "ipv1", "ipv2", "undervax", "zerodose", "dropout_measles12", "dropout_penta13", "dropout_penta3mcv1"),
                            denominator = c('dhis2', 'anc1', 'penta1'),
                            palette = c('Reds', 'Blues', 'Greens', 'Purples', 'YlGnBu'),
                            plot_year = NULL,
                            title = NULL,
                            ...) {

  year = NULL

  check_required(indicator)
  check_required(denominator)

  indicator <- arg_match(indicator)
  denominator <- arg_match(denominator)
  palette <- arg_match(palette)

  column <- paste0('cov_', indicator, '_', denominator)
  if (!column %in% colnames(x)) {
    cd_abort(
      c('x' = '{.field {column}} not in the data')
    )
  }

  if(is.null(title)) {
    title <- paste('Distribution of', indicator, ' by Regions')
  }

  x %>%
    filter(if(is.null(plot_year)) TRUE else year %in% plot_year) %>%
    st_as_sf() %>%
    st_set_crs(4326) %>%
    st_transform(crs = 4326) %>%
    ggplot() +
      geom_sf(aes(fill = !!sym(column))) +
      coord_sf(default_crs = 4326, lims_method = "geometry_bbox") +
      facet_wrap(~ year, scales = 'fixed', ncol = 5) +
      scale_fill_gradientn(colors = RColorBrewer::brewer.pal(7, palette)) +
      labs(
        title = title,
        fill = column,
        caption = 'Data Source: DHIS-2 analysis'
      ) +
      cd_plot_theme() +
      theme(
        panel.border = element_blank(),
        panel.spacing = unit(1, 'lines'),

        legend.key.size = unit(6, 'mm'),
        legend.background = element_blank(),
        legend.title = element_text(size = 11),

        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),

        strip.text = element_text(size = 12, face = 'bold'),

        aspect.ratio = 1
      )
}
