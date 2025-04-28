#' Plot Missing Summary
#'
#' This method visualizes missing results for immunization indicators
#'
#' @param x A `cd_outlier` object containing pre-processed outlier data.
#' @param indicator Optional. One of the supported indicators (`"opv1"`, `"penta3"`, etc.)
#'   to visualize in the plot. If `NULL` all indicators will be shown.
#' @param ... Reserved for future use.
#'
#' @return A `ggplot` or `plotly` object depending on the selection type.
#'
#' @examples
#' \dontrun{
#'   # Region-level summary
#'   plot(missing_data, indicator = "penta3")
#' }
#'
#' @export
plot.cd_completeness_summary <- function(x,
                            indicator = NULL,
                            ...) {

  admin_level <- attr(x, 'admin_level')

  indicator <- if (is.null(indicator) || indicator == '') {
    NULL
  } else {
    arg_match(indicator, list_vaccines())
  }

  print(x)

  if (is.null(indicator)) {
    x <- x %>%
      pivot_longer(cols = starts_with('mis_'), names_to = 'indicator') %>%
      mutate(indicator = str_remove(indicator, 'mis_'))

    ggplot(x, aes(x = !!sym(admin_level), y = indicator, fill = value)) +
      geom_tile(color = 'white') +
      geom_text(aes(label = value), color = 'black', size = 3, vjust = 0.5) +
      scale_fill_gradient2(low = 'red3', mid = 'orange', high = 'forestgreen', midpoint = 80) +
      labs(x = admin_level, y = 'Indicator', fill = 'Value') +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
  } else {
    column_name <- paste0('mis_', indicator)
    ggplot(x, aes(x = !!sym(admin_level), y = factor(year), fill = !!sym(column_name))) +
      geom_tile(color = 'white') +
      geom_text(aes(label = !!sym(column_name)), color = 'black', size = 3, vjust = 0.5) +
      scale_fill_gradient2(low = 'red3', mid = 'orange', high = 'forestgreen', midpoint = 80) +
      labs(x = admin_level, y = 'Year', fill = paste0(indicator, ' Value')) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
  }
}
