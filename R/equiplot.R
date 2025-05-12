#' Create Dot Plots for Equity Analysis
#'
#' `equiplot` generates a dot plot to visualize the distribution of variables across
#' different groups (e.g., countries, regions, or interventions). It is designed for
#' equity analysis by plotting values for variables like health intervention coverage
#' across subgroups, allowing insights into disparities.
#'
#' @param .data A data frame containing the data to be plotted.
#' @param variables A vector of column names in `.data` representing the variables to plot.
#' @param group_by A column in `.data` to group the data by (e.g., countries or regions).
#' @param x_title Optional. A title for the x-axis.
#' @param legend_title Optional. A title for the legend.
#' @param reverse_y_axis Logical. Whether to reverse the y-axis order. Default is FALSE.
#' @param connect_dots Logical. Whether to connect the dots with lines. Default is TRUE.
#' @param dot_size Optional. Controls the size of the dots (1 to 5, with 5 being the largest).
#'
#' @return A ggplot object representing the dot plot.
#'
#' @examples
#' \dontrun{
#'   equiplot(
#'     .data = data,
#'     variables = c('Var1', 'Var2', 'Var3'),
#'     group_by = Country,
#'     x_title = 'Percentage (%)',
#'     legend_title = 'Variables',
#'     reverse_y_axis = TRUE
#'   )
#' }
#'
#' @export
equiplot <- function(.data, variables, group_by, x_title = NULL, legend_title = NULL,
                     reverse_y_axis = FALSE, connect_dots = TRUE, dot_size = NULL) {

  Variable = Value = over_factor = NULL

  # check_equity_data(.data)

  # Input checks and data preparation
  if (!is.data.frame(.data)) {
    cd_abort(
      c('x' = '{.arg .data} must be a data frame.')
    )
  }
  if (!all(variables %in% names(.data))) {
    cd_abort(
      c('x' = 'The following variables are not found in the data provided: {.arg {variables}}.')
    )
  }

  group_by <- as.character(ensym(group_by))
  if (!(group_by %in% names(.data))) {
    cd_abort(
      c('x' = '{.arg group_by} variable not found in data.')
    )
  }

  # Handle over variable using tidyverse
  .data <- .data %>%
    mutate(over_factor = fct_inorder(as.character(!!sym(group_by))))

  nlevels <- length(variables)

  # Color palettes (adapted from Stata code)
  colors <- switch(as.character(nlevels),
                   '2' = c('#15353B', '#FFB300'),
                   '3' = c('#15353B', '#46919D', '#FFB300'),
                   '4' = c('#15353B', '#005866', '#46919D', '#FFDA83'),
                   '5' = c('#15353B', '#005866', '#46919D', '#FFDA83', '#FFB300'),
                   '6' = c('#814374', '#51A39D', '#B7695C', '#CDBB79', '#D4D4D4', '#06425C'),
                   '7' = c('#814374', '#51A39D', '#B7695C', '#CDBB79', '#D4D4D4', '#06425C', '#968989'),
                   '10' = c('#201E1E', '#0C444E', '#415B61', '#196975', '#39767F', '#73A0A7', '#FFE7AB', '#FFCD58', '#D5B670', '#C38B09'),
                   grDevices::rainbow(nlevels) # Default rainbow if nlevels is not defined
  )

  # Dot size
  dot_size_scale <- ifelse(is.null(dot_size), 4, pmin(pmax(dot_size, 1), 5))

  # Data reshaping and plotting with ggplot2
  p <- .data %>%
    pivot_longer(cols = all_of(variables), names_to = 'Variable', values_to = 'Value') %>%
    mutate(Variable = fct_inorder(factor(Variable, levels = variables))) %>%
    ggplot(aes(x = Value, y = over_factor, color = Variable)) +
      geom_point(size = dot_size_scale) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(0, 100)) +
      scale_color_manual(values = colors, name = legend_title) +
      labs(x = x_title, y = '') +
      cd_plot_theme() +
      theme(
        panel.border = element_blank(),
        panel.grid.major.y = element_line(colour = 'lightblue1', linetype = 'dashed'),
        panel.grid.major.x = element_line(colour = 'gray90', linetype = 'dashed'),

        legend.background = element_blank(),
        legend.title = element_text(color = '#005866', hjust = 0.5),
        legend.position = 'top',
        legend.title.position = 'top',

        axis.text = element_text(color = '#005866'),
        axis.line = element_line(color = '#005866'),
        axis.ticks = element_line(color = '#005866')
      )

  if(connect_dots){
    p <- p + geom_line(aes(group = over_factor), color = 'dodgerblue')
  }
  if (reverse_y_axis) {
    p <- p + scale_y_discrete(limits = rev)
  }

  p
}

#' A Specialized Dot Plot for Area of Residence Analysis
#'
#' `equiplot_area` generates a dot plot comparing rural and urban coverage of a
#' specific indicator across years.
#'
#' @param .data A data frame containing the data to be plotted.
#' @param indicator A string specifying the indicator to be analyzed (e.g., 'sba').
#' @param x_title Optional. A title for the x-axis. Defaults to '<indicator> Coverage (%)'.
#'
#' @return A ggplot object representing the dot plot.
#'
#' @examples
#' \dontrun{
#'   equiplot_area(data, indicator = 'sba')
#' }
#'
#' @export
equiplot_area <- function(.data,
                          indicator,
                          x_title = NULL,
                          dot_size = NULL) {

  year = NULL

  check_equity_data(.data)

  indicator <- arg_match(indicator, list_vaccine_indicators())

  if (is.null(x_title)) {
    x_title <- paste0(indicator, ' Coverage (%)')
  }

  indicator <- paste0('r_', indicator)

  .data %>%
    select(year, level, contains(indicator)) %>%
    mutate(level = str_to_title(level)) %>%
    pivot_wider(names_from = level, values_from = !!sym(indicator)) %>%
    equiplot(variables = c('Rural', 'Urban'),
             group_by = year,
             x_title = x_title,
             legend_title = 'Area of residence',
             reverse_y_axis = TRUE,
             dot_size = dot_size)

}

#' A Specialized Dot Plot for Maternal Education Analysis
#'
#' `equiplot_education` generates a dot plot comparing coverage across maternal education levels
#' (no education, primary, and secondary or higher) for a specific indicator across years.
#'
#' @param .data A data frame containing the data to be plotted.
#' @param indicator A string specifying the indicator to be analyzed (e.g., 'sba').
#' @param x_title Optional. A title for the x-axis. Defaults to '<indicator> Coverage (%)'.
#'
#' @return A ggplot object representing the dot plot.
#'
#' @examples
#' \dontrun{
#'   equiplot_education(data, indicator = 'sba')
#' }
#'
#' @export
equiplot_education <- function(.data,
                               indicator,
                               x_title = NULL,
                               dot_size = NULL) {

  year = NULL

  check_equity_data(.data)

  indicator <- arg_match(indicator, list_vaccine_indicators())

  if (is.null(x_title)) {
    x_title <- paste0(indicator, ' Coverage (%)')
  }

  indicator <- paste0('r_', indicator)

  .data %>%
    select(year, level, contains(indicator)) %>%
    mutate(
      level = case_match(level,
                         'none' ~ 'No education',
                         'primary' ~ 'Primary',
                         'secondary+' ~ 'Secondary or higher')) %>%
    pivot_wider(names_from = level, values_from = !!sym(indicator)) %>%
    equiplot(variables = c('No education', 'Primary', 'Secondary or higher'),
             group_by = year,
             x_title = x_title,
             legend_title = 'Maternal Education',
             reverse_y_axis = TRUE,
             dot_size = dot_size)

}

#' A Specialized Dot Plot for Wealth Quintile Analysis
#'
#' `equiplot_wealth` generates a dot plot comparing coverage across wealth quintiles (Q1 to Q5)
#' for a specific indicator across years.
#'
#' @param .data A data frame containing the data to be plotted.
#' @param indicator A string specifying the indicator to be analyzed (e.g., 'sba').
#' @param x_title Optional. A title for the x-axis. Defaults to '<indicator> Coverage (%)'.
#'
#' @return A ggplot object representing the dot plot.
#'
#' @examples
#' \dontrun{
#'   equiplot_wealth(data, indicator = 'sba')
#' }
#' # Example Usage:
#'
#' @export
equiplot_wealth <- function(.data,
                            indicator,
                            x_title = NULL,
                            dot_size = NULL) {
  year = NULL

  check_equity_data(.data)

  indicator <- arg_match(indicator, list_vaccine_indicators())

  indicator_name <- paste0('r_', indicator)

  if (is.null(x_title)) {
    x_title <- paste0(indicator, ' Coverage (%)')
  }

  .data %>%
    select(year, level, contains(indicator_name)) %>%
    mutate(level = str_to_title(level)) %>%
    pivot_wider(names_from = level, values_from = !!sym(indicator_name)) %>%
    equiplot(variables = c('Q1', 'Q2', 'Q3', 'Q4', 'Q5'),
             group_by = year,
             x_title = x_title,
             legend_title = 'Wealth quintiles',
             reverse_y_axis = TRUE,
             dot_size = dot_size)

}
