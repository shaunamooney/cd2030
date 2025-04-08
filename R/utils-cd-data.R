# Generic safe attribute getter
attr_or_abort <- function(.data, attr_name) {
  value <- attr(.data, attr_name)
  if (is.null(value)) {
    cd_abort(c('x' = str_glue('Missing attribute: {attr_name}')))
  }
  value
}

# Accessors for standard cd_data attributes
#' @export
get_vaccine_tracers <- function(.data) attr_or_abort(.data, 'tracers')
#' @export
get_country_name <- function(.data) attr_or_abort(.data, 'country')
#' @export
get_country_iso3 <- function(.data) attr_or_abort(.data, 'iso3')
#' @export
get_indicator_groups <- function(.data) attr_or_abort(.data, 'indicator_groups')

# Variants of indicator_groups
#' @export
get_indicator_group_names <- function(.data) names(get_indicator_groups(.data))
#' @export
get_all_indicators <- function(.data) list_c(get_indicator_groups(.data))
#' @export
get_named_indicators <- function(.data) {
  groups <- get_indicator_groups(.data)
  out <- list_c(groups)
  names(out) <- rep(names(groups), lengths(groups))
  out
}

