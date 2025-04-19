#' Retrieve Attribute or Abort
#'
#' Safely retrieves an attribute from an object or aborts with an error.
#' @param .data An object
#' @param attr_name The name of the attribute to retrieve
#'
#' @return The attribute value
#'
#' @keywords internal
attr_or_abort <- function(.data, attr_name) {
  value <- attr(.data, attr_name)
  if (is.null(value)) {
    cd_abort(c('x' = str_glue('Missing attribute: {attr_name}')))
  }
  value
}

#' Get Country Name
#'
#' @param .data A `cd_data` object
#'
#' @return The country name as a character string
#'
#' @export
get_country_name <- function(.data) {
  check_cd_data(.data)
  attr_or_abort(.data, 'country')
}

#' Get Country ISO3 Code
#'
#' @param .data A `cd_data` object
#'
#' @return The ISO3 country code as a character string
#'
#' @export
get_country_iso3 <- function(.data) {
  check_cd_data(.data)
  attr_or_abort(.data, 'iso3')
}

#' List All Routine Vaccine Indicators
#'
#' Returns all vaccine indicators used in coverage estimates
#'
#' @return A character vector of vaccine names
#'
#' @export
list_vaccines  <- function() {
  c('bcg', 'ipv1', 'ipv2', 'measles1', 'measles2', 'opv1', 'opv2', 'opv3',
    'penta1', 'penta2', 'penta3', 'pcv1', 'pcv2', 'pcv3', 'rota1', 'rota2')
}

#' List All Vaccine and Related Coverage Indicators
#'
#' Returns a vector of standard vaccine and related indicators used in coverage
#' and dropout analysis. This includes routine immunizations, tracer indicators,
#' and derived dropout/coverage metrics commonly used in DHIS2-based health reporting.
#'
#' @return A character vector of 24 indicator names. These cover vaccine doses
#'   (e.g., `penta1`, `measles1`, `pcv3`), derived indicators
#'   (e.g., `dropout_penta13`, `undervax`), and service delivery markers
#'   (e.g., `anc1`, `instdeliveries`).
#'
#' @examples
#' list_vaccine_indicators()
#'
#' @export
list_vaccine_indicators <- function() {
  c('anc1', 'bcg', 'instlivebirths', 'instdeliveries', 'ipv1', 'ipv2', 'measles1', 'measles2',
    'opv1', 'opv2', 'opv3', 'pcv1', 'pcv2', 'pcv3', 'penta1', 'penta2', 'penta3',
    'rota1', 'rota2', 'dropout_measles12', 'dropout_penta13', 'dropout_penta1mcv1',
    'dropout_penta3mcv1', 'undervax', 'zerodose')
}

#' List Tracer Vaccines
#'
#' Returns a subset of vaccines used in tracer metrics
#'
#' @return A character vector of tracer vaccines
#'
#' @export
list_tracer_vaccines  <- function() {
  c('bcg', 'measles1', 'opv1', 'opv2', 'opv3', 'penta1', 'penta2', 'penta3')
}

#' Get Indicator Groups
#'
#' Default grouping of indicators used in CD2030 coverage framework
#'
#' @return A named list of grouped indicators
#'
#' @export
get_indicator_groups <- function() {
  list(
    anc = c('anc1'),
    idelv = c('ideliv', 'instlivebirths'),
    vacc = list_vaccines ()
  )
}

#' Get Indicator Group Names
#' @return Character vector of indicator group names
#' @export
get_indicator_group_names <- function() names(get_indicator_groups())

#' @title Get All Indicators
#' @description Flatten all indicators from all groups
#' @return Character vector of all indicators
#' @export
get_all_indicators <- function() list_c(get_indicator_groups())

#' @title Get Named Indicator Vector
#' @description Each indicator is named by its group
#' @return Named character vector of indicators
#' @export
get_named_indicators <- function() {
  groups <- get_indicator_groups()
  out <- list_c(groups)
  names(out) <- rep(names(groups), lengths(groups))
  out
}

#' Get Population Denominator Column Based on Indicator Only
#'
#' @param indicator Character scalar. Indicator name (e.g., "penta1", "measles1")
#' @param denominator Character scalar. The denominator for the indicator
#'
#' @return A string naming the corresponding population column
#'
#' @export
get_population_column <- function(indicator, denominator) {
  indicator <- arg_match(indicator, list_vaccine_indicators())
  denominator <- arg_match(denominator, c('dhis2', 'anc1', 'penta1'))
  population <- case_match(
    indicator,
    c('anc1', 'anc4') ~ 'totpreg',
    c('bcg', 'instlivebirths', 'instdeliveries') ~ if_else(denominator == 'dhis2', 'totbirths', 'totlbirths'),
    c('penta1', 'penta2', 'penta3', 'pcv1', 'pcv2', 'pcv3', 'rota1',
      'rota2', 'ipv1', 'ipv2', 'opv1', 'opv2', 'opv3', "undervax","dropout_penta13","zerodose","dropout_penta3mcv1","dropout_penta1mcv1") ~ 'totinftpenta',
    c('measles1', 'dropout_measles12') ~ 'totinftmeasles',
    'measles2' ~ 'totmeasles2'
  )
  if (is.na(population)) return(NA)
  return(paste(population, denominator, sep = '_'))
}
