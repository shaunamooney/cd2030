#' Summarize National Denominators Data
#'
#' Provides a filtered summary of the `cd_national_denominators` object,
#' displaying total population and live birth data from DHIS-2 and UN sources for
#' the selected country.
#'
#' @param x A `cd_national_denominators` object containing national-level
#'   demographic data.
#' @param ... Additional arguments (not used in this function).
#'
#' @return A character vector summarizing the table's title, country, and key
#'   demographic columns.
#'
#' @export
tbl_sum.cd_national_denominators <- function(x, ...) {

  country <- x %>% distinct(country) %>% pull(country)

  c(
    'Table 2a' = 'Total Population and Live Birth by Source',
    'Country' = country,
    NextMethod()
  )
}
