#' Get Mapping Data for Subnational Analysis
#'
#' `get_mapping_data` merges input data with a country's shapefile and computes
#' indicator coverage at the specified subnational level.
#'
#' @param .data A data frame containing the input data for mapping.
#' @param un_estimates A data frame containing UN estimates for coverage or demographic indicators.
#' @param rates A named list of rates required for calculations, including:
#'   - `sbr`: Stillbirth rate
#'   - `nmr`: Neonatal mortality rate
#'   - `pnmr`: Post-neonatal mortality rate
#'   - `anc1`: ANC1 survey coverage rate
#'   - `penta1`: Penta1 survey coverage rate
#'   - `twin_rate`: Twin birth rate
#'   - `preg_loss`: Pregnancy loss rate
#' @param level A character string specifying the level of analysis. Options are:
#'   - `"adminlevel_1"`: First administrative level.
#'   - `"district"`: District level (default for Kenya).
#'
#' @return A data frame merged with the corresponding shapefile for subnational mapping.
#'
#' @examples
#' rates <- list(sbr = 0.02, nmr = 0.03, pnmr = 0.02, anc1 = 0.8, penta1 = 0.75, twin_rate = 0.015, preg_loss = 0.03)
#' get_mapping_data(data, un_estimates, rates, level = "adminlevel_1")
#'
#' @export
get_mapping_data <- function(.data, un_estimates, rates, subnational_map = NULL, admin_level = c('adminlevel_1', 'district')) {

  check_cd_data(.data)
  check_un_estimates_data(un_estimates)
  check_required(rates)

  iso <- attr(.data, 'iso3')

  shapefile <- get_country_shapefile(iso, 'admin_level_1')

  shapefile <- if (is.null(subnational_map)) {
    shapefile %>%
      mutate(adminlevel_1 = NAME_1)
  } else {
    shapefile %>%
      left_join(subnational_map, by = 'NAME_1')
  }

  merged_data <- if (iso == 'KEN') {
    .data %>%
      calculate_indicator_coverage('district', un_estimates,
                                   sbr = rates$sbr, nmr = rates$nmr,
                                   pnmr = rates$pnmr, anc1survey = rates$anc1,
                                   dpt1survey = rates$penta1, twin = rates$twin_rate,
                                   preg_loss = rates$preg_loss) %>%
      select(-adminlevel_1) %>%
      rename(adminlevel_1 = district)
  } else {
    .data %>%
      calculate_indicator_coverage('adminlevel_1', un_estimates,
                                   sbr = rates$sbr, nmr = rates$nmr,
                                   pnmr = rates$pnmr, anc1survey = rates$anc1,
                                   dpt1survey = rates$penta1, twin = rates$twin_rate,
                                   preg_loss = rates$preg_loss)
  }

  merged_data <- merged_data %>%
    left_join(shapefile, by = 'adminlevel_1')

  new_tibble(
    merged_data,
    class = 'cd_mapping'
  )
}

#' Retrieve Country Shapefile
#'
#' `get_country_shapefile` retrieves the shapefile for a specified country and
#' administrative level.
#'
#' @param country_iso A string specifying the ISO3 code of the country (e.g., "KEN" for Kenya).
#' @param level A string specifying the level of analysis. Options are:
#'   - `"adminlevel_1"`: First administrative level.
#'   - `"district"`: District level.
#'
#' @return A `sf` object containing the shapefile for the specified country and level.
#'
#' @examples
#' shapefile <- get_country_shapefile("KEN", level = "admin_level_1")
#'
#' @export
get_country_shapefile <- function(country_iso, level = c('admin_level_1', 'district')) {

  check_required(country_iso)
  level <- arg_match(level)

  shapefile_path <- system.file(file.path('shapefiles', country_iso, paste0(country_iso, '_', level, '.shp')), package = 'cd2030')
  if (!file.exists(shapefile_path)) {
    cd_abort(
      c('x' = 'Shapefile not found for {.arg {country_iso}} and admin level {.arg {level}}.')
    )
  }

  st_read(shapefile_path, quiet = TRUE)
}
