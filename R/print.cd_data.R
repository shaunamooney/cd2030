#' Print Method for `cd_data` Class
#'
#' This function provides a custom print method for objects of class `cd_data`,
#' displaying a summary of the dataset, including data dimensions, available years,
#' regions, variable groups, and any quality metrics associated with the data.
#' It uses the `cli` package for enhanced, styled output.
#'
#' @param x An object of class `cd_data`, which contains `merged_data` (a dataframe
#' with demographic data) and optional `quality_metrics` (a dataframe or list of quality metrics).
#' @param ... Additional arguments (currently not used).
#'
#' @return Invisibly returns `x`. This function is called for its side effect of printing
#' a formatted summary of the `cd_data` object.
#'
#' @details This print method provides the following summary sections:
#'   1. **Data Summary**: Shows the number of observations and variables in `merged_data`.
#'   2. **Years Available**: Lists unique years present in `merged_data`.
#'   3. **Regions Available**: Lists unique regions in the `adminlevel_1` column.
#'   4. **Variable Groups**: Displays predefined groups of variables, including ANC,
#'      delivery, vaccination, PNC, OPD, and IPD groups.
#'   5. **Quality Metrics**: If present, prints `quality_metrics` for additional data insights.
#'
#' @section Variable Groups:
#' - `ancvargroup`: Variables related to antenatal care.
#' - `idelvvargroup`: Variables related to delivery.
#' - `vaccvargroup`: Variables related to vaccinations.
#' - `pncvargroup`: Variables related to postnatal care.
#' - `opdvargroup`: Variables related to outpatient care.
#' - `ipdvargroup`: Variables related to inpatient care.
#'
#' @examples
#' \dontrun{
#'   # Assuming `cd_data_obj` is an object of class `cd_data`:
#'   print(cd_data_obj)
#' }
#'
#' @export
print.cd_data <- function(x, ...) {

  year = adminlevel_1 = NULL

  # Extract merged data
  data <- x$merged_data

  # Summarize variable groups
  variable_groups <- list(
    "ANC Variables" = c('anc1', 'anc4', 'ipt2'),
    "Delivery Variables" = c('ideliv', 'instlivebirths', 'csection', 'total_stillbirth', 'stillbirth_f', 'stillbirth_m', 'maternal_deaths'),
    "Vaccination Variables" = c('opv1', 'opv2', 'opv3', 'penta1', 'penta2', 'penta3', 'measles1', 'pcv1', 'pcv2', 'pcv3', 'measles2', 'bcg', 'rota1', 'rota2', 'ipv1', 'ipv2'),
    "PNC Variables" = 'pnc48h',
    "OPD Variables" = c('opd_total', 'opd_under5'),
    "IPD Variables" = c('ipd_total', 'ipd_under5')
  )

  # Display header with styled border
  cli::cli_rule(center = "Countdown Object Summary")

  years <- data %>% distinct(year) %>% pull(year)
  regions <- data %>% distinct(adminlevel_1) %>% pull(adminlevel_1)

  # Basic data details
  cli::cli_h2("{.emph Data Summary}")
  cli::cli_ul()
  cli::cli_li("{.field Number of observations}: {nrow(data)}")
  cli::cli_li("{.field Number of variables}: {ncol(data)}")
  cli::cli_li("{.field Number of years}: {length(years)}")
  cli::cli_li("{.field Number of regions}: {length(regions)}")
  cli::cli_end()

  # Years available
  cli::cli_h2("{.emph Years Available}")
  cli::cli_bullets(setNames(as.list(years), rep("*", length(years))))

  # Regions available
  cli::cli_h2("{.emph Regions Available}")
  cli::cli_bullets(setNames(as.list(regions), rep("*", length(regions))))

  # Variable groups section
  cli::cli_h2("{.emph Variable Groups Available}")
  variable_bullets <- imap(variable_groups, ~ {
    # Check if any variables in the group are present in the data
    available_vars <- .x[.x %in% names(data)]
    if (length(available_vars) > 0) {
      paste0("* {.field ", .y, "}: ", paste(available_vars, collapse = ", "))
    } else {
      NULL
    }
  }) %>% compact()

  cli::cli_bullets(variable_bullets)

  # Closing line
  cli::cli_rule()
}
