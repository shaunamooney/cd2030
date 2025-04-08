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

  # Summarize variable groups
  variable_groups <- get_indicator_groups()

  # Display header with styled border
  cli::cli_rule(center = "Countdown Object Summary")

  years <- if ("year" %in% colnames(x)) {
    x %>% distinct(year) %>% pull(year)
  } else {
    NULL  # Or another default if desired
  }

  regions <- if ("adminlevel_1" %in% colnames(x)) {
    x %>% distinct(adminlevel_1) %>% pull(adminlevel_1)
  } else {
    NULL  # Or another default if desired
  }

  # Basic data details
  cli::cli_h2("{.emph Data Summary}")
  cli::cli_ul()
  cli::cli_li("{.field Number of observations}: {nrow(x)}")
  cli::cli_li("{.field Number of variables}: {ncol(x)}")

  if (!is.null(years)) cli::cli_li("{.field Number of years}: {length(years)}")
  if (!is.null(regions)) cli::cli_li("{.field Number of regions}: {length(regions)}")
  cli::cli_end()

  if (!is.null(years))
  {
    # Years available
    cli::cli_h2("{.emph Years Available}")
    cli::cli_bullets(set_names(as.list(years), rep("*", length(years))))
  }

  if (!is.null(regions))
  {
    # Regions available
    cli::cli_h2("{.emph Regions Available}")
    cli::cli_bullets(set_names(as.list(regions), rep("*", length(regions))))
  }

  variable_bullets <- imap(variable_groups, ~ {
    # Check if any variables in the group are present in the data
    available_vars <- .x[.x %in% names(x)]
    if (length(available_vars) > 0) {
      paste0("* {.field ", .y, "}: ", paste(available_vars, collapse = ", "))
    } else {
      NULL
    }
  }) %>% compact()

  if (length(variable_bullets) > 0) {
    # Variable groups section
    cli::cli_h2("{.emph Variable Groups Available}")
    cli::cli_bullets(variable_bullets)
  }

  # Closing line
  cli::cli_rule()

  NextMethod()
}
