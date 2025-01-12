#' Launch the Dashboard Application
#'
#' This function launches a Shiny dashboard application that leverages the
#' analytical functions and datasets within this package. The dashboard
#' provides a user-friendly interface for exploring, visualizing, and analyzing
#' health and facility data.
#'
#' @return This function does not return a value; it launches a Shiny application
#'   in the default web browser.
#'
#' @details
#' The `dashboard` function serves as an entry point to the Shiny application
#' included in this package. This application is located in the `inst/shiny/app.R`
#' directory of the package. It uses the package's internal functions and data to
#' facilitate interactive data analysis, visualization, and reporting.
#'
#' To use this function, ensure that all dependencies for running Shiny applications
#' are installed. The Shiny app opens in the default web browser and provides
#' tools for users to conduct analyses without directly interacting with the
#' underlying code.
#'
#' @examples
#' \dontrun{
#'   # Launch the Shiny dashboard
#'   dashboard()
#' }
#'
#' @export
dashboard <- function() {

  ui = server = NULL

  shiny_dir <- system.file('app', package = 'cd2030')
  if (shiny_dir == '') {
    cd_abort(
      c('x' = 'Could not find app directory. Try re-installing {.pkg cd2030}.')
    )
  }

  # evaluate them inside function environment, also change working directory temporarily
  source(file.path(shiny_dir, "app.R"), local = TRUE, chdir = TRUE)

  shiny::runApp(shiny::shinyAppDir(shiny_dir), launch.browser = TRUE, display.mode = "normal")
}
