#' Render Plot with Error Handling
#'
#' `render_with_error_handling` is a helper function designed to render plots in a Shiny application.
#' It wraps the plot-rendering code within a `tryCatch` block to gracefully handle errors and display
#' a user-friendly message in case of failure.
#'
#' @param expr Expression. The plotting code to evaluate. This is the main block of code that generates the plot.
#' @param error_message Character. A custom message to display when an error occurs. Default is
#'   "An error occurred while rendering the plot."
#'
#' @details
#' This function ensures that any errors during the rendering of plots are caught and do not crash
#' the Shiny application. Instead of displaying an error stack trace, the function displays a blank
#' plot with the error message and a clean explanation of the issue.
#'
#' The function is intended to be used within Shiny `renderPlot` calls to standardize error handling
#' across multiple plots in the application.
#'
#' @return The evaluated plot or a blank plot with an error message if the evaluation fails.
#'
#' @examples
#' \dontrun{
#' # Example usage in a Shiny app
#' output$measles1 <- renderPlot({
#'   render_with_error_handling({
#'     plot(measles1_coverage()) # Replace with your plotting code
#'   }, error_message = "Failed to render Measles 1 coverage plot.")
#' })
#'
#' output$penta3 <- renderPlot({
#'   render_with_error_handling({
#'     plot(penta3_coverage()) # Replace with your plotting code
#'   })
#' })
#' }
#'
#' @export
render_with_error_handling <- function(expr, error_message = "An error occurred while rendering the plot.") {
  tryCatch(
    {
      expr
    },
    error = function(e) {
      print(e)
      clean_message <- cli::ansi_strip(conditionMessage(e))
      plot.new() # Start a blank plot
      text(
        x = 0.5, y = 0.5,
        labels = paste(error_message, clean_message),
        cex = 1.2, col = "red"
      )
    }
  )
}
