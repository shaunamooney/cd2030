calculateRatiosUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    box(
      title = 'Set Survey Values',
      status = 'success',
      width = 12,
      fluidRow(
        column(12, plotOutput(ns('ratios')))
      )
    )
  )
}

calculateRatiosServer <- function(id, data, survey_values) {
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(survey_values))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      output$ratios <- renderPlot({
        plot(calculate_ratios_summary(data(), survey_coverage = survey_values()))
      })

    }
  )
}
