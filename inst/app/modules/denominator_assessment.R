denominatorAssessmentUI <- function(id) {
  ns <- NS(id)

  fluidRow(

    tabBox(
      title = 'Denominator Assessment',
      id = 'denominator_assessment',
      width = 12,
      tabPanel(
        title = 'Total Population',
        fluidRow(
          column(12, plotOutput(ns('population'))),
          column(4, downloadButtonUI(ns('population_plot'), label = 'Download Plot'))
        )
      ),

      tabPanel(
        title = 'Births',
        fluidRow(
          column(12, plotOutput(ns('births'))),
          column(4, downloadButtonUI(ns('births_plot'), label = 'Download Plot'))
        )
      )
    )
  )
}

denominatorAssessmentServer <- function(id, data, national_values) {
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(national_values))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      un_estimates <- reactive({
        national_values()$data$un
      })

      denominators <- reactive({
        if (!isTruthy(un_estimates())) return(NULL)

        data() %>%
          prepare_population_metrics(un_estimates = un_estimates())
      })

      output$population <- renderPlot({
        req(denominators())
        plot(denominators(), 'population')
      })

      output$births <- renderPlot({
        req(denominators())
        plot(denominators(), 'births')
      })

      downloadButtonServer(
        id = 'population_plot',
        filename = 'population_plot',
        extension = 'png',
        content = function(file) {
          plot(denominators(), 'population')
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        },
        data = denominators
      )

      downloadButtonServer(
        id = 'births_plot',
        filename = 'births_plot',
        extension = 'png',
        content = function(file) {
          plot(denominators(), 'births')
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        },
        data = denominators
      )
    }
  )
}
