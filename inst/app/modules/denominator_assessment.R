denominatorAssessmentUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('denominator_assessment'), 'Denominator Assessment'),
    contentBody(
      tabBox(
        title = 'Denominator Assessment',
        width = 12,
        tabPanel(
          title = 'Total Population',
          fluidRow(
            column(12, plotCustomOutput(ns('population'))),
            column(4, downloadButtonUI(ns('population_plot')))
          )
        ),

        tabPanel(
          title = 'Births',
          fluidRow(
            column(12, plotCustomOutput(ns('births'))),
            column(4, downloadButtonUI(ns('births_plot')))
          )
        )
      )
    )
  )
}

denominatorAssessmentServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      data <- reactive({
        req(cache())
        cache()$get_adjusted_data()
      })

      un_estimates <- reactive({
        req(cache())
        cache()$get_un_estimates()
      })

      denominators <- reactive({
        req(cache(), un_estimates())

        data() %>%
          prepare_population_metrics(un_estimates = un_estimates())
      })

      output$population <- renderCustomPlot({
        req(denominators())
        plot(denominators(), 'population')
      })

      output$births <- renderCustomPlot({
        req(denominators())
        plot(denominators(), 'births')
      })

      downloadPlot(
        id = 'population_plot',
        filename = 'population_plot',
        data = denominators,
        plot_function = function() {
          plot(denominators(), 'population')
        }
      )

      downloadPlot(
        id = 'births_plot',
        filename = 'births_plot',
        data = denominators,
        plot_function = function() {
          plot(denominators(), 'births')
        }
      )

      contentHeaderServer(
        'denominator_assessment',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = 'Denominator Assessment',
        md_file = '2_reporting_rate.md'
      )
    }
  )
}
