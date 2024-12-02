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
          column(3, downloadButton(ns('population_plot'), label = 'Download Plot', style = 'color:#2196F3;width:100%;margin-top:10px;'))
        )
      ),

      tabPanel(
        title = 'Births',
        fluidRow(
          column(12, plotOutput(ns('births'))),
          column(3, downloadButton(ns('births_plot'), label = 'Download Plot', style = 'color:#2196F3;width:100%;margin-top:10px;'))
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
        req(un_estimates())

        data() %>%
          prepare_population_metrics(un_estimates = un_estimates())
      })

      output$population <- renderPlot({
        plot(denominators(), 'population')
      })

      output$births <- renderPlot({
        plot(denominators(), 'births')
      })

      output$population_plot <- downloadHandler(
        filename = function() { paste0("population_plot_", Sys.Date(), ".png") },
        content = function(file) {
          plot(denominators(), 'population')
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        }
      )

      output$births_plot <- downloadHandler(
        filename = function() { paste0("births_plot_", Sys.Date(), ".png") },
        content = function(file) {
          plot(denominators(), 'births')
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        }
      )
    }
  )
}
