calculateRatiosUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    box(
      title = 'Setup Survey Values',
      status = 'success',
      width = 12,
      fluidRow(
        column(3, offset = 1, sliderInput(ns('anc1_coverage'),
                                          'ANC1 Coverage (%)',
                                          min = 0, max = 100, value = 98, step = 1)),
        column(3, offset = 0, sliderInput(ns('penta1_coverage'),
                                          'Penta1 Coverage (%)',
                                          min = 0, max = 100, value = 97, step = 1)),
        column(3, offset = 0, sliderInput(ns('penta3_coverage'),
                                          'Penta3 Coverage (%)',
                                          min = 0, max = 100, value = 89, step = 1))
      ),
      fluidRow(

        column(3, offset = 1, sliderInput(ns('opv1_coverage'),
                                          'OPV1 Coverage (%)',
                                          min = 0, max = 100, value = 97, step = 1)),
        column(3, offset = 0, sliderInput(ns('opv3_coverage'),
                                          'OPV3 Coverage (%)',
                                          min = 0, max = 100, value = 78, step = 1)),
        column(3, offset = 0, sliderInput(ns('pcv1_coverage'),
                                          'PCV1 Coverage (%)',
                                          min = 0, max = 100, value = 97, step = 1))
      ),
      fluidRow(
        column(3, offset = 1, sliderInput(ns('rota1_coverage'),
                                          'Rota1 Coverage (%)',
                                          min = 0, max = 100, value = 96, step = 1))
      )
    ),
    box(
      title = 'Set Survey Values',
      status = 'success',
      width = 12,
      fluidRow(
        column(2, offset = 10, helpButtonUI(ns('survey_values')), style = 'margin-bottom: 10px;'),
        column(12, plotOutput(ns('ratios'))),
        column(3, downloadButton(ns('ratio_plot_download'), label = 'Download Plot', style = 'color:#2196F3;width:100%;margin-top:10px;'))
      )
    )
  )
}

calculateRatiosServer <- function(id, data) {
  stopifnot(is.reactive(data))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      survey_coverage <- reactive({
        c(anc1 = input$anc1_coverage,
          penta1 = input$penta1_coverage,
          penta3 = input$penta3_coverage,
          opv1 = input$opv1_coverage,
          opv3 = input$opv3_coverage,
          pcv1 = input$pcv1_coverage,
          rota1 = input$rota1_coverage)
      })

      output$ratios <- renderPlot({
        req(data())
        plot(calculate_ratios_summary(data(), survey_coverage = survey_coverage()))
      })

      output$ratio_plot_download <- downloadHandler(
        filename = function() { paste0("ratio_plot_", Sys.Date(), ".png") },
        content = function(file) {
          req(data())
          plot(calculate_ratios_summary(data(), survey_coverage = survey_coverage()))
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        }
      )

      helpButtonServer('survey_values', 'Ratios for Internal Consistency', 'l', '2_calculate_ratios.md')

      return(reactive({
        list(anc1 = input$anc1_coverage,
          penta1 = input$penta1_coverage)
      }))
    }
  )
}
