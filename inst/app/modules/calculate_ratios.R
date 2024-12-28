calculateRatiosUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    box(
      title = 'Ratio Options',
      status = 'success',
      width = 12,
      fluidRow(
        column(3, offset = 1, numericInput(ns('anc1_coverage'),
                                          'ANC1 Coverage (%)',
                                          min = 0, max = 100, value = 98, step = 1)),
        column(3, offset = 0, numericInput(ns('penta1_coverage'),
                                          'Penta1 Coverage (%)',
                                          min = 0, max = 100, value = 97, step = 1)),
        column(3, offset = 0, numericInput(ns('penta3_coverage'),
                                          'Penta3 Coverage (%)',
                                          min = 0, max = 100, value = 89, step = 1))
      )
    ),
    box(
      title = 'Ratio Plots',
      status = 'success',
      width = 12,
      fluidRow(
        column(2, offset = 10, helpButtonUI(ns('ratios')), style = 'margin-bottom: 10px;'),
        column(12, plotOutput(ns('ratios_plot'))),
        column(4, downloadButtonUI(ns('ratio_plot_download'), label = 'Download Plot'))
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

      output$ratios_plot <- renderPlot({
        req(data())
        plot(calculate_ratios_summary(data(), survey_coverage = survey_coverage()))
      })

      downloadButtonServer(
        id = 'ratio_plot_download',
        filename = 'ratio_plot',
        extension = 'png',
        content = function(file) {
          plot(calculate_ratios_summary(data(), survey_coverage = survey_coverage()))
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        },
        data = data
      )

      helpButtonServer(
        id = 'ratios',
        title = 'Ratios for Internal Consistency',
        size = 'l',
        md_file = '2_calculate_ratios.md')

      return(reactive({
        list(anc1 = input$anc1_coverage,
          penta1 = input$penta1_coverage)
      }))
    }
  )
}
