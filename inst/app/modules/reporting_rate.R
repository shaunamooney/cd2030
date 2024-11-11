reportingRateUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    box(
      title = 'Performance Threshold',
      status = 'success',
      width = 12,
      fluidRow(
        column(3, numericInput(ns('threshold'), label = NULL, value = 90)),
        column(2, offset = 7, helpButtonUI(ns('average_reporting')))
      )
    ),
    box(
      title = 'Average RR by year',
      status = 'success',
      width = 12,
      fluidRow(
        column(6, gt_output(ns('average_report'))),
        column(6, gt_output(ns('district_report')))
      )
    ),
    box(
      title = 'Reporting Rate Summary',
      status = 'success',
      width = 12,
      fluidRow(
        column(12, plotOutput(ns('district_report_plot')))
      )
    )
  )
}

reportingRateServer <- function(id, data) {
  stopifnot(is.reactive(data))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      district_rr <- reactive({
        req(data())

        data() %>%
          cd2030::calculate_district_reporting_rate(input$threshold)
      })

      average_rr <- reactive({
        req(data())

        data() %>%
          cd2030::calculate_average_reporting_rate()
      })

      output$average_report <- gt::render_gt({
        average_rr() %>%
          gt() %>%
          tab_header(
            title = 'Table 1a - average reporting rates for all indicators, by year'
          )
      })

      output$district_report <- gt::render_gt({
        district_rr() %>%
          gt() %>%
          tab_header(
            title = paste0('Table 1b - Percentage of districts with reporting rates >= ', input$threshold, ', by year')
          )
      })

      output$district_report_plot <- renderPlot({
        plot(district_rr())
      })

      helpButtonServer('average_reporting', 'Reporting Rate Indicators', 'l', '2_reporting_rate.md')
    }
  )
}
