consistencyCheckUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    tabBox(
      title = 'Consistency Checks',
      id = 'consistency_checks',
      width = 12,
      tabPanel(
        'ANC1 and Penta1',
        fluidRow(
          column(12, plotOutput(ns('anc1_penta1')))
        )
      ),
      tabPanel(
        'Penta1 and Penta3',
        fluidRow(
          column(12, plotOutput(ns('penta1_penta3')))
        )
      ),
      tabPanel(
        'OPV1 and OPV3',
        fluidRow(
          column(12, plotOutput(ns('opv1_opv3')))
        )
      )
    ),

    box(
      title = 'Custom Check',
      status = 'success',
      width = 12,
      fluidRow(
        column(3, selectizeInput(ns('x_axis'), label = 'X axis', choices = NULL)),
        column(3, offset = 1, selectizeInput(ns('y_axis'), label = 'Y axis', choices = NULL)),
        fluidRow(
          plotOutput(ns('custom_graph'))
        )
      )
    )
  )
}

consistencyCheckServer <- function(id, data) {
  stopifnot(is.reactive(data))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      output$anc1_penta1 <- renderPlot({
        plot_comparison_anc1_penta1(data())
      })

      output$penta1_penta3 <- renderPlot({
        plot_comparison_penta1_penta3(data())
      })

      output$opv1_opv3 <- renderPlot({
        plot_comparison_opv1_opv3(data())
      })

      observe({
        req(data())

        indicator_groups <- attr(data(), 'indicator_groups')
        all_indicators <- purrr::list_c(indicator_groups)
        names(all_indicators) <- all_indicators
        all_indicators <- c('Select' = '0', all_indicators)

        updateSelectizeInput(session, 'x_axis', choices = all_indicators)
        updateSelectizeInput(session, 'y_axis', choices = all_indicators)
      })

      output$custom_graph <- renderPlot({

        req(input$x_axis != '0', input$y_axis != '0')
        plot_comparison(data(), input$x_axis, input$y_axis)
      })
    }
  )
}
