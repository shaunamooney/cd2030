consistencyCheckUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(6, h4('INTERNAL CONSISTENCY CHECKS')),
    column(3, offset = 3, helpButtonUI(ns('internal_consistency_help')), style = 'margin-bottom:10px;'),
    tabBox(
      title = 'Consistency Checks',
      width = 12,
      tabPanel(
        'ANC1 and Penta1',
        fluidRow(
          column(12, plotOutput(ns('anc1_penta1'))),
          column(3, downloadButton(ns('anc1_penta1_plot'), label = 'Download Plot', style = 'color:#2196F3;width:100%;margin-top:10px;'))
        )
      ),
      tabPanel(
        'Penta1 and Penta3',
        fluidRow(
          column(12, plotOutput(ns('penta1_penta3'))),
          column(3, downloadButton(ns('penta1_penta3_plot'), label = 'Download Plot', style = 'color:#2196F3;width:100%;margin-top:10px;'))
        )
      ),
      tabPanel(
        'OPV1 and OPV3',
        fluidRow(
          column(12, plotOutput(ns('opv1_opv3'))),
          column(3, downloadButton(ns('opv1_opv3_plot'), label = 'Download Plot', style = 'color:#2196F3;width:100%;margin-top:10px;'))
        )
      ),
      tabPanel(
        'Custom Check',
        fluidRow(
          column(3, selectizeInput(ns('x_axis'), label = 'X axis', choices = NULL)),
          column(3, offset = 1, selectizeInput(ns('y_axis'), label = 'Y axis', choices = NULL)),
          column(12, plotOutput(ns('custom_graph'))),
          column(3, downloadButton(ns('custom_graph_plot'), label = 'Download Plot', style = 'color:#2196F3;width:100%;margin-top:10px;'))
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
        req(data())
        plot_comparison_anc1_penta1(data())
      })

      output$penta1_penta3 <- renderPlot({
        req(data())
        plot_comparison_penta1_penta3(data())
      })

      output$opv1_opv3 <- renderPlot({
        req(data())
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
        req(data())
        req(input$x_axis != '0', input$y_axis != '0')
        plot_comparison(data(), input$x_axis, input$y_axis)
      })

      output$anc1_penta1_plot <- downloadHandler(
        filename = function() { paste0("anc1_penta1_plot_", Sys.Date(), ".png") },
        content = function(file) {
          req(data())
          plot_comparison_anc1_penta1(data())
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        }
      )

      output$penta1_penta3_plot <- downloadHandler(
        filename = function() { paste0("penta1_penta3_plot_", Sys.Date(), ".png") },
        content = function(file) {
          req(data())
          plot_comparison_penta1_penta3(data())
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        }
      )

      output$opv1_opv3_plot <- downloadHandler(
        filename = function() { paste0("opv1_opv3_plot_", Sys.Date(), ".png") },
        content = function(file) {
          req(data())
          plot_comparison_opv1_opv3(data())
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        }
      )

      output$custom_graph_plot <- downloadHandler(
        filename = function() { paste0("custom_graph_plot_", Sys.Date(), ".png") },
        content = function(file) {
          req(data())
          plot_comparison(data(), input$x_axis, input$y_axis)
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        }
      )

      helpButtonServer('internal_consistency_help', 'Internal Consistency', 'l', '2_internal_consistency.md')
    }
  )
}
