consistencyCheckUI <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('consistency_checks'), i18n$t("title_consistency"), i18n = i18n),
    contentBody(
      tabBox(
        title = i18n$t("title_consistency_checks"),
        width = 12,
        tabPanel(
          i18n$t("opt_anc1_and_penta1"),
          fluidRow(
            column(12, plotCustomOutput(ns('anc1_penta1'))),
            column(4, downloadButtonUI(ns('anc1_penta1_plot')))
          )
        ),
        tabPanel(
          i18n$t("opt_penta1_and_penta3"),
          fluidRow(
            column(12, plotCustomOutput(ns('penta1_penta3'))),
            column(4, downloadButtonUI(ns('penta1_penta3_plot')))
          )
        ),
        tabPanel(
          i18n$t("opt_opv1_and_opv3"),
          fluidRow(
            column(12, plotCustomOutput(ns('opv1_opv3'))),
            column(4, downloadButtonUI(ns('opv1_opv3_plot')))
          )
        ),
        tabPanel(
          i18n$t("opt_custom_check"),
          fluidRow(
            column(3, selectizeInput(ns('x_axis'), label = i18n$t("title_x_axis"), choices = NULL)),
            column(3, offset = 1, selectizeInput(ns('y_axis'), label = i18n$t("title_y_axis"), choices = NULL)),
            column(12, plotCustomOutput(ns('custom_graph'))),
            column(4, downloadButtonUI(ns('custom_graph_plot')))
          )
        )
      )
    )
  )
}

consistencyCheckServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      data <- reactive({
        req(cache())
        cache()$countdown_data
      })

      observe({
        req(cache())

        all_indicators <- list_vaccines()
        all_indicators <- c('Select' = '', all_indicators)

        updateSelectizeInput(session, 'x_axis', choices = all_indicators)
        updateSelectizeInput(session, 'y_axis', choices = all_indicators)
      })

      output$anc1_penta1 <- renderCustomPlot({
        req(data())
        plot_comparison_anc1_penta1(data())
      })

      output$penta1_penta3 <- renderCustomPlot({
        req(data())
        plot_comparison_penta1_penta3(data())
      })

      output$opv1_opv3 <- renderCustomPlot({
        req(data())
        plot_comparison_opv1_opv3(data())
      })

      output$custom_graph <- renderCustomPlot({
        req(data())
        req(input$x_axis, input$y_axis)
        plot_comparison(data(), input$x_axis, input$y_axis)
      })

      downloadPlot(
        id = 'anc1_penta1_plot',
        filename = reactive('anc1_penta1_plot'),
        data = data,
        i18n = i18n,
        plot_function = function() {
          plot_comparison_anc1_penta1(data())
        }
      )

      downloadPlot(
        id = 'penta1_penta3_plot',
        filename = reactive('penta1_penta3_plot'),
        data = data,
        i18n = i18n,
        plot_function = function() {
          plot_comparison_penta1_penta3(data())
        }
      )

      downloadPlot(
        id = 'opv1_opv3_plot',
        filename = reactive('opv1_opv3_plot'),
        data = data,
        i18n = i18n,
        plot_function = function() {
          plot_comparison_opv1_opv3(data())
        }
      )

      downloadPlot(
        id = 'custom_graph_plot',
        filename = reactive(paste0(input$x_axis, '_', input$y_axis, '_plot')),
        data = data,
        i18n = i18n,
        plot_function = function() {
          plot_comparison(data(), input$x_axis, input$y_axis)
        }
      )

      contentHeaderServer(
        'consistency_checks',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = i18n$t("title_consistency"),
        md_file = 'quality_checks_internal_consistency.md',
        i18n = i18n
      )
    }
  )
}
