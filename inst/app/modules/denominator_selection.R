denominatorSelectionUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('denominator_selection'), 'Denominator Selection'),
    contentBody(
      tabBox(
        title = 'Denominator Selection',
        width = 12,
        tabPanel(
          title = 'Penta 3',
          fluidRow(
            column(12, plotOutput(ns('penta3'))),
            column(3, downloadButtonUI(ns('penta3_plot'), label = 'Download Plot'))
          )
        ),

        tabPanel(
          title = 'Measles 1',
          fluidRow(
            column(12, plotOutput(ns('measles1'))),
            column(3, downloadButtonUI(ns('measles1_plot'), label = 'Download Plot'))
          )
        ),

        tabPanel(
          title = 'BCG',
          fluidRow(
            column(12, plotOutput(ns('bcg'))),
            column(3, downloadButtonUI(ns('bcg_plot'), label = 'Download Plot'))
          )
        ),

        # tabPanel(
        #   title = 'Custom Checks',
        #   fluidRow(
        #     column(3, selectizeInput(ns('indicator'), label = 'Indicator', choice = NULL)),
        #     column(12, plotOutput(ns('custom_plot'))),
        #     column(3, downloadButtonUI(ns('custom_download'), label = 'Download Plot'))
        #   )
        # )
      )
    )
  )
}

denominatorSelectionServer <- function(id, data, national_values) {
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(national_values))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      national_rates <- reactive({
        national_values()$rates
      })

      un_estimates <- reactive({
        national_values()$data$un
      })

      indicator_coverage <- reactive({
        if (!isTruthy(national_rates()) || !isTruthy(un_estimates())) return(NULL)

        rates <- national_rates()

        data() %>%
          calculate_indicator_coverage(un_estimates = un_estimates(),
                                       sbr = rates$sbr,
                                       nmr = rates$nmr,
                                       pnmr = rates$pnmr,
                                       twin = rates$twin_rate,
                                       preg_loss = rates$preg_loss,
                                       anc1survey = rates$anc1,
                                       dpt1survey = rates$penta1)
      })

      observe({
        req(data())

        indicator_groups <- attr(data(), 'indicator_groups')
        inds <- indicator_groups$vacc

        updateSelectizeInput(session, 'indicator', choices = inds)
      })

      output$penta3 <- renderPlot({
        req(indicator_coverage())
        plot_absolute_differences(indicator_coverage(), 'penta3')
      })

      output$measles1 <- renderPlot({
        req(indicator_coverage())
        plot_absolute_differences(indicator_coverage(), 'measles1')
      })

      output$bcg <- renderPlot({
        req(indicator_coverage())
        plot_absolute_differences(indicator_coverage(), 'bcg')
      })

      output$custom_plot <- renderPlot({
        req(indicator_coverage())
        plot_absolute_differences(indicator_coverage(), input$indicator)
      })

      downloadButtonServer(
        id = 'penta3_plot',
        filename = 'penta3_plot',
        extension = 'png',
        content = function(file) {
          plot_absolute_differences(indicator_coverage(), 'penta3')
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        },
        data = indicator_coverage
      )

      downloadButtonServer(
        id = 'measles1_plot',
        filename = 'measles1_plot',
        extension = 'png',
        content = function(file) {
          plot_absolute_differences(indicator_coverage(), 'measles1')
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        },
        data = indicator_coverage
      )

      downloadButtonServer(
        id = 'bcg_plot',
        filename = 'bcg_plot',
        extension = 'png',
        content = function(file) {
          plot_absolute_differences(indicator_coverage(), 'bcg')
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        },
        data = indicator_coverage
      )

      contentHeaderServer(
        'denominator_selection',
        md_title = 'Denominator Selection',
        md_file = '2_reporting_rate.md'
      )

      return(indicator_coverage)
    }
  )
}
