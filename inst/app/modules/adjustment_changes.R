adjustmentChangesUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('adjustment_changes'), 'Data Adjustment Changes'),
    contentBody(
      tabBox(
        title = 'Visualize effects of changes',
        id = 'visualize_changes',
        width = 12,

        tabPanel(
          title = 'Live Births',
          fluidRow(
            column(12, plotOutput(ns('live_births'))),
            column(3, downloadButtonUI(ns('live_births_plot'), label = 'Download Plot'))
          )
        ),

        tabPanel(
          title = 'Penta 1',
          fluidRow(
            column(12, plotOutput(ns('penta1'))),
            column(3, downloadButtonUI(ns('penta1_plot'), label = 'Download Plot'))
          )
        ),

        tabPanel(
          title = 'BCG',
          fluidRow(
            column(12, plotOutput(ns('bcg'))),
            column(3, downloadButtonUI(ns('bcg_plot'), label = 'Download Plot'))
          )
        ),

        tabPanel(
          title = 'Measles',
          fluidRow(
            column(12, plotOutput(ns('measles1'))),
            column(3, downloadButtonUI(ns('measles1_plot'), label = 'Download Plot'))
          )
        ),

        tabPanel(
          title = 'Custom Check',
          fluidRow(
            column(3, selectizeInput(ns('indicator'), label = 'Indicator', choices = NULL))
          ),
          fluidRow(
            column(12, plotOutput(ns('custom_check'))),
            column(3, downloadButtonUI(ns('custom_check_plot'), label = 'Download Plot'))
          )
        )
      )
    )
  )
}

adjustmentChangesServer <- function(id, data, k_factors) {
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(k_factors))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      adjustments <- reactive({
        if (!isTruthy(data()) || !isTruthy(k_factors())) return(NULL)

        data() %>%
          generate_adjustment_values(adjustment = 'custom', k_factors = k_factors())
      })

      output$live_births <- renderPlot({
        req(adjustments())
        plot(adjustments(),
             indicator = 'ideliv',
             title = 'Figure 1c: Comparison of number of live births before and after adjustments for completness and outliers')
      })

      output$penta1 <- renderPlot({
        req(adjustments())
        plot(adjustments(),
             indicator = 'penta1',
             title = 'Figure 1d: Comparison of number of penta1 vaccination before and after adjustments for completness and outliers')
      })

      output$bcg <- renderPlot({
        req(adjustments())
        plot(adjustments(),
             indicator = 'bcg',
             title = 'Figure 1e: Comparison of number of BCG vaccination before and after adjustments for completness and outliers')
      })

      output$measles1 <- renderPlot({
        req(adjustments())
        plot(adjustments(),
             indicator = 'measles1',
             title = 'Figure 1f: Comparison of number of measles vaccination before and after adjustments for completness and outliers')
      })

      output$custom_check <- renderPlot({
        req(adjustments())
        req(input$indicator != '0')

        plot(adjustments(),
             indicator = input$indicator)
      })

      downloadButtonServer(
        id = 'live_births_plot',
        filename = 'live_births_plot',
        extension = 'png',
        content = function(file) {
          plot(adjustments(),
               indicator = 'ideliv',
               title = 'Figure 1c: Comparison of number of live births before and after adjustments for completness and outliers')
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        },
        data = adjustments
      )

      downloadButtonServer(
        id = 'penta1_plot',
        filename = 'penta1_plot',
        extension = 'png',
        content = function(file) {
          plot(adjustments(),
               indicator = 'penta1',
               title = 'Figure 1d: Comparison of number of penta1 vaccination before and after adjustments for completness and outliers')
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        },
        data = adjustments
      )

      downloadButtonServer(
        id = 'bcg_plot',
        filename = 'bcg_plot',
        extension = 'png',
        content = function(file) {
          plot(adjustments(),
               indicator = 'bcg',
               title = 'Figure 1e: Comparison of number of BCG vaccination before and after adjustments for completness and outliers')
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        },
        data = adjustments
      )

      downloadButtonServer(
        id = 'measles1_plot',
        filename = 'measles1_plot',
        extension = 'png',
        content = function(file) {
          plot(adjustments(),
               indicator = 'measles1',
               title = 'Figure 1f: Comparison of number of measles vaccination before and after adjustments for completness and outliers')
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        },
        data = adjustments
      )

      downloadButtonServer(
        id = 'custom_check_plot',
        filename = paste0(input$indicator, '_plot'),
        extension = 'png',
        content = function(file) {
          plot(adjustments(),
               indicator = input$indicator)
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        },
        data = adjustments
      )

      contentHeaderServer(
        'adjustment_changes',
        md_title = 'Data Adjustments Changes',
        md_file = '2_reporting_rate.md'
      )
    }
  )
}
