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
            column(12, plotCustomOutput(ns('live_births'))),
            column(3, downloadButtonUI(ns('live_births_plot')))
          )
        ),

        tabPanel(
          title = 'Penta 1',
          fluidRow(
            column(12, plotCustomOutput(ns('penta1'))),
            column(3, downloadButtonUI(ns('penta1_plot')))
          )
        ),

        tabPanel(
          title = 'BCG',
          fluidRow(
            column(12, plotCustomOutput(ns('bcg'))),
            column(3, downloadButtonUI(ns('bcg_plot')))
          )
        ),

        tabPanel(
          title = 'Measles',
          fluidRow(
            column(12, plotCustomOutput(ns('measles1'))),
            column(3, downloadButtonUI(ns('measles1_plot')))
          )
        ),

        tabPanel(
          title = 'Custom Check',
          fluidRow(
            column(3, selectizeInput(ns('indicator'), label = 'Indicator', choices = NULL))
          ),
          fluidRow(
            column(12, plotCustomOutput(ns('custom_check'))),
            column(3, downloadButtonUI(ns('custom_check_plot')))
          )
        )
      )
    )
  )
}

adjustmentChangesServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      data <- reactive({
        req(cache())
        cache()$data_with_excluded_years
      })

      k_factors <- reactive({
        req(cache())

        if (cache()$adjusted_flag) {
          cache()$k_factors
        } else {
          c(anc = 0, idelv = 0, vacc = 0)
        }
      })

      adjustments <- reactive({
        req(data())
        data() %>%
          generate_adjustment_values(adjustment = 'custom', k_factors = k_factors())
      })

      vaccines_indicator <- reactive({
        req(cache())
        cache()$vaccine_indicators
      })

      observe({
        req(cache())
        vaccs <- vaccines_indicator()
        updateSelectizeInput(session, 'indicator', choices = c('Select Indicator' = '', vaccs))
      })

      output$live_births <- renderCustomPlot({
        req(adjustments())
        plot(adjustments(),
             indicator = 'ideliv',
             title = 'Figure 1c: Comparison of number of live births before and after adjustments for completness and outliers')
      })

      output$penta1 <- renderCustomPlot({
        req(adjustments())
        plot(adjustments(),
             indicator = 'penta1',
             title = 'Figure 1d: Comparison of number of penta1 vaccination before and after adjustments for completness and outliers')
      })

      output$bcg <- renderCustomPlot({
        req(adjustments())
        plot(adjustments(),
             indicator = 'bcg',
             title = 'Figure 1e: Comparison of number of BCG vaccination before and after adjustments for completness and outliers')
      })

      output$measles1 <- renderCustomPlot({
        req(adjustments())
        plot(adjustments(),
             indicator = 'measles1',
             title = 'Figure 1f: Comparison of number of measles vaccination before and after adjustments for completness and outliers')
      })

      output$custom_check <- renderCustomPlot({
        req(adjustments(), input$indicator != '0')

        plot(adjustments(),
             indicator = input$indicator)
      })

      downloadPlot(
        id = 'live_births_plot',
        filename = 'live_births_plot',
        data = adjustments,
        plot_function = function() {
          plot(adjustments(),
               indicator = 'ideliv',
               title = 'Figure 1c: Comparison of number of live births before and after adjustments for completness and outliers')
        }
      )

      downloadPlot(
        id = 'penta1_plot',
        filename = 'penta1_plot',
        data = adjustments,
        plot_function = function() {
          plot(adjustments(),
               indicator = 'penta1',
               title = 'Figure 1d: Comparison of number of penta1 vaccination before and after adjustments for completness and outliers')
        }
      )

      downloadPlot(
        id = 'bcg_plot',
        filename = 'bcg_plot',
        data = adjustments,
        plot_function = function() {
          plot(adjustments(),
               indicator = 'bcg',
               title = 'Figure 1e: Comparison of number of BCG vaccination before and after adjustments for completness and outliers')
        }
      )

      downloadPlot(
        id = 'measles1_plot',
        filename = 'measles1_plot',
        data = adjustments,
        plot_function = function() {
          plot(adjustments(),
               indicator = 'measles1',
               title = 'Figure 1f: Comparison of number of measles vaccination before and after adjustments for completness and outliers')
        }
      )

      downloadPlot(
        id = 'custom_check_plot',
        filename = paste0(input$indicator, '_plot'),
        data = adjustments,
        plot_function = function() {
          plot(adjustments(),
               indicator = input$indicator)
        }
      )

      contentHeaderServer(
        'adjustment_changes',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = 'Data Adjustments Changes',
        md_file = '2_reporting_rate.md'
      )
    }
  )
}
