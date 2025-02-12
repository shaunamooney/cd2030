lowReportingUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('low_reporting'), 'Low Reporting'),
    contentBody(
      box(
        title = 'Analysis Options',
        status = 'success',
        width = 12,
        solidHeader = TRUE,
        fluidRow(
          column(3, selectizeInput(ns('admin_level'), label = 'Subnational Level',
                                   choices = c('Admin Level 1' = 'adminlevel_1',
                                               'District' = 'district'))),
          column(3, selectizeInput(ns('denominator'), label = 'Denominator',
                                   choices = c('DHIS2' = 'dhis2',
                                               'ANC 1' = 'anc1',
                                               'Penta 1' = 'penta1')))
        )
      ),

      tabBox(
        title = 'Subnational Inequality',
        width = 12,

        tabPanel(
          title = 'Coverage',
          fluidRow(
            column(12, plotCustomOutput(ns('coverage'))),
            downloadCoverageUI(ns('coverage_download'))
          )
        ),

        tabPanel(
          title = 'Dropout',
          fluidRow(
            column(12, plotCustomOutput(ns('dropout'))),
            downloadCoverageUI(ns('dropout_download'))
          )
        )
      )
    )
  )
}

lowReportingServer <- function(id, cache) {

  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      denominator <- reactive({
        req(cache())
        cache()$denominator
      })

      data <- reactive({
        req(cache())
        cache()$adjusted_data
      })

      coverage_threshold <- reactive({
        req(data(), input$admin_level)
        calculate_threshold(cache()$adjusted_data, input$admin_level, 'coverage')
      })

      dropout_threshold <- reactive({
        req(data(), input$admin_level)
        calculate_threshold(cache()$adjusted_data, input$admin_level, 'dropout')
      })

      observe({
        req(denominator())
        updateSelectInput(session, 'denominator', selected = denominator())
      })

      observeEvent(input$denominator, {
        req(cache(), input$denominator)
        cache()$set_denominator(input$denominator)
      })

      output$coverage <- renderCustomPlot({
        req(coverage_threshold(), denominator())
        plot(coverage_threshold(), denominator = denominator())
      })

      output$dropout <- renderCustomPlot({
        req(dropout_threshold(), denominator())
        plot(dropout_threshold(), denominator = denominator())
      })

      contentHeaderServer(
        'low_reporting',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = 'Regions with vaccination coverage of >= 90%',
        md_file = '2_calculate_ratios.md'
      )

    }
  )
}
