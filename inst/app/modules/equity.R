equityUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('equity_assessment'), 'Equity Assessment'),
    contentBody(
      box(
        title = 'Analysis Options',
        status = 'success',
        width = 12,
        solidHeader = TRUE,
        fluidRow(
          column(3, selectizeInput(ns('type'), label = 'Equity Type',
                                   choices = c('Area' = 'area',
                                               'Maternal Education' = 'meduc',
                                               'Wealth Quintile' = 'wiq')))
        )
      ),

      tabBox(
        title = 'Equity Analysis',
        width = 12,

        tabPanel(
          title = 'Penta 3',
          fluidRow(
            column(12, plotCustomOutput(ns('penta3'))),
            column(3, downloadButtonUI(ns('penta3_download')))
          )
        ),

        tabPanel(
          title = 'Measles 1',
          fluidRow(
            column(12, plotCustomOutput(ns('measles1'))),
            column(3, downloadButtonUI(ns('measles1_download')))
          )
        ),

        tabPanel(
          'Custom Check',
          fluidRow(
            column(3, selectizeInput(ns('indicator'), label = 'Indicator', choices = NULL))
          ),
          fluidRow(
            column(12, plotCustomOutput(ns('custom_check'))),
            column(3, downloadButtonUI(ns('custom_download')))
          )
        )

      )
    )
  )
}

equityServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      wiq <- reactive({
        req(cache())
        cache()$get_wiq_survey()
      })

      meduc <- reactive({
        req(cache())
        cache()$get_education_survey()
      })

      area <- reactive({
        req(cache())
        cache()$get_area_survey()
      })

      observe({

        indicators <-  c("bcg", "anc1", "pcv3", "opv1", "opv2", "opv3", "penta2", "pcv1", "pcv2",
                         "penta1", "penta3", "measles1", "rota1", "rota2", "instdeliveries", "measles2",
                         "ipv1", "ipv2", "undervax", "dropout_penta13", "zerodose", "dropout_measles12",
                         "dropout_penta3mcv1")

        names(indicators) <- indicators
        indicators <- c('Select' = '', indicators)

        updateSelectInput(session, 'indicator', choices = indicators)
      })

      penta3_equiplot <- reactive({
        req(wiq(), area(), meduc(), input$type)

        switch(input$type,
               'area' = equiplot_area(area(), 'penta3'),
               'meduc' = equiplot_education(meduc(), 'penta3'),
               'wiq' = equiplot_wealth(wiq(), 'penta3'))
      })

      measles1_equiplot <- reactive({
        req(wiq(), area(), meduc(), input$type)

        switch(input$type,
               'area' = equiplot_area(area(), 'measles1'),
               'meduc' = equiplot_education(meduc(), 'measles1'),
               'wiq' = equiplot_wealth(wiq(), 'measles1'))
      })

      custom_equiplot <- reactive({
        req(wiq(), area(), meduc(), input$type, input$indicator)

        switch(input$type,
               'area' = equiplot_area(area(), input$indicator),
               'meduc' = equiplot_education(meduc(), input$indicator),
               'wiq' = equiplot_wealth(wiq(), input$indicator))
      })

      output$penta3 <- renderCustomPlot({
        req(penta3_equiplot())
        penta3_equiplot()
      })

      output$measles1 <- renderCustomPlot({
        req(measles1_equiplot())
        measles1_equiplot()
      })

      output$custom_check <- renderCustomPlot({
        req(custom_equiplot())
        custom_equiplot()
      })

      downloadPlot(
        id = 'penta3_download',
        filename = paste0('penta3_', input$type, '_equity'),
        data = penta3_equiplot,
        plot_function = function() {
          plot(penta3_equiplot())
        }
      )

      downloadPlot(
        id = 'measles1_download',
        filename = paste0('measles1_', input$type, '_equity'),
        data = measles1_equiplot,
        plot_function = function() {
          plot(measles1_equiplot())
        }
      )

      downloadPlot(
        id = 'custom_download',
        filename = paste0(input$indicator, '_', input$type, '_equity'),
        data = custom_equiplot,
        plot_function = function() {
          plot(custom_equiplot())
        }
      )

      contentHeaderServer(
        'equity_assessment',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = 'Equity Assessment',
        md_file = '2_reporting_rate.md'
      )
    }
  )
}
