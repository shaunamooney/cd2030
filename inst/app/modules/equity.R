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
            column(12, plotOutput(ns('penta3'))),
            column(3, downloadButtonUI(ns('penta3_download'), label = 'Download Plot'))
          )
        ),

        tabPanel(
          title = 'Measles 1',
          fluidRow(
            column(12, plotOutput(ns('measles1'))),
            column(3, downloadButtonUI(ns('measles1_download'), label = 'Download Plot'))
          )
        ),

        tabPanel(
          'Custom Check',
          fluidRow(
            column(3, selectizeInput(ns('indicator'), label = 'Indicator', choices = NULL))
          ),
          fluidRow(
            column(12, plotOutput(ns('custom_check'))),
            column(3, downloadButtonUI(ns('custom_download'), label = 'Download Plot'))
          )
        )

      )
    )
  )
}

equityServer <- function(id, data_values) {
  stopifnot(is.reactive(data_values))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      national_data <- reactive({
        data_values()$data
      })

      observe({

        indicators <-  c("bcg", "anc1", "pcv3", "opv1", "opv2", "opv3", "penta2", "pcv1", "pcv2",
                         "penta1", "penta3", "measles1", "rota1", "rota2", "instdeliveries", "measles2",
                         "ipv1", "ipv2", "undervax", "dropout_penta13", "zerodose", "dropout_measles12",
                         "dropout_penta3mcv1")

        names(indicators) <- indicators
        indicators <- c('Select' = '0', indicators)

        updateSelectInput(session, 'indicator', choices = indicators)
      })

      penta3_equiplot <- reactive({
        if (!isTruthy(national_data()$wiq) || !isTruthy(national_data()$area) ||
            !isTruthy(national_data()$meduc) || !isTruthy(input$type)) {
          return(NULL)
        }

        switch(input$type,
               'area' = equiplot_area(national_data()$area, 'penta3'),
               'meduc' = equiplot_education(national_data()$meduc, 'penta3'),
               'wiq' = equiplot_wealth(national_data()$wiq, 'penta3'))
      })

      measles1_equiplot <- reactive({
        if (!isTruthy(national_data()$wiq) || !isTruthy(national_data()$area) ||
            !isTruthy(national_data()$meduc) || !isTruthy(input$type)) {
          return(NULL)
        }

        switch(input$type,
               'area' = equiplot_area(national_data()$area, 'measles1'),
               'meduc' = equiplot_education(national_data()$meduc, 'measles1'),
               'wiq' = equiplot_wealth(national_data()$wiq, 'measles1'))
      })

      custom_equiplot <- reactive({
        if (!isTruthy(national_data()$wiq) || !isTruthy(national_data()$area) ||
            !isTruthy(national_data()$meduc) || !isTruthy(input$type) || !isTruthy(input$indicator != '0')) {
          return(NULL)
        }

        switch(input$type,
               'area' = equiplot_area(national_data()$area, input$indicator),
               'meduc' = equiplot_education(national_data()$meduc, input$indicator),
               'wiq' = equiplot_wealth(national_data()$wiq, input$indicator))
      })

      output$penta3 <- renderPlot({
        req(penta3_equiplot())

        render_with_error_handling({
          penta3_equiplot()
        })
      })

      output$measles1 <- renderPlot({
        req(measles1_equiplot())

        render_with_error_handling({
          measles1_equiplot()
        })
      })

      output$custom_check <- renderPlot({
        req(custom_equiplot())

        render_with_error_handling({
          custom_equiplot()
        })
      })

      downloadButtonServer(
        id = 'penta3_download',
        filename = paste0('penta3_', input$type, '_equity'),
        extension = 'png',
        content = function(file) {
          plot(penta3_equiplot())
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        },
        data = penta3_equiplot
      )

      downloadButtonServer(
        id = 'measles1_download',
        filename = paste0('measles1_', input$type, '_equity'),
        extension = 'png',
        content = function(file) {
          plot(measles1_equiplot())
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        },
        data = measles1_equiplot
      )

      downloadButtonServer(
        id = 'custom_download',
        filename = paste0(input$indicator, '_', input$type, '_equity'),
        extension = 'png',
        content = function(file) {
          plot(custom_equiplot())
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        },
        data = custom_equiplot
      )

      contentHeaderServer(
        'equity_assessment',
        md_title = 'Equity Assessment',
        md_file = '2_reporting_rate.md'
      )
    }
  )
}
