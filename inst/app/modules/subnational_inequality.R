subnationalInequalityUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('subnational_inequality'), 'Subnational Inequality'),
    contentBody(
      box(
        title = 'Analysis Options',
        status = 'success',
        width = 12,
        solidHeader = TRUE,
        fluidRow(
          column(3, selectizeInput(ns('level'), label = 'Subnational Level',
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
          title = 'Measles 1',
          fluidRow(
            column(12, plotCustomOutput(ns('measles1'))),
            downloadCoverageUI(ns('measles1_download'))
          )
        ),

        tabPanel(
          title = 'Penta 3',
          fluidRow(
            column(12, plotCustomOutput(ns('penta3'))),
            downloadCoverageUI(ns('penta3_download'))
          )
        ),

        tabPanel(
          'Custom Check',
          fluidRow(
            column(3, selectizeInput(ns('indicator'), label = 'Indicator',
                                     choices = c('Select' = '', "anc1", "bcg",  "opv1", "opv2", "opv3", "pcv1", "pcv2", "pcv3",
                                                 "penta1", "penta2", "rota1", "rota2", "measles2", "ipv1", "ipv2")))
          ),
          fluidRow(
            column(12, plotCustomOutput(ns('custom_check'))),
            downloadCoverageUI(ns('custom_download'))
          )
        )
      )
    )
  )
}

subnationalInequalityServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      data <- reactive({
        req(cache())
        cache()$get_adjusted_data()
      })

      un_estimates <- reactive({
        req(cache())
        cache()$get_un_estimates()
      })

      national_data <- reactive({
        national_values()$data
      })

      measles1_coverage <- reactive({
        req(input$level, input$denominator, un_estimates())

        rates <- cache()$get_national_estimates()

        analyze_inequality(data(),
                           admin_level = input$level,
                           indicator = 'measles1',
                           denominator =  input$denominator,
                           un_estimates =  un_estimates(),
                           sbr = rates$sbr,
                           nmr = rates$nmr,
                           pnmr = rates$pnmr,
                           anc1survey = rates$anc1,
                           dpt1survey = rates$penta1,
                           twin = rates$twin_rate,
                           preg_loss = rates$preg_loss)
      })

      penta3_coverage <- reactive({
        req(input$level, input$denominator, un_estimates())

        rates <- cache()$get_national_estimates()

        analyze_inequality(data(),
                           admin_level = input$level,
                           indicator = 'penta3',
                           denominator =  input$denominator,
                           un_estimates = un_estimates(),
                           sbr = rates$sbr,
                           nmr = rates$nmr,
                           pnmr = rates$pnmr,
                           anc1survey = rates$anc1,
                           dpt1survey = rates$penta1,
                           twin = rates$twin_rate,
                           preg_loss = rates$preg_loss)
      })

      custom_coverage <- reactive({
        req(input$level, input$denominator, un_estimates(), input$indicator)

        rates <- cache()$get_national_estimates()

        analyze_inequality(data(),
                           admin_level = input$level,
                           indicator = input$indicator,
                           denominator =  input$denominator,
                           un_estimates = un_estimates(),
                           sbr = rates$sbr,
                           nmr = rates$nmr,
                           pnmr = rates$pnmr,
                           anc1survey = rates$anc1,
                           dpt1survey = rates$penta1,
                           twin = rates$twin_rate,
                           preg_loss = rates$preg_loss)
      })

      output$measles1 <- renderCustomPlot({
        req(measles1_coverage())
        plot(measles1_coverage())
      })

      output$penta3 <- renderCustomPlot({
        req(penta3_coverage())
        plot(penta3_coverage())
      })

      output$custom_check <- renderCustomPlot({
        req(custom_coverage())
        plot(custom_coverage())
      })

      observe({
        req(cache())

        selected_denoninator <- cache()$get_denominator()
        updateSelectInput(session, 'denominator', selected = selected_denoninator)
      })

      observeEvent(input$denominator, {
        req(cache())
        cache()$set_denominator(input$denominator)
      })

      downloadCoverageServer(
        id = 'measles1_download',
        data_fn = measles1_coverage,
        filename = paste0('measles1_', input$level, '_inequality_', input$denominator),
        sheet_name = 'Measles 1 Inequality'
      )

      downloadCoverageServer(
        id = 'penta3_download',
        data_fn = penta3_coverage,
        filename = paste0('penta3_', input$level, '_inequality_', input$denominator),
        sheet_name = 'Penta 3 Inequality'
      )

      downloadCoverageServer(
        id = 'custom_download',
        data_fn = custom_coverage,
        filename = paste0(input$indicator, '_', input$level, '_inequality_', input$denominator),
        sheet_name = paste0(input$indicator, ' Inequality')
      )

      contentHeaderServer(
        'subnational_inequality',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = 'Subnational Inequality',
        md_file = '2_reporting_rate.md'
      )
    }
  )
}
