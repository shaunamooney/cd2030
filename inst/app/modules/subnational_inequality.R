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
          title = 'Penta 1 to Penta 3 Dropout',
          fluidRow(
            column(12, plotCustomOutput(ns('dropout_penta13'))),
            downloadCoverageUI(ns('dropout_penta13_download'))
          )
        ),

        tabPanel(
          title = 'Penta 3 to Measles 1 Dropout',
          fluidRow(
            column(12, plotCustomOutput(ns('dropout_penta3mcv1'))),
            downloadCoverageUI(ns('dropout_penta3mcv1_download'))
          )
        ),

        tabPanel(
          'Custom Check',
          fluidRow(
            column(3, selectizeInput(ns('indicator'), label = 'Indicator',
                                     choices = c(
                                       'Select' = '', "bcg", "anc1", "opv1", "opv2", "opv3", "pcv1",
                                       "pcv2", "pcv3", "penta1", "penta2", "rota1", "rota2",
                                       "instdeliveries", "measles2", "ipv1", "ipv2", "undervax",
                                       "zerodose", "dropout_measles12"
                                     )
            )
            )
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

      denominator <- reactive({
        req(cache())
        cache()$denominator
      })

      inequalities <- reactive({
        req(cache(), cache()$un_estimates, input$admin_level)

        rates <- cache()$national_estimates

        calculate_inequality(
          .data = cache()$adjusted_data,
          admin_level = input$admin_level,
          un_estimates = cache()$un_estimates,
          sbr = rates$sbr,
          nmr = rates$nmr,
          pnmr = rates$pnmr,
          anc1survey = rates$anc1,
          dpt1survey = rates$penta1,
          twin = rates$twin_rate,
          preg_loss = rates$preg_loss
        )
      })

      observe({
        req(cache())
        updateSelectInput(session, 'denominator', selected = cache()$denominator)
      })

      observeEvent(input$denominator, {
        req(cache())
        cache()$set_denominator(input$denominator)
      })

      output$measles1 <- renderCustomPlot({
        req(inequalities(), denominator())
        plot(inequalities(), indicator = 'measles1', denominator = denominator())
      })

      output$penta3 <- renderCustomPlot({
        req(inequalities(), denominator())
        plot(inequalities(), indicator = 'penta3', denominator = denominator())
      })

      output$dropout_penta13 <- renderCustomPlot({
        req(inequalities(), denominator())
        plot(inequalities(), indicator = 'dropout_penta13', denominator = denominator())
      })

      output$dropout_penta3mcv1 <- renderCustomPlot({
        req(inequalities(), denominator())
        plot(inequalities(), indicator = 'dropout_penta3mcv1', denominator = denominator())
      })

      output$custom_check <- renderCustomPlot({
        req(inequalities(), denominator(), input$indicator)
        plot(inequalities(), indicator = input$indicator, denominator = denominator())
      })

      downloadCoverageServer(
        id = 'measles1_download',
        data = inequalities,
        filename = paste0('measles1_', input$level, '_inequality_', denominator()),
        indicator = reactive('measles1'),
        denominator = denominator,
        data_fn = filter_inequality,
        sheet_name = 'Measles 1 Inequality'
      )

      downloadCoverageServer(
        id = 'penta3_download',
        data = inequalities,
        filename = paste0('penta3_', input$level, '_inequality_', denominator()),
        indicator = reactive('penta3'),
        denominator =denominator,
        data_fn = filter_inequality,
        sheet_name = 'Penta 3 Inequality'
      )

      downloadCoverageServer(
        id = 'dropout_penta13_download',
        data = inequalities,
        filename = paste0('dropout_penta13_', input$level, '_inequality_', denominator()),
        indicator = reactive('dropout_penta13'),
        denominator = denominator,
        data_fn = filter_inequality,
        sheet_name = 'Penta 1 to Penta 3 Dropout Inequality'
      )

      downloadCoverageServer(
        id = 'dropout_penta3mcv1_download',
        data = inequalities,
        filename = paste0('dropout_penta3mcv1_', input$level, '_inequality_', denominator()),
        indicator = reactive('dropout_penta3mcv1'),
        denominator = denominator,
        data_fn = filter_inequality,
        sheet_name = 'Penta 3 to Measles 1 Dropout Inequality'
      )

      downloadCoverageServer(
        id = 'custom_download',
        data = inequalities,
        filename = paste0(input$indicator, '_', input$level, '_inequality_', denominator()),
        indicator = reactive(input$indicator),
        denominator = denominator,
        data_fn = filter_inequality,
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
