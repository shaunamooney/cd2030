subnationalInequalityUI <- function(id) {
  ns <- NS(id)

  fluidRow(
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
          column(12, plotOutput(ns('measles1'))),
          downloadCoverageUI(ns('measles1_download'))
        )
      ),

      tabPanel(
        title = 'Penta 3',
        fluidRow(
          column(12, plotOutput(ns('penta3'))),
          downloadCoverageUI(ns('penta3_download'))
        )
      ),

      tabPanel(
        'Custom Check',
        fluidRow(
          column(3, selectizeInput(ns('indicator'), label = 'Indicator',
                                   choices = c('Select' = '0', "bcg", "anc1", "opv1", "opv2", "opv3", "pcv1", "pcv2", "pcv3",
                                               "penta1", "penta2", "rota1", "rota2", "instdeliveries", "measles2",
                                               "ipv1", "ipv2", "undervax", "dropout_penta13", "zerodose", "dropout_measles12",
                                               "dropout_penta3mcv1")))
        ),
        fluidRow(
          column(12, plotOutput(ns('custom_check'))),
          downloadCoverageUI(ns('custom_download'))
        )
      )
    )
  )
}

subnationalInequalityServer <- function(id, data, national_values) {
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(national_values))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      national_rates <- reactive({
        national_values()$rates
      })

      national_data <- reactive({
        national_values()$data
      })

      measles1_coverage <- reactive({
        if (!isTruthy(input$level) || !isTruthy(input$denominator) ||  !isTruthy(national_rates()) ||
            !isTruthy(national_data()) || !isTruthy(national_data()$un)) {
          return(NULL)
        }

        surv_data <- national_data()
        rates <- national_rates()

        analyze_inequality(data(),
                           admin_level = input$level,
                           indicator = 'measles1',
                           denominator =  input$denominator,
                           un_estimates = surv_data$un,
                           sbr = rates$sbr,
                           nmr = rates$nmr,
                           pnmr = rates$pnmr,
                           anc1survey = rates$anc1,
                           dpt1survey = rates$penta1,
                           twin = rates$twin_rate,
                           preg_loss = rates$preg_loss)
      })

      penta3_coverage <- reactive({
        if (!isTruthy(input$level) || !isTruthy(input$denominator) ||  !isTruthy(national_rates()) ||
            !isTruthy(national_data()) || !isTruthy(national_data()$un)) {
          return(NULL)
        }

        surv_data <- national_data()
        rates <- national_rates()

        analyze_inequality(data(),
                           admin_level = input$level,
                           indicator = 'penta3',
                           denominator =  input$denominator,
                           un_estimates = surv_data$un,
                           sbr = rates$sbr,
                           nmr = rates$nmr,
                           pnmr = rates$pnmr,
                           anc1survey = rates$anc1,
                           dpt1survey = rates$penta1,
                           twin = rates$twin_rate,
                           preg_loss = rates$preg_loss)
      })

      custom_coverage <- reactive({
        if (!isTruthy(input$level) || !isTruthy(input$denominator) ||  !isTruthy(national_rates()) ||
            !isTruthy(input$indicator != '0') || !isTruthy(national_data()) || !isTruthy(national_data()$un)) {
          return(NULL)
        }

        surv_data <- national_data()
        rates <- national_rates()

        analyze_inequality(data(),
                           admin_level = input$level,
                           indicator = input$indicator,
                           denominator =  input$denominator,
                           un_estimates = surv_data$un,
                           sbr = rates$sbr,
                           nmr = rates$nmr,
                           pnmr = rates$pnmr,
                           anc1survey = rates$anc1,
                           dpt1survey = rates$penta1,
                           twin = rates$twin_rate,
                           preg_loss = rates$preg_loss)
      })

      output$measles1 <- renderPlot({
        req(measles1_coverage())

        render_with_error_handling({
          plot(measles1_coverage())
        })
      })

      output$penta3 <- renderPlot({
        req(penta3_coverage())

        render_with_error_handling({
          plot(penta3_coverage())
        })
      })

      output$custom_check <- renderPlot({
        req(custom_coverage())

        render_with_error_handling({
          plot(custom_coverage())
        })
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
    }
  )
}
