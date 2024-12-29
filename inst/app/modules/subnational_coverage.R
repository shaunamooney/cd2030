subnationalCoverageUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('subnational_coverage'), 'Subnational Coverage'),
    contentBody(
      box(
        title = 'Analysis Options',
        status = 'success',
        width = 12,
        solidHeader = TRUE,
        fluidRow(
          column(3, selectizeInput(ns('admin_level'), label ='Admin Level',
                                   choices = c('Admin Level 1' = 'adminlevel_1',
                                               'District' = 'district'))),
          column(3, selectizeInput(ns('region'), label ='Admin Level 1', choices = NULL)),
          column(3, selectizeInput(ns('denominator'), label = 'Denominator',
                                   choices = c('DHIS2' = 'dhis2',
                                               'ANC 1' = 'anc1',
                                               'Penta 1' = 'penta1'))),
          column(3, selectizeInput(ns('year'), label = 'Survey Start Year', choices = NULL))
        )
      ),

      tabBox(
        title = 'Subnational Coverage Trend',
        id = 'national_trend',
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
  )
}

subnationalCoverageServer <- function(id, data, national_values) {
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(national_values))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      national_data <- reactive({
        national_values()$data
      })

      gregion <- reactive({
        req(national_data()$gregion)

        national_data()$gregion %>%
          filter(year >= as.numeric(input$year))
      })

      indicator_coverage <- reactive({
        if (!isTruthy(data()) || !isTruthy(input$admin_level)) return(NULL)

        rates <- national_values()$rates
        data() %>%
          calculate_indicator_coverage(
            admin_level = input$admin_level,
            sbr = rates$sbr,
            nmr = rates$nmr,
            pnmr = rates$pnmr,
            twin = rates$twin_rate,
            preg_loss = rates$preg_loss,
            anc1survey = rates$anc1,
            dpt1survey = rates$penta1)
      })


      observe({
        req(indicator_coverage())

        # Extract distinct values if column_name is valid
        admin_level <- indicator_coverage() %>%
          distinct(!!sym(input$admin_level)) %>%
          arrange(!!sym(input$admin_level)) %>%
          pull(!!sym(input$admin_level))

        # Update the select input
        updateSelectInput(
          session,
          'region',
          choices = admin_level,
          label = if (input$admin_level == 'adminlevel_1') 'Admin Level 1' else 'District'
        )
      })

      observe({
        req(national_data()$survdata)

        years <- national_data()$survdata %>%
          distinct(year) %>%
          pull(year)

        # Update the select input
        updateSelectInput(
          session,
          'year',
          choices = years
        )
      })

      measles1_coverage <- reactive({
        if (!isTruthy(indicator_coverage()) || !isTruthy(input$denominator) || !isTruthy(national_data()) ||
            !isTruthy(input$region) || !isTruthy(national_data()$wuenic) || !isTruthy(national_data()$gregion)) {
          return(NULL)
        }

        indicator_coverage() %>%
          analyze_coverage(
            admin_level = input$admin_level,
            indicator = 'measles1',
            denominator = input$denominator,
            survey_data = gregion(),
            wuenic_data = national_data()$wuenic,
            subnational_map = national_data()$survey_map,
            region = input$region
          )
      })

      penta3_coverage <- reactive({
        if (!isTruthy(indicator_coverage()) || !isTruthy(input$denominator) || !isTruthy(national_data()) ||
            !isTruthy(input$region) || !isTruthy(national_data()$wuenic) || !isTruthy(national_data()$gregion)) {
          return(NULL)
        }

        indicator_coverage() %>%
          analyze_coverage(
            admin_level = input$admin_level,
            indicator = 'penta3',
            denominator = input$denominator,
            survey_data = gregion(),
            wuenic_data = national_data()$wuenic,
            subnational_map = national_data()$survey_map,
            region = input$region
          )
      })

      custom_coverage <- reactive({
        if (!isTruthy(indicator_coverage()) || !isTruthy(input$indicator != '0') || !isTruthy(input$denominator) || !isTruthy(national_data()) ||
            !isTruthy(input$region) || !isTruthy(national_data()$wuenic) || !isTruthy(national_data()$gregion)) {
          return(NULL)
        }

        indicator_coverage() %>%
          analyze_coverage(
            admin_level = input$admin_level,
            indicator = input$indicator,
            denominator = input$denominator,
            survey_data = gregion(),
            wuenic_data = national_data()$wuenic,
            subnational_map = national_data()$survey_map,
            region = input$region
          )
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
        filename = paste0('measles1_', input$region, '_survey_', input$denominator),
        sheet_name = 'Measles 1 Coverage'
      )

      downloadCoverageServer(
        id = 'penta3_download',
        data_fn = penta3_coverage,
        filename = paste0('penta3_', input$region, '_survey_', input$denominator),
        sheet_name = 'Penta 3 Coverage'
      )

      downloadCoverageServer(
        id = 'custom_download',
        data_fn = penta3_coverage,
        filename = paste0(input$indicator, '_', input$region, '_survey_', input$denominator),
        sheet_name = paste0(input$indicator, ' Coverage')
      )

      contentHeaderServer(
        'subnational_coverage',
        md_title = 'Subnational Coverage',
        md_file = '2_reporting_rate.md'
      )
    }
  )
}
