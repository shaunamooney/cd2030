nationalCoverageUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    box(
      title = 'Analysis Options',
      status = 'success',
      width = 12,
      solidHeader = TRUE,
      fluidRow(
        column(3, selectizeInput(ns('denominator'), label = 'Denominator',
                                 choices = c('DHIS2' = 'dhis2',
                                             'ANC 1' = 'anc1',
                                             'Penta 1' = 'penta1'))),
        column(3, selectizeInput(ns('year'), label = 'Survey Start Year', choices = NULL))
      )
    ),

    tabBox(
      title = 'National Coverage Trend',
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
                                   choices = c('Select' = '0', "bcg", "anc1", "opv1", "opv2", "opv3", "penta2", "pcv1", "pcv2", "pcv3",
                                               "penta1", "penta2",  "rota1", "rota2", "instdeliveries", "measles2",
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

nationalCoverageServer <- function(id, data, national_values) {
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(national_values))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      national_data <- reactive({
        national_values()$data
      })

      survdata <- reactive({
        req(national_data()$survdata)

        national_data()$survdata %>%
          filter(year >= as.numeric(input$year))
      })

      observe({
        req(national_data()$survdata)

        years <- national_data()$survdata %>%
          distinct(year) %>%
          arrange(year) %>%
          pull(year)

        updateSelectInput(
          session,
          'year',
          choices = years
        )
      })

      measles1_coverage <- reactive({
        if (!isTruthy(input$denominator) || !isTruthy(national_data()) ||
            !isTruthy(national_data()$wuenic) || !isTruthy(national_data()$survdata)) {
          return(NULL)
        }

        data() %>%
          analyze_coverage(
            indicator = 'measles1',
            denominator = input$denominator,
            survey_data = survdata(),
            wuenic_data = national_data()$wuenic
          )
      })

      penta3_coverage <- reactive({
        if (!isTruthy(input$denominator) || !isTruthy(national_data()) ||
            !isTruthy(national_data()$wuenic) || !isTruthy(national_data()$survdata)) {
          return(NULL)
        }

        data() %>%
          analyze_coverage(
            indicator = 'penta3',
            denominator = input$denominator,
            survey_data = survdata(),
            wuenic_data = national_data()$wuenic
          )
      })

      custom_coverage <- reactive({
        if (!isTruthy(input$indicator != '0') || !isTruthy(input$denominator) || !isTruthy(national_data()) ||
            !isTruthy(national_data()$wuenic) || !isTruthy(national_data()$survdata)) {
          return(NULL)
        }

        data() %>%
          analyze_coverage(
            indicator = input$indicator,
            denominator = input$denominator,
            survey_data = survdata(),
            wuenic_data = national_data()$wuenic
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
        filename = paste0('measles1_survey_', input$denominator),
        sheet_name = 'Measles 1 Coverage'
      )

      downloadCoverageServer(
        id = 'penta3_download',
        data_fn = measles1_coverage,
        filename = paste0('penta3_survey_', input$denominator),
        sheet_name = 'Penta 3 Coverage'
      )

      downloadCoverageServer(
        id = 'custom_download',
        data_fn = measles1_coverage,
        filename = paste0(input$indicator, '_survey_', input$denominator),
        sheet_name = paste0(input$indicator, ' Coverage')
      )

      return(reactive(input$year))
    }
  )
}
