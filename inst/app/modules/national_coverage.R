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
          column(3, downloadUI(ns('measles1_download'), label = 'Download Plot')),
          column(3, downloadUI(ns('measles1_data_download'), label = 'Download Data'))
        )
      ),

      tabPanel(
        title = 'Penta 3',
        fluidRow(
          column(12, plotOutput(ns('penta3'))),
          column(3, downloadUI(ns('penta3_download'), label = 'Download Plot')),
          column(3, downloadUI(ns('penta3_data_download'), label = 'Download Data'))
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
          column(3, downloadUI(ns('custom_download'), label = 'Download Plot')),
          column(3, downloadUI(ns('custom_data_download'), label = 'Download Data'))
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

      national_rates <- reactive({
        national_values()$rates
      })

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

        # Update the select input
        updateSelectInput(
          session,
          'year',
          choices = years
        )
      })

      measles1_coverage <- reactive({
        if (!isTruthy(input$denominator) || !isTruthy(national_data()) || !isTruthy(national_data()$un) ||
            !isTruthy(national_data()$wuenic) || !isTruthy(national_data()$survdata) || !isTruthy(national_rates())) {
          return(NULL)
        }

        rates <- national_rates()
        surv_data <- national_data()

        analyze_coverage(data(),
                         indicator = 'measles1',
                         denominator = input$denominator,
                         un_estimates = surv_data$un,
                         wuenic_data = surv_data$wuenic,
                         survey_data = survdata(),
                         sbr = rates$sbr,
                         nmr = rates$nmr,
                         pnmr = rates$pnmr,
                         anc1survey =rates$anc1,
                         dpt1survey = rates$penta1,
                         twin = rates$twin_rate,
                         preg_loss = rates$preg_loss)
      })

      penta3_coverage <- reactive({
        if (!isTruthy(input$denominator) || !isTruthy(national_data()) || !isTruthy(national_data()$un) ||
            !isTruthy(national_data()$wuenic) || !isTruthy(national_data()$survdata) || !isTruthy(national_rates())) {
          return(NULL)
        }

        rates <- national_rates()
        surv_data <- national_data()

        analyze_coverage(data(),
                         indicator = 'penta3',
                         denominator = input$denominator,
                         un_estimates = surv_data$un,
                         wuenic_data = surv_data$wuenic,
                         survey_data = survdata(),
                         sbr = rates$sbr,
                         nmr = rates$nmr,
                         pnmr = rates$pnmr,
                         anc1survey =rates$anc1,
                         dpt1survey = rates$penta1,
                         twin = rates$twin_rate,
                         preg_loss = rates$preg_loss)
      })

      custom_coverage <- reactive({
        if (!isTruthy(input$indicator != '0') || !isTruthy(input$denominator) || !isTruthy(national_data()) || !isTruthy(national_data()$un) ||
            !isTruthy(national_data()$wuenic) || !isTruthy(national_data()$survdata) || !isTruthy(national_rates())) {
          return(NULL)
        }

        rates <- national_rates()
        surv_data <- national_data()

        analyze_coverage(data(),
                         indicator = input$indicator,
                         denominator = input$denominator,
                         un_estimates = surv_data$un,
                         wuenic_data = surv_data$wuenic,
                         survey_data = survdata(),
                         sbr = rates$sbr,
                         nmr = rates$nmr,
                         pnmr = rates$pnmr,
                         anc1survey =rates$anc1,
                         dpt1survey = rates$penta1,
                         twin = rates$twin_rate,
                         preg_loss = rates$preg_loss)
      })

      output$measles1 <- renderPlot({
        req(measles1_coverage())

        tryCatch({
            plot(measles1_coverage())
          },
          error = function(e) {

            clean_message <- cli::ansi_strip(conditionMessage(e))
            plot.new() # Start a blank plot
            text(
              x = 0.5, y = 0.5,
              labels = paste("Error:", clean_message),
              cex = 1.2, col = "red"
            )
          }
        )

      })

      output$penta3 <- renderPlot({
        req(penta3_coverage())

        tryCatch({
            plot(penta3_coverage())
          },
          error = function(e) {

            clean_message <- cli::ansi_strip(conditionMessage(e))
            plot.new() # Start a blank plot
            text(
              x = 0.5, y = 0.5,
              labels = paste("Error:", clean_message),
              cex = 1.2, col = "red"
            )
          }
        )

      })

      output$custom_check <- renderPlot({
        req(custom_coverage())

        tryCatch({
            plot(custom_coverage())
          },
          error = function(e) {
            clean_message <- cli::ansi_strip(conditionMessage(e))
            plot.new() # Start a blank plot
            text(
              x = 0.5, y = 0.5,
              labels = paste("Error:", clean_message),
              cex = 1.2, col = "red"
            )
          }
        )
      })

      downloadServer(
        id = 'measles1_download',
        filename = paste0('measles1_survey_', input$denominator),
        extension = 'png',
        content = function(file) {
          plot(measles1_coverage())
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        },
        data = measles1_coverage
      )

      downloadServer(
        id = 'measles1_data_download',
        filename = paste0('T3_measles1_survey_', input$denominator),
        extension = 'xlsx',
        content = function(file) {
          wb <- createWorkbook()
          coverage <- measles1_coverage()

          sheet_name_1 <- "Measles 1 Coverage"
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = coverage, startCol = 1, startRow = 1)

          # Save the workbook
          saveWorkbook(wb, file, overwrite = TRUE)
        },
        data = measles1_coverage
      )

      downloadServer(
        id = 'penta3_download',
        filename = paste0('penta3_survey_', input$denominator),
        extension = 'png',
        content = function(file) {
          plot(penta3_coverage())
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        },
        data = penta3_coverage
      )

      downloadServer(
        id = 'penta3_data_download',
        filename = paste0('T3_penta3_survey_', input$denominator),
        extension = 'xlsx',
        content = function(file) {
          wb <- createWorkbook()
          coverage <- penta3_coverage()

          sheet_name_1 <- "Penta 3 Coverage"
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = coverage, startCol = 1, startRow = 1)

          # Save the workbook
          saveWorkbook(wb, file, overwrite = TRUE)
        },
        data = penta3_coverage
      )

      downloadServer(
        id = 'custom_download',
        filename = paste0(input$indicator, '_survey_', input$denominator),
        extension = 'png',
        content = function(file) {
          plot(custom_coverage())
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        },
        data = custom_coverage
      )

      downloadServer(
        id = 'custom_data_download',
        filename = paste0('T3_', input$indicator, '_survey_', input$denominator),
        extension = 'xlsx',
        content = function(file) {
          wb <- createWorkbook()
          coverage <- custom_coverage()

          sheet_name_1 <- paste0(input$indicator, ' Coverage')
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = coverage, startCol = 1, startRow = 1)

          # Save the workbook
          saveWorkbook(wb, file, overwrite = TRUE)
        },
        data = custom_coverage
      )

      return(reactive(input$year))
    }
  )
}
