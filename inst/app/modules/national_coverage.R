nationalCoverageUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    box(
      title = 'Level of analysis',
      status = 'success',
      width = 12,
      solidHeader = TRUE,
      fluidRow(
        column(3, selectizeInput(ns('denominator'), label = 'Denominator',
                                 choices = c('DHIS2' = 'dhis2',
                                             'ANC 1' = 'anc1',
                                             'Penta 1' = 'penta1')))
      )
    ),

    tabBox(
      title = 'National Coverage Trend',
      id = 'national_trend',
      width = 12,

      tabPanel(
        title = 'Measles 1',
        fluidRow(
          column(12, plotOutput(ns('measles1')))
        )
      ),

      tabPanel(
        title = 'Penta 3',
        fluidRow(
          column(12, plotOutput(ns('penta3')))
        )
      ),

      tabPanel(
        'Custom Check',
        fluidRow(
          column(3, selectizeInput(ns('indicator'), label = 'Indicator', choices = NULL))
        ),
        fluidRow(
          column(12, plotOutput(ns('custom_check')))
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

      observe({

        indicators <-  c("bcg", "anc1", "pcv3", "opv1", "opv2", "opv3", "penta2", "pcv1", "pcv2",
                      "penta1", "rota1", "rota2", "instdeliveries", "measles2",
                      "ipv1", "ipv2", "undervax", "dropout_penta13", "zerodose", "dropout_measles12",
                      "dropout_penta3mcv1")

        names(indicators) <- indicators
        indicators <- c('Select' = '0', indicators)

        updateSelectInput(session, 'indicator', choices = indicators)
      })

      output$measles1 <- renderPlot({
        req(national_data(), national_rates(), national_data()$un, national_data()$wuenic, national_data()$survdata)

        rates <- national_rates()
        surv_data <- national_data()

        coverage <- analyze_national_coverage(data(),
                                              indicator = 'measles1',
                                              denominator = input$denominator,
                                              un_estimates = surv_data$un,
                                              wuenic_data = surv_data$wuenic,
                                              survey_data = surv_data$survdata,
                                              sbr = rates$sbr,
                                              nmr = rates$nmr,
                                              pnmr = rates$pnmr,
                                              anc1survey =rates$anc1,
                                              dpt1survey = rates$penta1,
                                              twin = rates$twin_rate,
                                              preg_loss = rates$preg_loss)

        tryCatch(
          plot(coverage),
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
        req(national_data(), national_rates(), national_data()$un, national_data()$wuenic, national_data()$survdata)

        rates <- national_rates()
        surv_data <- national_data()

        coverage <- analyze_national_coverage(data(),
                                              indicator = 'penta3',
                                              denominator = input$denominator,
                                              un_estimates = surv_data$un,
                                              wuenic_data = surv_data$wuenic,
                                              survey_data = surv_data$survdata,
                                              sbr = rates$sbr,
                                              nmr = rates$nmr,
                                              pnmr = rates$pnmr,
                                              anc1survey =rates$anc1,
                                              dpt1survey = rates$penta1,
                                              twin = rates$twin_rate,
                                              preg_loss = rates$preg_loss)

        tryCatch(
          plot(coverage),
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
        req(input$indicator != '0', national_data(), national_rates(), national_data()$un)

        rates <- national_rates()
        surv_data <- national_data()

        coverage <- analyze_national_coverage(data(),
                                              indicator = input$indicator,
                                              denominator = input$denominator,
                                              un_estimates = surv_data$un,
                                              wuenic_data = surv_data$wuenic,
                                              survey_data = surv_data$survdata,
                                              sbr = rates$sbr,
                                              nmr = rates$nmr,
                                              pnmr = rates$pnmr,
                                              anc1survey =rates$anc1,
                                              dpt1survey = rates$penta1,
                                              twin = rates$twin_rate,
                                              preg_loss = rates$preg_loss)


        plot(coverage)
      })

    }
  )
}
