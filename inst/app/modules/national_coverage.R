nationalCoverageUI <- function(id) {
  ns <- NS(id)

  fluidRow(
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
      )
    ),

    box(
      title = 'Custom Check',
      status = 'success',
      width = 12,
      fluidRow(
        column(3, selectizeInput(ns('indicator'), label = 'Indicator', choices = NULL))
      ),
      fluidRow(
        column(12, plotOutput(ns('custom_check')))
      )
    )
  )
}

nationalCoverageServer <- function(id, data) {
  stopifnot(is.reactive(data))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      observe({

        indicators <-  c("bcg", "anc1", "pcv3", "opv1", "opv2", "opv3", "penta2", "pcv1", "pcv2",
                      "penta1", "penta3", "measles1", "rota1", "rota2", "instdeliveries", "measles2",
                      "ipv1", "ipv2", "undervax", "dropout_penta13", "zerodose", "dropout_measles12",
                      "dropout_penta3mcv1")

        names(indicators) <- indicators
        indicators <- c('Select' = '0', indicators)

        updateSelectInput(session, 'indicator', choices = indicators)
      })

      output$measles1 <- renderPlot({

        country <- data() %>% select(country) %>% distinct(country) %>% pull(country)

        coverage <- analyze_national_coverage(data(), country, 'KEN', 'measles1')

        plot(coverage)

      })

      output$penta3 <- renderPlot({

        country <- data() %>% select(country) %>% distinct(country) %>% pull(country)

        coverage <- analyze_national_coverage(data(), country, 'KEN', 'penta3')

        plot(coverage)

      })

      output$custom_check <- renderPlot({
        req(input$indicator != '0')

        country <- data() %>% select(country) %>% distinct(country) %>% pull(country)

        coverage <- analyze_national_coverage(data(), country, 'KEN', input$indicator)

        plot(coverage)
      })

    }
  )
}
