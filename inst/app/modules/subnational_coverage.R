subnationalCoverageUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    box(
      title = 'Level of analysis',
      status = 'success',
      width = 12,
      solidHeader = TRUE,
      fluidRow(
        column(3, selectizeInput(ns('level'), label = 'Subnational Level',
                                 choices = c('Admin Level 1' = 'admin_level_1',
                                             'District' = 'district'))),
        column(3, selectizeInput(ns('denominator'), label = 'Denominator',
                                 choices = c('DHIS2' = 'dhis2',
                                             'ANC 1' = 'anc1',
                                             'Penta 1' = 'penta1')))
      )
    ),

    tabBox(
      title = 'Subnational Coverage',
      id = 'subnational_coverage',
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

subnationalCoverageServer <- function(id, data) {
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

        coverage <- analyze_subnational_coverage(data(), country, input$level, 'measles1', input$denominator)

        plot(coverage)
      })

      output$penta3 <- renderPlot({
        country <- data() %>% select(country) %>% distinct(country) %>% pull(country)

        coverage <- analyze_subnational_coverage(data(), country, input$level, 'penta3', input$denominator)

        plot(coverage)
      })

      output$custom_check <- renderPlot({
        req(input$indicator != '0')

        country <- data() %>% select(country) %>% distinct(country) %>% pull(country)

        coverage <- analyze_subnational_coverage(data(), country, input$level, input$indicator, input$denominator)

        plot(coverage)
      })

    }
  )
}
