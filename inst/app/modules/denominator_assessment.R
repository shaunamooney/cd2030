denominatorAssessmentUI <- function(id) {
  ns <- NS(id)

  fluidRow(

    tabBox(
      title = 'Denominator Assessment',
      id = 'denominator_assessment',
      width = 12,
      tabPanel(
        title = 'Total Population',
        fluidRow(
          column(12, plotOutput(ns('population')))
        )
      ),

      tabPanel(
        title = 'Births',
        fluidRow(
          column(12, plotOutput(ns('births')))
        )
      )
    ),

    tabBox(
      title = 'Denominator Selection',
      id = 'denominator_selection',
      width = 12,
      tabPanel(
        title = 'Penta 3',
        fluidRow(
          column(12, plotOutput(ns('penta3')))
        )
      ),

      tabPanel(
        title = 'Measles 1',
        fluidRow(
          column(12, plotOutput(ns('measles1')))
        )
      ),

      tabPanel(
        title = 'BCG',
        fluidRow(
          column(12, plotOutput(ns('bcg')))
        )
      )
    )
  )
}

denominatorAssessmentServer <- function(id, data, national_rates, national_survey) {
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(national_rates))
  stopifnot(is.reactive(national_survey))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      denominators <- reactive({

        country <- data() %>% select(country) %>% distinct(country) %>% pull(country)

        data() %>%
          prepare_population_metrics(country)
      })

      indicator_coverage <- reactive({

        country <- data() %>% select(country) %>% distinct(country) %>% pull(country)

        rates <- national_rates()
        surveys <- national_survey()

        data() %>%
          calculate_indicator_coverage(country,
                                       sbr = rates['sbr']/100,
                                       nmr = rates['nmr']/100,
                                       pnmr = rates['pnmr']/100,
                                       twin = rates['twin_rate']/100,
                                       preg_loss = rates['preg_loss']/100,
                                       anc1survey = surveys['anc1']/100,
                                       dpt1survey = surveys['penta1']/100)
      })

      output$population <- renderPlot({
        plot(denominators(), 'population')
      })

      output$births <- renderPlot({
        plot(denominators(), 'births')
      })

      output$penta3 <- renderPlot({
        plot_absolute_differences(indicator_coverage(), 'penta3')
      })

      output$measles1 <- renderPlot({
        plot_absolute_differences(indicator_coverage(), 'measles1')
      })

      output$bcg <- renderPlot({
        plot_absolute_differences(indicator_coverage(), 'bcg')
      })
    }
  )
}
