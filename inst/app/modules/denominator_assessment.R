denominatorAssessmentUI <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('denominator_assessment'), i18n$t("title_denominator_assessment"), i18n = i18n),
    contentBody(
      tabBox(
        title = i18n$t("title_denominator_assessment"),
        width = 12,
        tabPanel(
          title = i18n$t("opt_total_population"),
          fluidRow(
            column(12, plotCustomOutput(ns('population'))),
            column(4, downloadButtonUI(ns('population_plot')))
          )
        ),

        tabPanel(
          title = i18n$t("opt_births"),
          fluidRow(
            column(12, plotCustomOutput(ns('births'))),
            column(4, downloadButtonUI(ns('births_plot')))
          )
        )
      )
    )
  )
}

denominatorAssessmentServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      data <- reactive({
        req(cache())
        cache()$adjusted_data
      })

      un_estimates <- reactive({
        req(cache())
        cache()$un_estimates
      })

      denominators <- reactive({
        req(cache(), un_estimates())

        data() %>%
          prepare_population_metrics(un_estimates = un_estimates())
      })

      output$population <- renderCustomPlot({
        req(denominators())
        plot(denominators(), 'population')
      })

      output$births <- renderCustomPlot({
        req(denominators())
        plot(denominators(), 'births')
      })

      downloadPlot(
        id = 'population_plot',
        filename = reactive('population_plot'),
        data = denominators,
        i18n = i18n,
        plot_function = function() {
          plot(denominators(), 'population')
        }
      )

      downloadPlot(
        id = 'births_plot',
        filename = reactive('births_plot'),
        data = denominators,
        i18n = i18n,
        plot_function = function() {
          plot(denominators(), 'births')
        }
      )

      contentHeaderServer(
        'denominator_assessment',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = i18n$t("title_denominator_assessment"),
        md_file = '2_reporting_rate.md',
        i18n = i18n
      )
    }
  )
}
