denominatorSelectionUI <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('denominator_selection'), i18n$t("title_denominator_selection"), i18n = i18n),
    contentBody(
      box(
        title = i18n$t("title_analysis_options"),
        status = 'success',
        width = 12,
        solidHeader = TRUE,
        fluidRow(column(3, denominatorInputUI(ns('denominator'), i18n)))
      ),
      tabBox(
        title = i18n$t("title_denominator_selection"),
        width = 12,
        tabPanel(
          title = i18n$t("opt_penta3"),
          fluidRow(
            column(12, plotCustomOutput(ns('penta3'))),
            column(3, downloadButtonUI(ns('penta3_plot')))
          )
        ),

        tabPanel(
          title = i18n$t("opt_mcv1"),
          fluidRow(
            column(12, plotCustomOutput(ns('measles1'))),
            column(3, downloadButtonUI(ns('measles1_plot')))
          )
        ),

        tabPanel(
          title = i18n$t("opt_bcg"),
          fluidRow(
            column(12, plotCustomOutput(ns('bcg'))),
            column(3, downloadButtonUI(ns('bcg_plot')))
          )
        )
      )
    )
  )
}

denominatorSelectionServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      denominatorInputServer('denominator', cache, allowInput = TRUE)

      data <- reactive({
        req(cache())
        cache()$adjusted_data
      })

      un_estimates <- reactive({
        req(cache())
        cache()$un_estimates
      })

      national_estimates <- reactive({
        req(cache())
        cache()$national_estimates
      })

      survey_estimates <- reactive({
        req(cache())
        cache()$survey_estimates
      })

      indicator_coverage <- reactive({
        req(data(), cache()$survey_year, un_estimates(), all(!is.na(national_estimates())))

        rates <- national_estimates()
        data() %>%
          calculate_indicator_coverage(un_estimates = un_estimates(),
                                       sbr = rates$sbr,
                                       nmr = rates$nmr,
                                       pnmr = rates$pnmr,
                                       twin = rates$twin_rate,
                                       preg_loss = rates$preg_loss,
                                       anc1survey = rates$anc1,
                                       dpt1survey = rates$penta1,
                                       survey_year = cache()$survey_year)
      })

      output$penta3 <- renderCustomPlot({
        req(indicator_coverage(), all(!is.na(survey_estimates())))
        penta3_rate <- unname(survey_estimates()['penta3'])
        plot_absolute_differences(indicator_coverage(), 'penta3', penta3_rate)
      })

      output$measles1 <- renderCustomPlot({
        req(indicator_coverage(), all(!is.na(survey_estimates())))
        measles1_rate <- unname(survey_estimates()['measles1'])
        plot_absolute_differences(indicator_coverage(), 'measles1', measles1_rate)
      })

      output$bcg <- renderCustomPlot({
        req(indicator_coverage(), all(!is.na(survey_estimates())))
        bcg_rate <- unname(survey_estimates()['bcg'])
        plot_absolute_differences(indicator_coverage(), 'bcg', bcg_rate)
      })

      output$custom_plot <- renderCustomPlot({
        req(indicator_coverage())
        plot_absolute_differences(indicator_coverage(), input$indicator)
      })

      downloadPlot(
        id = 'penta3_plot',
        filename = reactive('penta3_plot'),
        data = indicator_coverage,
        i18n = i18n,
        plot_function = function() {
          penta3_rate <- unname(survey_estimates()['penta3'])
          plot_absolute_differences(indicator_coverage(), 'penta3', penta3_rate)
        }
      )

      downloadPlot(
        id = 'measles1_plot',
        filename = reactive('measles1_plot'),
        data = indicator_coverage,
        i18n = i18n,
        plot_function = function() {
          measles1_rate <- unname(survey_estimates()['measles1'])
          plot_absolute_differences(indicator_coverage(), 'measles1', measles1_rate)
        }
      )

      downloadPlot(
        id = 'bcg_plot',
        filename = reactive('bcg_plot'),
        data = indicator_coverage,
        i18n = i18n,
        plot_function = function() {
          bcg_rate <- unname(survey_estimates()['bcg'])
          plot_absolute_differences(indicator_coverage(), 'bcg', bcg_rate)
        }
      )

      contentHeaderServer(
        'denominator_selection',
        cache = cache,
        object = pageObjectsConfig(input),
        md_title = i18n$t("title_denominator_selection"),
        md_file = '2_reporting_rate.md',
        i18n = i18n
      )
    }
  )
}
