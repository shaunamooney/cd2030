lowReportingUI <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('low_reporting'), i18n$t("title_vaccination_coverage"), i18n = i18n),
    contentBody(
      box(
        title = i18n$t("title_analysis_options"),
        status = 'success',
        width = 12,
        solidHeader = TRUE,
        fluidRow(
          column(3, adminLevelInputUI(ns('admin_level'), i18n)),
          column(3, denominatorInputUI(ns('denominator'), i18n))
        )
      ),

      tabBox(
        title = i18n$t("title_vaccination_coverage"),
        width = 12,

        tabPanel(
          title = i18n$t("title_coverage"),
          fluidRow(
            column(12, plotCustomOutput(ns('coverage'))),
            downloadCoverageUI(ns('coverage_download'))
          )
        ),

        tabPanel(
          title = i18n$t("dropout"),
          fluidRow(
            column(12, plotCustomOutput(ns('dropout'))),
            downloadCoverageUI(ns('dropout_download'))
          )
        )
      )
    )
  )
}

lowReportingServer <- function(id, cache, i18n) {

  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      denominator <- denominatorInputServer('denominator', cache)
      admin_level <- adminLevelInputServer('admin_level')

      data <- reactive({
        req(cache())
        cache()$adjusted_data
      })

      coverage_threshold <- reactive({
        req(data(), admin_level(), all(!is.na(cache()$national_estimates)))
        rates <- cache()$national_estimates
        calculate_threshold(data(),
                            admin_level = admin_level(),
                            indicator = 'coverage',
                            sbr = rates$sbr,
                            nmr = rates$nmr,
                            pnmr = rates$pnmr,
                            anc1survey = rates$anc1,
                            dpt1survey = rates$penta1,
                            survey_year = cache()$survey_year,
                            twin = rates$twin_rate,
                            preg_loss = rates$preg_loss)
      })

      dropout_threshold <- reactive({
        req(data(), admin_level(), all(!is.na(cache()$national_estimates)))
        rates <- cache()$national_estimates
        calculate_threshold(data(),
                            admin_level = admin_level(),
                            indicator = 'dropout',
                            sbr = rates$sbr,
                            nmr = rates$nmr,
                            pnmr = rates$pnmr,
                            anc1survey = rates$anc1,
                            dpt1survey = rates$penta1,
                            survey_year = cache()$survey_year,
                            twin = rates$twin_rate,
                            preg_loss = rates$preg_loss)
      })

      output$coverage <- renderCustomPlot({
        req(coverage_threshold(), denominator())
        plot(coverage_threshold(), denominator = denominator())
      })

      output$dropout <- renderCustomPlot({
        req(dropout_threshold(), denominator())
        plot(dropout_threshold(), denominator = denominator())
      })

      contentHeaderServer(
        'low_reporting',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = i18n$t("title_vaccination_coverage"),
        md_file = '2_calculate_ratios.md',
        i18n = i18n
      )

    }
  )
}
