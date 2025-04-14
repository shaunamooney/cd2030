nationalCoverageUI <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('national_coverage'), i18n$t("title_national_coverage"), i18n = i18n),
    contentBody(
      box(
        title = i18n$t("title_analysis_options"),
        status = 'success',
        width = 12,
        solidHeader = TRUE,
        fluidRow(
          column(3, selectizeInput(ns('denominator'), label = i18n$t("title_denominator"),
                                   choices = c('DHIS2' = 'dhis2',
                                               'ANC 1' = 'anc1',
                                               'Penta 1' = 'penta1'))),
          column(3, selectizeInput(ns('year'), label = i18n$t("title_survey_year"), choices = NULL))
        )
      ),

      tabBox(
        title = i18n$t("title_national_coverage_trend"),
        id = 'national_trend',
        width = 12,

        tabPanel(
          title = i18n$t("opt_mcv1"),
          fluidRow(
            column(12, plotCustomOutput(ns('measles1'))),
            downloadCoverageUI(ns('measles1_download'))
          )
        ),

        tabPanel(
          title = i18n$t("opt_penta3"),
          fluidRow(
            column(12, plotCustomOutput(ns('penta3'))),
            downloadCoverageUI(ns('penta3_download'))
          )
        ),

        tabPanel(
          title = i18n$t("title_penta3_mcv1_dropout"),
          fluidRow(
            column(12, plotCustomOutput(ns('dropout_penta3mcv1'))),
            downloadCoverageUI(ns('dropout_penta3mcv1_download'))
          )
        ),

        tabPanel(
          title = i18n$t("title_penta13_dropout"),
          fluidRow(
            column(12, plotCustomOutput(ns('dropout_penta13'))),
            downloadCoverageUI(ns('dropout_penta13_download'))
          )
        ),

        tabPanel(
          i18n$t("opt_custom_check"),
          fluidRow(
            column(3, selectizeInput(ns('indicator'), label = i18n$t("title_indicator"),
                                     choices = c('Select' = '', list_vaccine_indicators())))
          ),
          fluidRow(
            column(12, plotCustomOutput(ns('custom_check'))),
            downloadCoverageUI(ns('custom_download'))
          )
        )
      )
    )
  )
}

nationalCoverageServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      survey_data <- reactive({
        req(cache())
        cache()$national_survey
      })

      coverage <- reactive({
        req(cache(), cache()$un_estimates, survey_data(), cache()$wuenic_estimates)

        rates <- cache()$national_estimates
        filtered_survey_data <- survey_data() %>%
          filter(year >= as.numeric(input$year))

        cache()$adjusted_data %>%
          calculate_coverage(
            survey_data = filtered_survey_data,
            wuenic_data = cache()$wuenic_estimates,
            un_estimates = cache()$un_estimates,
            sbr = rates$sbr,
            nmr = rates$nmr,
            pnmr = rates$pnmr,
            twin = rates$twin_rate,
            preg_loss = rates$preg_loss,
            anc1survey = rates$anc1,
            dpt1survey = rates$penta1
          )
      })

      denominator <- reactive({
        req(cache())
        cache()$denominator
      })

      observe({
        req(survey_data())

        years <- survey_data() %>%
          distinct(year) %>%
          arrange(year) %>%
          pull(year)

        updateSelectInput(session, 'year', choices = years, selected = cache()$start_survey_year)
      })

      observe({
        req(cache())
        updateSelectInput(session, 'denominator', selected = denominator())
      })

      observeEvent(input$year, {
        req(cache(), input$year)
        cache()$set_start_survey_year(as.numeric(input$year))
      })

      observeEvent(input$denominator, {
        req(cache())
        cache()$set_denominator(input$denominator)
      })

      output$measles1 <- renderCustomPlot({
        req(coverage(), denominator())
        plot(coverage(), indicator = 'measles1', denominator = denominator())
      })

      output$penta3 <- renderCustomPlot({
        req(coverage(), denominator())
        plot(coverage(), indicator = 'penta3', denominator = denominator())
      })

      output$dropout_penta13 <- renderCustomPlot({
        req(coverage(), denominator())
        plot(coverage(), indicator = 'dropout_penta13', denominator = denominator())
      })

      output$dropout_penta3mcv1 <- renderCustomPlot({
        req(coverage(), denominator())
        plot(coverage(), indicator = 'dropout_penta3mcv1', denominator = denominator())
      })

      output$custom_check <- renderCustomPlot({
        req(coverage(), denominator(), input$indicator)
        plot(coverage(), indicator = input$indicator, denominator = denominator())
      })

      downloadCoverageServer(
        id = 'measles1_download',
        data = coverage,
        filename = paste0('measles1_survey_', input$denominator),
        indicator = reactive('measles1'),
        denominator = denominator,
        data_fn = filter_coverage,
        sheet_name = i18n$t("title_mcv1_coverage"),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'penta3_download',
        data = coverage,
        filename = paste0('penta3_survey_', input$denominator),
        indicator = reactive('penta3'),
        denominator = denominator,
        data_fn = filter_coverage,
        sheet_name = i18n$t("title_penta3_coverage"),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'dropout_penta13_download',
        data = coverage,
        filename = paste0('dropout_penta13_survey_', input$denominator),
        indicator = reactive('dropout_penta13'),
        denominator = denominator,
        data_fn = filter_coverage,
        sheet_name = i18n$t("title_penta13_dropout"),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'dropout_penta3mcv1_download',
        data = coverage,
        filename = paste0('dropout_penta3mcv1_survey_', input$denominator),
        indicator = reactive('dropout_penta3mcv1'),
        denominator = denominator,
        data_fn = filter_coverage,
        sheet_name = i18n$t("title_penta3_mcv1_dropout"),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'custom_download',
        data = coverage,
        filename = paste0(input$indicator, '_survey_', input$denominator),
        indicator = reactive(input$indicator),
        denominator = denominator,
        data_fn = filter_coverage,
        sheet_name = paste(input$indicator, i18n$t("title_coverage")),
        i18n = i18n
      )

      contentHeaderServer(
        'national_coverage',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = i18n$t("title_national_coverage"),
        md_file = '2_reporting_rate.md',
        i18n = i18n
      )
    }
  )
}
