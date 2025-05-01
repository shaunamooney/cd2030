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
      ),

      box(
        title = i18n$t('title_district_low_reporting'),
        status = 'success',
        collapsible = TRUE,
        width = 6,
        fluidRow(
          column(3, selectizeInput(ns('indicator'), label = i18n$t('title_indicator'), choice = list_vaccine_indicators())),
          # column(3, selectizeInput(ns('year'), label = i18n$t('title_year'), choice = NULL)),
          column(3, offset = 6, downloadButtonUI(ns('download_regions'))),
          column(12, withSpinner(reactableOutput(ns('district_low_reporting'))))
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
      region <- regionInputServer('region', cache, admin_level, i18n)

      data <- reactive({
        req(cache())
        cache()$adjusted_data
      })

      coverage <- reactive({
        req(cache(), cache()$survey_year, cache()$un_estimates, cache()$wuenic_estimates,
            cache()$regional_survey, all(!is.na(cache()$national_estimates)))

        rates <- cache()$national_estimates

        cache()$adjusted_data %>%
          calculate_coverage(
            admin_level = admin_level(),
            survey_data = cache()$regional_survey,
            wuenic_data = cache()$wuenic_estimates,
            sbr = rates$sbr,
            nmr = rates$nmr,
            pnmr = rates$pnmr,
            twin = rates$twin_rate,
            preg_loss = rates$preg_loss,
            anc1survey = rates$anc1,
            dpt1survey = rates$penta1,
            survey_year = cache()$survey_year,
            subnational_map = cache()$survey_mapping
          )
      })

      coverage_threshold <- reactive({
        req(data(), admin_level(), cache()$survey_year, all(!is.na(cache()$national_estimates)))
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

      district_coverage_rate <- reactive({
        req(coverage(), cache()$denominator, input$indicator)

        indicator <- paste0('cov_', input$indicator, '_', cache()$denominator)

        threshold_func <- if (grepl('zerodose|undervax|dropout_penta13|dropout_measles12|dropout_penta3mcv1|dropout_penta1mcv1', input$indicator)) {
          function(x) x < 10
        } else {
          function(x) x >= 90
        }

        coverage() %>%
          mutate(!!sym(indicator) := round(!!sym(indicator))) %>%
          # filter(!!sym(indicator) >= 90) %>%
          filter(threshold_func(!!sym(indicator))) %>%
          select(any_of(c('adminlevel_1', 'district', 'year', indicator)))
      })

      output$coverage <- renderCustomPlot({
        req(coverage_threshold(), denominator())
        plot(coverage_threshold(), denominator = denominator())
      })

      output$dropout <- renderCustomPlot({
        req(dropout_threshold(), denominator())
        plot(dropout_threshold(), denominator = denominator())
      })

      output$district_low_reporting <- renderReactable({
        req(district_coverage_rate())
        district_coverage_rate() %>%
          reactable()
      })

      downloadExcel(
        id = 'download_regions',
        filename = reactive(paste0('district_high_coverage_rate', input$year)),
        data = district_coverage_rate,
        i18n = i18n,
        excel_write_function = function(wb) {
          low_districts <- district_coverage_rate()

          sheet_name_1 <- i18n$t("title_districts_coverage_rate")
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = i18n$t("title_districts_coverage_rate"), startCol = 1, startRow = 1)
          writeData(wb, sheet = sheet_name_1, x = low_districts, startCol = 1, startRow = 3)
        }
      )

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
