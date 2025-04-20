subnationalCoverageUI <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('subnational_coverage'), i18n$t("title_subnational_coverage"), i18n = i18n),
    contentBody(
      box(
        title = i18n$t("title_analysis_options"),
        status = 'success',
        width = 12,
        solidHeader = TRUE,
        fluidRow(
          column(3, selectizeInput(
            ns('admin_level'),
            label = i18n$t("title_admin_level"),
            choices = c('Admin Level 1' = 'adminlevel_1', 'District' = 'district')
          )),
          column(3, selectizeInput(
            ns('region'), label = i18n$t("opt_admin_level_1"), choices = NULL
          )),
          column(3, denominatorInputUI(ns('denominator'), i18n)),
          column(
            3,
            selectizeInput(ns('year'), label = i18n$t("title_survey_year"), choices = NULL)
          )
        )
      ),

      tabBox(
        title = i18n$t("title_subnational_coverage_trend"),
        id = 'national_trend',
        width = 12,

        tabPanel(title = i18n$t("opt_mcv1"), fluidRow(
          column(12, plotCustomOutput(ns('measles1'))), downloadCoverageUI(ns('measles1_download'))
        )),

        tabPanel(title = i18n$t("opt_penta3"), fluidRow(
          column(12, plotCustomOutput(ns('penta3'))), downloadCoverageUI(ns('penta3_download'))
        )),

        tabPanel(title = i18n$t("title_penta3_mcv1_dropout"), fluidRow(
          column(12, plotCustomOutput(ns(
            'dropout_penta3mcv1'
          ))), downloadCoverageUI(ns('dropout_penta3mcv1_download'))
        )),

        tabPanel(title = i18n$t("title_penta13_dropout"), fluidRow(
          column(12, plotCustomOutput(ns(
            'dropout_penta13'
          ))), downloadCoverageUI(ns('dropout_penta13_download'))
        )),

        tabPanel(
          i18n$t("opt_custom_check"),
          fluidRow(column(
            3, selectizeInput(
              ns('indicator'),
              label = i18n$t("title_indicator"),
              choices = c('Select' = '', list_vaccine_indicators())
            )
          )),
          fluidRow(column(12, plotCustomOutput(
            ns('custom_check')
          )), downloadCoverageUI(ns(
            'custom_download'
          )))
        )
      )
    )
  )
}

subnationalCoverageServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      denominator <- denominatorInputServer('denominator', cache)

      survey_data <- reactive({
        req(cache())
        cache()$regional_survey
      })

      coverage <- reactive({
        req(cache(), cache()$un_estimates, cache()$wuenic_estimates, survey_data())

        rates <- cache()$national_estimates
        filtered_survey_data <- survey_data() %>%
          filter(year >= as.numeric(input$year))

        cache()$adjusted_data %>%
          calculate_coverage(
            admin_level = input$admin_level,
            survey_data = filtered_survey_data,
            wuenic_data = cache()$wuenic_estimates,
            sbr = rates$sbr,
            nmr = rates$nmr,
            pnmr = rates$pnmr,
            twin = rates$twin_rate,
            preg_loss = rates$preg_loss,
            anc1survey = rates$anc1,
            dpt1survey = rates$penta1,
            subnational_map = cache()$survey_mapping
          )
      })

      observe({
        req(cache())

        data <- cache()$countdown_data

        # Extract distinct values if column_name is valid
        admin_level <- data %>%
          distinct(!!sym(input$admin_level)) %>%
          arrange(!!sym(input$admin_level)) %>%
          pull(!!sym(input$admin_level))

        selected_region <- if (input$admin_level == 'adminlevel_1') {
          admin_level_1 <- cache()$selected_admin_level_1
          if (is.null(admin_level_1)) {
            admin_level[1]
          } else {
            admin_level_1
          }
        } else {
          district <- cache()$selected_district
          if (is.null(district)) {
            admin_level[1]
          } else {
            district
          }
        }

        # Update the select input
        updateSelectInput(
          session,
          'region',
          choices = admin_level,
          selected = selected_region,
          label = if (input$admin_level == 'adminlevel_1') i18n$t("opt_admin_level_1") else i18n$t("opt_district")
        )
      })

      observeEvent(input$region, {
        req(cache())
        if (input$admin_level == 'adminlevel_1') {
          cache()$set_selected_admin_level_1(input$region)
        } else {
          cache()$set_selected_district(input$region)
        }
      })

      observe({
        req(survey_data())

        years <- survey_data() %>%
          distinct(year) %>%
          pull(year)

        selected_year <- cache()$start_survey_year

        updateSelectInput(session, 'year', choices = years, selected = selected_year)
      })

      observeEvent(input$year, {
        req(cache())
        cache()$set_start_survey_year(as.numeric(input$year))
      })

      output$measles1 <- renderCustomPlot({
        req(coverage(), denominator(), input$region)
        plot(coverage(), indicator = 'measles1', denominator = denominator(), region = input$region)
      })

      output$penta3 <- renderCustomPlot({
        req(coverage(), denominator(), input$region)
        plot(coverage(), indicator = 'penta3', denominator = denominator(), region = input$region)
      })

      output$dropout_penta13 <- renderCustomPlot({
        req(coverage(), denominator(), input$region)
        plot(coverage(), indicator = 'dropout_penta13', denominator = denominator(), region = input$region)
      })

      output$dropout_penta3mcv1 <- renderCustomPlot({
        req(coverage(), denominator(), input$region)
        plot(coverage(), indicator = 'dropout_penta3mcv1', denominator = denominator(), region = input$region)
      })

      output$custom_check <- renderCustomPlot({
        req(coverage(), denominator(), input$region, input$indicator)
        plot(coverage(), indicator = input$indicator, denominator = denominator(), region = input$region)
      })

      downloadCoverageServer(
        id = 'measles1_download',
        data = coverage,
        filename = paste0('measles1_', input$region, '_survey_', denominator()),
        indicator = reactive('measles1'),
        denominator = denominator,
        data_fn = filter_coverage,
        region = input$region,
        sheet_name = i18n$t("title_mcv1_coverage"),
        i18n = i18n
      )

      downloadCoverageServer(
        id = 'penta3_download',
        data = coverage,
        filename = paste0('penta3_', input$region, '_survey_', denominator()),
        indicator = reactive('penta3'),
        denominator = denominator,
        data_fn = filter_coverage,
        region = input$region,
        i18n = i18n,
        sheet_name = i18n$t("title_penta3_coverage")
      )

      downloadCoverageServer(
        id = 'dropout_penta13_download',
        data = coverage,
        filename = paste0('dropout_penta13_', input$region, '_survey_', denominator()),
        indicator = reactive('dropout_penta13'),
        denominator = denominator,
        data_fn = filter_coverage,
        region = input$region,
        i18n = i18n,
        sheet_name = i18n$t("title_penta13_dropout")
      )

      downloadCoverageServer(
        id = 'dropout_penta3mcv1_download',
        data = coverage,
        filename = paste0('dropout_penta3mcv1_', input$region, '_survey_', denominator()),
        indicator = reactive('dropout_penta3mcv1'),
        denominator = denominator,
        data_fn = filter_coverage,
        region = input$region,
        i18n = i18n,
        sheet_name = i18n$t("title_penta3_mcv1_dropout")
      )

      downloadCoverageServer(
        id = 'custom_download',
        data = coverage,
        filename = paste0(input$indicator, '_', input$region, '_survey_', denominator()),
        indicator = reactive(input$indicator),
        denominator = denominator,
        data_fn = filter_coverage,
        region = input$region,
        i18n = i18n,
        sheet_name = paste(input$indicator, i18n$t("title_coverage"))
      )

      contentHeaderServer(
        'subnational_coverage',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = i18n$t("title_subnational_coverage"),
        md_file = '2_reporting_rate.md',
        i18n = i18n
      )
    }
  )
}
