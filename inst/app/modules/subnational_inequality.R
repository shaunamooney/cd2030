subnationalInequalityUI <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('subnational_inequality'), i18n$t("title_subnational_inequality"), i18n = i18n),
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
        title = i18n$t("title_subnational_inequality"),
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
          title = i18n$t("title_penta13_dropout"),
          fluidRow(
            column(12, plotCustomOutput(ns('dropout_penta13'))),
            downloadCoverageUI(ns('dropout_penta13_download'))
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

subnationalInequalityServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      denominator <- denominatorInputServer('denominator', cache)
      admin_level <- adminLevelInputServer('admin_level')

      inequalities <- reactive({
        req(cache(), cache()$adjusted_data, cache()$survey_year, cache()$un_estimates,
            admin_level(), all(!is.na(cache()$national_estimates)))

        rates <- cache()$national_estimates

        calculate_inequality(
          .data = cache()$adjusted_data,
          admin_level = admin_level(),
          un_estimates = cache()$un_estimates,
          sbr = rates$sbr,
          nmr = rates$nmr,
          pnmr = rates$pnmr,
          anc1survey = rates$anc1,
          dpt1survey = rates$penta1,
          survey_year = cache()$survey_year,
          twin = rates$twin_rate,
          preg_loss = rates$preg_loss
        )
      })

      output$measles1 <- renderCustomPlot({
        req(inequalities(), denominator())
        plot(inequalities(), indicator = 'measles1', denominator = denominator())
      })

      output$penta3 <- renderCustomPlot({
        req(inequalities(), denominator())
        plot(inequalities(), indicator = 'penta3', denominator = denominator())
      })

      output$dropout_penta13 <- renderCustomPlot({
        req(inequalities(), denominator())
        plot(inequalities(), indicator = 'dropout_penta13', denominator = denominator())
      })

      output$dropout_penta3mcv1 <- renderCustomPlot({
        req(inequalities(), denominator())
        plot(inequalities(), indicator = 'dropout_penta3mcv1', denominator = denominator())
      })

      output$custom_check <- renderCustomPlot({
        req(inequalities(), denominator(), input$indicator)
        plot(inequalities(), indicator = input$indicator, denominator = denominator())
      })

      downloadCoverageServer(
        id = 'measles1_download',
        data = inequalities,
        filename = reactive(paste0('measles1_', admin_level(), '_inequality_', denominator())),
        indicator = reactive('measles1'),
        denominator = denominator,
        data_fn = filter_inequality,
        i18n = i18n,
        sheet_name = reactive(i18n$t("title_mcv1_inequality"))
      )

      downloadCoverageServer(
        id = 'penta3_download',
        data = inequalities,
        filename = reactive(paste0('penta3_', admin_level(), '_inequality_', denominator())),
        indicator = reactive('penta3'),
        denominator =denominator,
        data_fn = filter_inequality,
        i18n = i18n,
        sheet_name = reactive(i18n$t("title_penta3_inequality"))
      )

      downloadCoverageServer(
        id = 'dropout_penta13_download',
        data = inequalities,
        filename = reactive(paste0('dropout_penta13_', admin_level(), '_inequality_', denominator())),
        indicator = reactive('dropout_penta13'),
        denominator = denominator,
        data_fn = filter_inequality,
        i18n = i18n,
        sheet_name = reactive(i18n$t("title_penta13_inequality"))
      )

      downloadCoverageServer(
        id = 'dropout_penta3mcv1_download',
        data = inequalities,
        filename = reactive(paste0('dropout_penta3mcv1_', admin_level(), '_inequality_', denominator())),
        indicator = reactive('dropout_penta3mcv1'),
        denominator = denominator,
        data_fn = filter_inequality,
        i18n = i18n,
        sheet_name = reactive(i18n$t("title_penta3_mcv1_inequality"))
      )

      downloadCoverageServer(
        id = 'custom_download',
        data = inequalities,
        filename = reactive(paste0(input$indicator, '_', admin_level(), '_inequality_', denominator())),
        indicator = reactive(input$indicator),
        denominator = denominator,
        data_fn = filter_inequality,
        i18n = i18n,
        sheet_name = reactive(paste0(input$indicator, ' Inequality'))
      )

      contentHeaderServer(
        'subnational_inequality',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = i18n$t("title_subnational_inequality"),
        md_file = '2_reporting_rate.md',
        i18n = i18n
      )
    }
  )
}
