subnationalCoverageUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('subnational_coverage'), 'Subnational Coverage'),
    contentBody(
      box(
        title = 'Analysis Options',
        status = 'success',
        width = 12,
        solidHeader = TRUE,
        fluidRow(
          column(3, selectizeInput(ns('admin_level'), label ='Admin Level',
                                   choices = c('Admin Level 1' = 'adminlevel_1',
                                               'District' = 'district'))),
          column(3, selectizeInput(ns('region'), label ='Admin Level 1', choices = NULL)),
          column(3, selectizeInput(ns('denominator'), label = 'Denominator',
                                   choices = c('DHIS2' = 'dhis2',
                                               'ANC 1' = 'anc1',
                                               'Penta 1' = 'penta1'))),
          column(3, selectizeInput(ns('year'), label = 'Survey Start Year', choices = NULL))
        )
      ),

      tabBox(
        title = 'Subnational Coverage Trend',
        id = 'national_trend',
        width = 12,

        tabPanel(
          title = 'Measles 1',
          fluidRow(
            column(12, plotCustomOutput(ns('measles1'))),
            downloadCoverageUI(ns('measles1_download'))
          )
        ),

        tabPanel(
          title = 'Penta 3',
          fluidRow(
            column(12, plotCustomOutput(ns('penta3'))),
            downloadCoverageUI(ns('penta3_download'))
          )
        ),

        tabPanel(
          title = 'Penta 3 to Measles 1 Dropout',
          fluidRow(
            column(12, plotCustomOutput(ns('dropout_penta3mcv1'))),
            downloadCoverageUI(ns('dropout_penta3mcv1_download'))
          )
        ),

        tabPanel(
          title = 'Penta 1 to Penta 3 Droput',
          fluidRow(
            column(12, plotCustomOutput(ns('dropout_penta13'))),
            downloadCoverageUI(ns('dropout_penta13_download'))
          )
        ),

        tabPanel(
          'Custom Check',
          fluidRow(
            column(3, selectizeInput(ns('indicator'), label = 'Indicator',
                                     choices = c('Select' = '', "bcg", "anc1", "opv1", "opv2", "opv3", "pcv1", "pcv2", "pcv3",
                                                 "penta1", "penta2", "rota1", "rota2", "instdeliveries", "measles2",
                                                 "ipv1", "ipv2", "undervax", "dropout_penta13", "zerodose", "dropout_measles12",
                                                 "dropout_penta3mcv1")))
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

subnationalCoverageServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      data <- reactive({
        req(cache())
        cache()$get_adjusted_data()
      })

      survey_data <- reactive({
        req(cache())
        cache()$get_regional_survey()
      })

      filtered_survey_data <- reactive({
        req(survey_data())
        survey_data() %>%
          filter(year >= as.numeric(input$year))
      })

      wuenic_data <- reactive({
        req(cache())
        cache()$get_wuenic_estimates()
      })

      survey_mapping <- reactive({
        req(cache())
        cache()$get_survey_mapping()
      })

      indicator_coverage <- reactive({
        req(data(), input$admin_level)

        rates <- cache()$get_national_estimates()
        data() %>%
          calculate_indicator_coverage(
            admin_level = input$admin_level,
            sbr = rates$sbr,
            nmr = rates$nmr,
            pnmr = rates$pnmr,
            twin = rates$twin_rate,
            preg_loss = rates$preg_loss,
            anc1survey = rates$anc1,
            dpt1survey = rates$penta1)
      })


      observe({
        req(indicator_coverage())

        # Extract distinct values if column_name is valid
        admin_level <- indicator_coverage() %>%
          distinct(!!sym(input$admin_level)) %>%
          arrange(!!sym(input$admin_level)) %>%
          pull(!!sym(input$admin_level))

        # Update the select input
        updateSelectInput(
          session,
          'region',
          choices = admin_level,
          label = if (input$admin_level == 'adminlevel_1') 'Admin Level 1' else 'District'
        )
      })

      observe({
        req(survey_data())

        years <- survey_data() %>%
          distinct(year) %>%
          pull(year)

        updateSelectInput(session, 'year', choices = years)
      })

      measles1_coverage <- reactive({
        req(indicator_coverage(), input$denominator, input$region, wuenic_data(),
            filtered_survey_data())

        tryCatch(
        indicator_coverage() %>%
          analyze_coverage(
            admin_level = input$admin_level,
            indicator = 'measles1',
            denominator = input$denominator,
            survey_data = filtered_survey_data(),
            wuenic_data = wuenic_data(),
            subnational_map = survey_mapping(),
            region = input$region
          ),
        error = function(e) {
          print(clean_error_message(e))
        }
        )
      })

      penta3_coverage <- reactive({
        req(indicator_coverage(), input$denominator, input$region, wuenic_data(),
            filtered_survey_data())

        indicator_coverage() %>%
          analyze_coverage(
            admin_level = input$admin_level,
            indicator = 'penta3',
            denominator = input$denominator,
            survey_data = filtered_survey_data(),
            wuenic_data = wuenic_data(),
            subnational_map = survey_mapping(),
            region = input$region
          )
      })

      dropout_penta13_coverage <- reactive({
        req(indicator_coverage(), input$denominator, input$region, wuenic_data(),
            filtered_survey_data())

        indicator_coverage() %>%
          analyze_coverage(
            admin_level = input$admin_level,
            indicator = 'dropout_penta13',
            denominator = input$denominator,
            survey_data = filtered_survey_data(),
            wuenic_data = wuenic_data(),
            subnational_map = survey_mapping(),
            region = input$region
          )
      })

      dropout_penta3mcv1_coverage <- reactive({
        req(indicator_coverage(), input$denominator, input$region, wuenic_data(),
            filtered_survey_data())

        indicator_coverage() %>%
          analyze_coverage(
            admin_level = input$admin_level,
            indicator = 'dropout_penta3mcv1',
            denominator = input$denominator,
            survey_data = filtered_survey_data(),
            wuenic_data = wuenic_data(),
            subnational_map = survey_mapping(),
            region = input$region
          )
      })

      custom_coverage <- reactive({
        req(indicator_coverage(), input$denominator, input$region, wuenic_data(),
            filtered_survey_data(), input$indicator)

        indicator_coverage() %>%
          analyze_coverage(
            admin_level = input$admin_level,
            indicator = input$indicator,
            denominator = input$denominator,
            survey_data = filtered_survey_data(),
            wuenic_data = wuenic_data(),
            subnational_map = survey_mapping(),
            region = input$region
          )
      })

      output$measles1 <- renderCustomPlot({
        req(measles1_coverage())
        plot(measles1_coverage())
      })

      output$penta3 <- renderCustomPlot({
        req(penta3_coverage())
        plot(penta3_coverage())
      })

      output$dropout_penta13 <- renderCustomPlot({
        req(dropout_penta13_coverage())
        plot(dropout_penta13_coverage())
      })

      output$dropout_penta3mcv1 <- renderCustomPlot({
        req(dropout_penta3mcv1_coverage())
        plot(dropout_penta3mcv1_coverage())
      })

      output$custom_check <- renderCustomPlot({
        req(custom_coverage())
        plot(custom_coverage())
      })

      downloadCoverageServer(
        id = 'measles1_download',
        data_fn = measles1_coverage,
        filename = paste0('measles1_', input$region, '_survey_', input$denominator),
        sheet_name = 'Measles 1 Coverage'
      )

      downloadCoverageServer(
        id = 'penta3_download',
        data_fn = penta3_coverage,
        filename = paste0('penta3_', input$region, '_survey_', input$denominator),
        sheet_name = 'Penta 3 Coverage'
      )

      downloadCoverageServer(
        id = 'dropout_penta13_download',
        data_fn = dropout_penta13_coverage,
        filename = paste0('dropout_penta13_', input$region, '_survey_', input$denominator),
        sheet_name = 'Penta 1 to Penta 3 Dropout'
      )

      downloadCoverageServer(
        id = 'dropout_penta3mcv1_download',
        data_fn = dropout_penta3mcv1_coverage,
        filename = paste0('dropout_penta3mcv1_', input$region, '_survey_', input$denominator),
        sheet_name = 'Penta 3 to Measles 1 Dropout'
      )

      downloadCoverageServer(
        id = 'custom_download',
        data_fn = penta3_coverage,
        filename = paste0(input$indicator, '_', input$region, '_survey_', input$denominator),
        sheet_name = paste0(input$indicator, ' Coverage')
      )

      contentHeaderServer(
        'subnational_coverage',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = 'Subnational Coverage',
        md_file = '2_reporting_rate.md'
      )
    }
  )
}
