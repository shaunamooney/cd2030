nationalCoverageUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('national_coverage'), 'National Coverage'),
    contentBody(
      box(
        title = 'Analysis Options',
        status = 'success',
        width = 12,
        solidHeader = TRUE,
        fluidRow(
          column(3, selectizeInput(ns('denominator'), label = 'Denominator',
                                   choices = c('DHIS2' = 'dhis2',
                                               'ANC 1' = 'anc1',
                                               'Penta 1' = 'penta1'))),
          column(3, selectizeInput(ns('year'), label = 'Survey Start Year', choices = NULL))
        )
      ),

      tabBox(
        title = 'National Coverage Trend',
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
                                     choices = c('Select' = '', "bcg", "anc1", "opv1", "opv2", "opv3", "penta2", "pcv1", "pcv2", "pcv3",
                                                 "penta1", "penta2",  "rota1", "rota2", "instdeliveries", "measles2",
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

nationalCoverageServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      data <- reactive({
        req(cache(), cache()$get_un_estimates())

        rates <- cache()$get_national_estimates()
        cache()$get_adjusted_data() %>%
          calculate_indicator_coverage(
            un_estimates = cache()$get_un_estimates(),
            sbr = rates$sbr,
            nmr = rates$nmr,
            pnmr = rates$pnmr,
            twin = rates$twin_rate,
            preg_loss = rates$preg_loss,
            anc1survey = rates$anc1,
            dpt1survey = rates$penta1)
      })

      wuenic_data <- reactive({
        req(cache())
        cache()$get_wuenic_estimates()
      })

      survey_data <- reactive({
        req(cache())
        cache()$get_national_survey()
      })

      filtered_survey_data <- reactive({
        req(survey_data())

        survey_data() %>%
          filter(year >= as.numeric(input$year))
      })

      observe({
        req(survey_data())

        years <- survey_data() %>%
          distinct(year) %>%
          arrange(year) %>%
          pull(year)

        selected_year <- cache()$get_start_survey_year()

        updateSelectInput(session, 'year', choices = years, selected = selected_year)
      })

      observe({
        req(cache())

        selected_denoninator <- cache()$get_denominator()
        updateSelectInput(session, 'denominator', selected = selected_denoninator)
      })

      observeEvent(input$year, {
        req(cache())
        cache()$set_start_survey_year(as.numeric(input$year))
      })

      observeEvent(input$denominator, {
        req(cache())
        cache()$set_denominator(input$denominator)
      })

      measles1_coverage <- reactive({
        req(data(), input$denominator, wuenic_data(), filtered_survey_data())

        data() %>%
          analyze_coverage(
            indicator = 'measles1',
            denominator = input$denominator,
            survey_data = filtered_survey_data(),
            wuenic_data = wuenic_data()
          )
      })

      penta3_coverage <- reactive({
        req(data(), input$denominator, wuenic_data(), filtered_survey_data())

        data() %>%
          analyze_coverage(
            indicator = 'penta3',
            denominator = input$denominator,
            survey_data = filtered_survey_data(),
            wuenic_data = wuenic_data()
          )
      })

      dropout_penta13_coverage <- reactive({
        req(data(), input$denominator, wuenic_data(), filtered_survey_data())

        data() %>%
          analyze_coverage(
            indicator = 'dropout_penta13',
            denominator = input$denominator,
            survey_data = filtered_survey_data(),
            wuenic_data = wuenic_data()
          )
      })

      dropout_penta3mcv1_coverage <- reactive({
        req(data(), input$denominator, wuenic_data(), filtered_survey_data())

        data() %>%
          analyze_coverage(
            indicator = 'dropout_penta3mcv1',
            denominator = input$denominator,
            survey_data = filtered_survey_data(),
            wuenic_data = wuenic_data()
          )
      })

      custom_coverage <- reactive({
        req(input$indicator, input$denominator, wuenic_data(), filtered_survey_data())

        data() %>%
          analyze_coverage(
            indicator = input$indicator,
            denominator = input$denominator,
            survey_data = filtered_survey_data(),
            wuenic_data = wuenic_data()
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
        filename = paste0('measles1_survey_', input$denominator),
        sheet_name = 'Measles 1 Coverage'
      )

      downloadCoverageServer(
        id = 'penta3_download',
        data_fn = measles1_coverage,
        filename = paste0('penta3_survey_', input$denominator),
        sheet_name = 'Penta 3 Coverage'
      )

      downloadCoverageServer(
        id = 'dropout_penta13_download',
        data_fn = dropout_penta13_coverage,
        filename = paste0('dropout_penta13_survey_', input$denominator),
        sheet_name = 'Penta 1 to Penta 3 Dropout'
      )

      downloadCoverageServer(
        id = 'dropout_penta3mcv1_download',
        data_fn = dropout_penta3mcv1_coverage,
        filename = paste0('dropout_penta3mcv1_survey_', input$denominator),
        sheet_name = 'Penta 3 to Measles 1 Dropout'
      )

      downloadCoverageServer(
        id = 'custom_download',
        data_fn = measles1_coverage,
        filename = paste0(input$indicator, '_survey_', input$denominator),
        sheet_name = paste0(input$indicator, ' Coverage')
      )

      contentHeaderServer(
        'national_coverage',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = 'National Coverage',
        md_file = '2_reporting_rate.md'
      )
    }
  )
}
