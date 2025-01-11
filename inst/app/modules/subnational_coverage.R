subnationalCoverageUI <- function(id) {
  ns <- NS(id)

  tagList(contentHeader(ns('subnational_coverage'), 'Subnational Coverage'),
          contentBody(
            box(
              title = 'Analysis Options',
              status = 'success',
              width = 12,
              solidHeader = TRUE,
              fluidRow(
                column(3, selectizeInput(
                  ns('admin_level'),
                  label = 'Admin Level',
                  choices = c('Admin Level 1' = 'adminlevel_1', 'District' = 'district')
                )),
                column(3, selectizeInput(
                  ns('region'), label = 'Admin Level 1', choices = NULL
                )),
                column(3, selectizeInput(
                  ns('denominator'),
                  label = 'Denominator',
                  choices = c(
                    'DHIS2' = 'dhis2',
                    'ANC 1' = 'anc1',
                    'Penta 1' = 'penta1'
                  )
                )),
                column(
                  3,
                  selectizeInput(ns('year'), label = 'Survey Start Year', choices = NULL)
                )
              )
            ),

            tabBox(
              title = 'Subnational Coverage Trend',
              id = 'national_trend',
              width = 12,

              tabPanel(title = 'Measles 1', fluidRow(
                column(12, plotCustomOutput(ns('measles1'))), downloadCoverageUI(ns('measles1_download'))
              )),

              tabPanel(title = 'Penta 3', fluidRow(
                column(12, plotCustomOutput(ns('penta3'))), downloadCoverageUI(ns('penta3_download'))
              )),

              tabPanel(title = 'Penta 3 to Measles 1 Dropout', fluidRow(
                column(12, plotCustomOutput(ns(
                  'dropout_penta3mcv1'
                ))), downloadCoverageUI(ns('dropout_penta3mcv1_download'))
              )),

              tabPanel(title = 'Penta 1 to Penta 3 Droput', fluidRow(
                column(12, plotCustomOutput(ns(
                  'dropout_penta13'
                ))), downloadCoverageUI(ns('dropout_penta13_download'))
              )),

              tabPanel(
                'Custom Check',
                fluidRow(column(
                  3, selectizeInput(
                    ns('indicator'),
                    label = 'Indicator',
                    choices = c(
                      'Select' = '', "bcg", "anc1", "opv1", "opv2", "opv3", "pcv1",
                      "pcv2", "pcv3", "penta1", "penta2", "rota1", "rota2",
                      "instdeliveries", "measles2", "ipv1", "ipv2", "undervax",
                      "dropout_penta13", "zerodose", "dropout_measles12",
                      "dropout_penta3mcv1"
                    )
                  )
                )),
                fluidRow(column(12, plotCustomOutput(
                  ns('custom_check')
                )), downloadCoverageUI(ns(
                  'custom_download'
                )))
              )
            )
          ))
}

subnationalCoverageServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(id = id, module = function(input, output, session) {

    survey_data <- reactive({
      req(cache())
      cache()$get_regional_survey()
    })

    coverage <- reactive({
      req(cache(), cache()$get_un_estimates(), cache()$get_wuenic_estimates(),
        survey_data())

      rates <- cache()$get_national_estimates()
      filtered_survey_data <- survey_data() %>%
        filter(year >= as.numeric(input$year))

      cache()$get_adjusted_data() %>%
        calculate_indicator_coverage(
          admin_level = input$admin_level,
          sbr = rates$sbr,
          nmr = rates$nmr,
          pnmr = rates$pnmr,
          twin = rates$twin_rate,
          preg_loss = rates$preg_loss,
          anc1survey = rates$anc1,
          dpt1survey = rates$penta1
        ) %>%
        combine_coverage(
            survey_data = filtered_survey_data,
            wuenic_data = cache()$get_wuenic_estimates(),
            subnational_map = cache()$get_survey_mapping()
          )
      })

      denominator <- reactive({
        req(cache())
        cache()$get_denominator()
      })

      observe({
        req(cache())

        data <- cache()$get_data()

        # Extract distinct values if column_name is valid
        admin_level <- data %>%
          distinct(!!sym(input$admin_level)) %>%
          arrange(!!sym(input$admin_level)) %>%
          pull(!!sym(input$admin_level))

        selected_region <- if (input$admin_level == 'adminlevel_1') {
          admin_level_1 <- cache()$get_admin_level_1()
          if (is.null(admin_level_1)) {
            admin_level[1]
          } else {
            admin_level_1
          }
        } else {
          district <- cache()$get_district()
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
          label = if (input$admin_level == 'adminlevel_1') 'Admin Level 1' else 'District'
        )
      })

      observeEvent(input$region, {
        req(cache())
        if (input$admin_level == 'adminlevel_1') {
          cache()$set_admin_level_1(input$region)
        } else {
          cache()$set_district(input$region)
        }
      })

      observe({
        req(survey_data())

        years <- survey_data() %>%
          distinct(year) %>%
          pull(year)

        selected_year <- cache()$get_start_survey_year()

        updateSelectInput(session, 'year', choices = years, selected = selected_year)
      })

      observe({
        req(cache())
        updateSelectInput(session, 'denominator', selected = denominator())
      })

      observeEvent(input$year, {
        req(cache())
        cache()$set_start_survey_year(as.numeric(input$year))
      })

      observeEvent(input$denominator, {
        req(cache())
        cache()$set_denominator(input$denominator)
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
        req(coverage(), denominator(), input$region)
        plot(coverage(), indicator = input$indicator, denominator = denominator(), region = input$region)
      })

      downloadCoverageServer(
        id = 'measles1_download',
        data_fn = coverage,
        filename = paste0('measles1_', input$region, '_survey_', denominator()),
        sheet_name = 'Measles 1 Coverage'
      )

      downloadCoverageServer(
        id = 'penta3_download',
        data_fn = coverage,
        filename = paste0('penta3_', input$region, '_survey_', denominator()),
        sheet_name = 'Penta 3 Coverage'
      )

      downloadCoverageServer(
        id = 'dropout_penta13_download',
        data_fn = coverage,
        filename = paste0('dropout_penta13_', input$region, '_survey_', denominator()),
        sheet_name = 'Penta 1 to Penta 3 Dropout'
      )

      downloadCoverageServer(
        id = 'dropout_penta3mcv1_download',
        data_fn = coverage,
        filename = paste0('dropout_penta3mcv1_', input$region, '_survey_', denominator()),
        sheet_name = 'Penta 3 to Measles 1 Dropout'
      )

      downloadCoverageServer(
        id = 'custom_download',
        data_fn = coverage,
        filename = paste0(input$indicator, '_', input$region, '_survey_', denominator()),
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
