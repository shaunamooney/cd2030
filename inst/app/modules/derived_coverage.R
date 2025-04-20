derivedCoverageUI <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    contentHeader(ns('derived_coverage'), i18n$t("title_derived_coverage"), i18n = i18n),
    contentBody(
      box(
        title = i18n$t("title_analysis_options"),
        status = 'success',
        width = 12,
        solidHeader = TRUE,
        fluidRow(
          column(3, adminLevelInputUI(ns('admin_level'), i18n, include_national = TRUE)),
          conditionalPanel(
            condition = str_glue("input['{ns('admin_level')}'] != 'national'"),
            column(3, selectizeInput(
              ns('region'), label = i18n$t("opt_admin_level_1"), choices = NULL
            ))
          )
        )
      ),
      tabBox(
        title = i18n$t("title_derived_coverage"),
        width = 12,
        tabPanel(
          title = i18n$t("opt_penta1"),
          fluidRow(
            column(12, plotCustomOutput(ns('penta1'))),
            column(12, tagList(
              column(4, downloadButtonUI(ns('penta1_plot'))),
              column(4, downloadButtonUI(ns('penta1_data')))
            ))
          )
        ),

        tabPanel(
          title = i18n$t("opt_penta3"),
          fluidRow(
            column(12, plotCustomOutput(ns('penta3'))),
            column(12, tagList(
              column(4, downloadButtonUI(ns('penta3_plot'))),
              column(4, downloadButtonUI(ns('penta3_data')))
            ))
          )
        ),

        tabPanel(
          title = i18n$t("opt_measles"),
          fluidRow(
            column(12, plotCustomOutput(ns('measles1'))),
            column(12, tagList(
              column(4, downloadButtonUI(ns('measles1_plot'))),
              column(4, downloadButtonUI(ns('measles1_data')))
            ))
          )
        ),

        tabPanel(
          i18n$t("opt_custom_check"),
          fluidRow(
            column(3, selectizeInput(ns('indicator'), label = i18n$t("title_indicator"),
                                     choices = c('Select' = '', get_all_indicators())))
          ),
          fluidRow(
            column(12, plotCustomOutput(ns('custom'))),
            column(12, tagList(
              column(4, downloadButtonUI(ns('custom_plot'))),
              column(4, downloadButtonUI(ns('custom_data')))
            ))
          )
        )
      )
    )
  )
}

derivedCoverageServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      data <- reactive({
        req(cache())
        cache()$adjusted_data
      })

      admin_level <- adminLevelInputServer('admin_level')

      un_estimates <- reactive({
        req(cache())
        cache()$un_estimates
      })

      populations <- reactive({
        req(data(), un_estimates(), admin_level())
        calculate_populations(data(), un_estimates = un_estimates(), admin_level = admin_level())
      })

      region <- reactive({
        region <- if (admin_level() == 'national') {
          NULL
        } else {
          input$region
        }
      })

      penta1_data <- reactive({
        req(populations())
        calculate_derived_coverage(populations(), 'penta1', 2021)
      })

      penta3_data <- reactive({
        req(populations())
        calculate_derived_coverage(populations(), 'penta3', 2021)
      })

      measles1_data <- reactive({
        req(populations())
        calculate_derived_coverage(populations(), 'measles1', 2021)
      })

      custom_data <- reactive({
        req(populations(), input$indicator)
        calculate_derived_coverage(populations(), input$indicator, 2021)
      })

      observeEvent(admin_level(), {
        req(admin_level() %in% c('adminlevel_1', 'district'), populations())

        choices <- populations() %>%
          distinct(!!sym(admin_level())) %>%
          arrange(!!sym(admin_level())) %>%
          pull(!!sym(admin_level()))

        label <- if (admin_level() == 'adminlevel_1') 'Admin Level 1' else 'District'

        updateSelectizeInput(session, 'region', choices = choices, label = label)
      })

      output$penta1 <- renderCustomPlot({
        req(penta1_data())
        plot(penta1_data(), region = region())
      })

      output$penta3 <- renderCustomPlot({
        req(penta3_data())
        plot(penta3_data(), region = region())
      })

      output$measles1 <- renderCustomPlot({
        req(measles1_data())
        plot(measles1_data(), region = region())
      })

      output$custom <- renderCustomPlot({
        req(custom_data())
        plot(custom_data(), region = region())
      })

      downloadPlot(
        id = 'penta1_plot',
        filename = 'penta1_derived_coverage',
        data = penta1_data,
        i18n = i18n,
        plot_function = function() plot(penta1_data(), region = region())
      )

      downloadExcel(
        id = 'penta1_data',
        filename = 'penta1_derived_coverage',
        data = penta3_data,
        i18n = i18n,
        excel_write_function = function(wb) {
          sheet_name_1 <- i18n$t('shhet_penta1_derived_coverage')
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, penta3_data(), startCol = 1, startRow = 1)
        }
      )

      downloadPlot(
        id = 'penta3_plot',
        filename = 'penta3_derived_coverage',
        data = penta3_data,
        i18n = i18n,
        plot_function = function() plot(penta3_data(), region = region())
      )

      downloadExcel(
        id = 'penta3_data',
        filename = 'penta3_derived_coverage',
        data = penta3_data,
        i18n = i18n,
        excel_write_function = function(wb) {
          sheet_name_1 <- i18n$t('sheet_penta3_derived_coverage')
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, penta3_data(), startCol = 1, startRow = 1)
        }
      )

      downloadPlot(
        id = 'measles1_plot',
        filename = 'measles1_derived_coverage',
        data = measles1_data,
        i18n = i18n,
        plot_function = function() plot(measles1_data(), region = region())
      )

      downloadExcel(
        id = 'measles1_data',
        filename = 'measles1_derived_coverage',
        data = measles1_data,
        i18n = i18n,
        excel_write_function = function(wb) {
          sheet_name_1 <- i18n$t('sheet_measles1_derived_coverage')
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, measles1_data(), startCol = 1, startRow = 1)
        }
      )

      downloadPlot(
        id = 'custom_plot',
        filename = paste0(input$indicator, '_derived_coverage'),
        data = custom_data,
        i18n = i18n,
        plot_function = function() plot(custom_data(), region = region())
      )

      downloadExcel(
        id = 'custom_data',
        filename = paste0(input$indicator, '_derived_coverage'),
        data = custom_data,
        i18n = i18n,
        excel_write_function = function(wb) {
          sheet_name_1 <- str_glue_data(list(indicator = input$indicator), i18n$t('sheet_custom_derived_coverage'))
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, custom_data(), startCol = 1, startRow = 1)
        }
      )

      contentHeaderServer(
        'derived_coverage',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = i18n$t("title_derived_coverage"),
        md_file = '2_reporting_rate.md',
        i18n = i18n
      )

    }
  )
}
