reportingRateUI <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('reporting_rate'), i18n$t("title_reporting"), i18n = i18n),
    contentBody(
      box(
        title = i18n$t("title_reporting_rate_options"),
        status = 'success',
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          column(3, numericInput(ns('threshold'), label = i18n$t("title_performance_threshold"), value = 90)),
          column(3, adminLevelInputUI(ns('admin_level'), i18n)),
          # TODO: to include translation
          column(3, selectInput(ns('indicator'),
                                   label = i18n$t("title_indicator"),
                                   choices = c('ANC' = 'anc_rr',
                                               'Institutional Delivery' = 'idelv_rr',
                                               'Vaccination' = 'vacc_rr')))
        )
      ),
      tabBox(
        title = i18n$t("title_subnational_reporting_rate"),
        width = 12,

        tabPanel(title = i18n$t("title_heat_map"), fluidRow(
          column(12, withSpinner(plotlyOutput(ns('district_missing_heatmap'))))
        )),

        tabPanel(title = i18n$t("title_bar_graph"), fluidRow(
          column(12, plotCustomOutput(ns('district_missing_bar')))
        ))
      ),
      box(
        title = i18n$t("title_national_reporting_rate"),
        status = 'success',
        collapsible = TRUE,
        width = 6,
        fluidRow(
          column(12, plotCustomOutput(ns('district_report_plot'))),
          column(4, downloadButtonUI(ns('download_plot'))),
          column(4, downloadButtonUI(ns('download_data')))
        )
      ),
      box(
        title = i18n$t("title_subnational_low_reporting"),
        width = 6,
        status = 'success',
        fluidRow(
          column(3, selectizeInput(ns('year'),
                                   label = i18n$t("title_year"),
                                   choices =NULL)),
          column(3, offset = 6, downloadButtonUI(ns('download_districts'))),
          column(12, withSpinner(reactableOutput(ns('low_reporting'))))
        )
      )
    )
  )
}

reportingRateServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      state <- reactiveValues(loaded = FALSE)
      admin_level <- adminLevelInputServer('admin_level')

      data <- reactive({
        req(cache())
        cache()$countdown_data
      })

      threshold <- reactive({
        req(data())
        cache()$performance_threshold
      })

      national_rr <- reactive({
        req(data())

        data() %>%
          calculate_reporting_rate()
      })

      subnational_rr <- reactive({
        req(data(), input$indicator, admin_level(), threshold())

        data() %>%
          calculate_reporting_rate(admin_level()) %>%
          select(any_of(c('adminlevel_1', 'district', 'year', input$indicator)))
      })

      district_rr <- reactive({
        req(data(), threshold())

        data() %>%
          calculate_district_reporting_rate(threshold())
      })

      district_low_rr <- reactive({
        req(subnational_rr(), input$year, input$indicator)

        subnational_rr() %>%
          filter(year == as.integer(input$year), !!sym(input$indicator) < threshold())
      })

      observeEvent(data(), {
        req(data())
        state$loaded <- FALSE
      })

      observeEvent(input$threshold, {
        req(cache())
        cache()$set_performance_threshold(input$threshold)
      })

      observe({
        req(data(), !state$loaded)
        updateNumericInput(session, 'threshold', value = threshold())
        state$loaded <- TRUE
      })

      observe({
        req(data())

        years <- data() %>%
          distinct(year) %>%
          arrange(desc(year)) %>%
          pull(year)

        updateSelectizeInput(session, 'year', choices = years)
      })

      output$district_missing_heatmap <- renderPlotly({
        req(subnational_rr(), input$indicator, threshold())
        ggplotly(plot(subnational_rr(),
                      plot_type = 'heat_map',
                      indicator = input$indicator,
                      threshold = threshold()))
      })

      output$district_missing_bar <- renderCustomPlot({
        req(subnational_rr(), input$indicator, threshold())
        plot(subnational_rr(),
             plot_type = 'bar',
             indicator = input$indicator,
             threshold = threshold())
      })

      output$district_report_plot <- renderCustomPlot({
        req(district_rr())
        plot(district_rr())
      })

      output$low_reporting <- renderReactable({
        req(district_low_rr())

        district_low_rr() %>%
          reactable(
            filterable = FALSE,
            minRows = 10,
            columns = list(
              year = colDef(
                aggregate = 'unique'
              )
            ),
            defaultColDef = colDef(
              cell = function(value) {
                if (!is.numeric(value)) {
                  return(value)
                }
                format(round(value), nsmall = 0)
              }
            )
          )
      })

      downloadPlot(
        id = 'download_plot',
        filename = reactive('district_rr_plot'),
        data = district_rr,
        i18n = i18n,
        plot_function = function() {
          plot(district_rr())
        }
      )

      downloadExcel(
        id = 'download_data',
        filename = reactive('checks_reporting_rate'),
        data = national_rr,
        i18n = i18n,
        excel_write_function = function(wb) {
          low_rr_national <- national_rr()
          district_rr_national <- district_rr()

          sheet_name_1 <- i18n$t("title_average_rr")
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = i18n$t("table_reporting_rate"), startCol = 1, startRow = 1)
          writeData(wb, sheet = sheet_name_1, x = low_rr_national, startCol = 1, startRow = 3)

          # Check if sheet exists; if not, add it
          sheet_name_2 <- str_glue(i18n$t("sheet_reporting_district"))
          addWorksheet(wb, sheet_name_2)
          writeData(wb, sheet = sheet_name_2, x = str_glue(i18n$t("table_district_reporting")), startRow = 1, startCol = 1)
          writeData(wb, sheet = sheet_name_2, x = district_rr_national, startCol = 1, startRow = 3)
        }
      )

      downloadExcel(
        id = 'download_districts',
        filename = reactive(paste0('district_low_reporting_rate_', input$year)),
        data = district_low_rr,
        i18n = i18n,
        excel_write_function = function(wb) {
          low_districts <- district_low_rr()

          sheet_name_1 <- i18n$t("title_districts_low_reporting")
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = str_glue(i18n$t("table_district_reporting_year")), startCol = 1, startRow = 1)
          writeData(wb, sheet = sheet_name_1, x = low_districts, startCol = 1, startRow = 3)
        }
      )

      contentHeaderServer(
        'reporting_rate',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = i18n$t("title_reporting"),
        md_file = 'quality_checks_reporting_rate.md',
        i18n = i18n
      )
    }
  )
}
