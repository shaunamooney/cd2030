library(plotly)

reportingRateUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('reporting_rate'), 'Reporting Rate'),
    contentBody(
      box(
        title = 'Reporting Rate Options',
        status = 'success',
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          column(3, numericInput(ns('threshold'), label = 'Performance Threshold', value = 90)),
          column(3, selectizeInput(ns('admin_level'), label = 'Admin Level',
                                   choices = c('Admin Level 1' = 'adminlevel_1',
                                               'District' = 'district'))),
          column(3, selectizeInput(ns('indicator'),
                                   label = 'Indicator',
                                   choices = c('ANC' = 'anc_rr',
                                               'Institutional Delivery' = 'idelv_rr',
                                               'Vaccination' = 'vacc_rr')))
        )
      ),
      tabBox(
        title = 'Sub-National Reporting Rate',
        width = 12,

        tabPanel(title = 'Heat Map', fluidRow(
          column(12, withSpinner(plotlyOutput(ns('district_missing_heatmap'))))
        )),

        tabPanel(title = 'Bar Graph', fluidRow(
          column(12, plotCustomOutput(ns('district_missing_bar')))
        ))
      ),
      box(
        title = 'Average Reporting Rate',
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
        title = 'Subnational Units with Low Reporting',
        width = 6,
        status = 'success',
        fluidRow(
          column(3, selectizeInput(ns('year'),
                                   label = 'Year',
                                   choices =NULL)),
          column(3, offset = 6, downloadButtonUI(ns('download_districts'))),
          column(12, reactableOutput(ns('low_reporting')))
        )
      )
    )
  )
}

reportingRateServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      state <- reactiveValues(loaded = FALSE)

      data <- reactive({
        req(cache())
        cache()$countdown_data
      })

      threshold <- reactive({
        req(data())
        cache()$performance_threshold
      })

      reporting_rate <- reactive({
        req(data(), input$indicator, input$admin_level, threshold())

        data() %>%
          calculate_reporting_rate(input$indicator, input$admin_level, threshold())
      })

      district_rr <- reactive({
        req(data(), threshold())

        data() %>%
          calculate_district_reporting_rate(threshold())
      })

      average_rr <- reactive({
        req(data())

        data() %>%
          calculate_average_reporting_rate()
      })

      district_low_rr <- reactive({
        req(reporting_rate(), input$year)
        reporting_rate() %>%
          subnational_low_reporting(as.integer(input$year))
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
        req(reporting_rate())
        ggplotly(plot(reporting_rate()))
      })

      output$district_missing_bar <- renderCustomPlot({
        req(reporting_rate())
        plot(reporting_rate(), plot_type = 'bar')
      })

      output$district_report_plot <- renderCustomPlot({
        req(district_rr())
        plot(district_rr())
      })

      output$low_reporting <- renderReactable({
        req(district_low_rr())

        dt <- district_low_rr()

        cols <- grep('_rr', colnames(dt))

        district_low_rr() %>%
          reactable(
            filterable = FALSE,
            minRows = 10,
            # groupBy = input$admin_level,
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
        filename = 'district_rr_plot',
        data = district_rr,
        plot_function = function() {
          plot(district_rr())
        }
      )

      downloadExcel(
        id = 'download_data',
        filename = 'checks_reporting_rate',
        data = average_rr,
        excel_write_function = function(wb) {
          low_rr_national <- average_rr()
          district_rr_national <- district_rr()

          sheet_name_1 <- "Average RR by year"
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = "Table 1 - average reporting rates for all indicators, by year", startCol = 1, startRow = 1)
          writeData(wb, sheet = sheet_name_1, x = low_rr_national, startCol = 1, startRow = 3)

          # Check if sheet exists; if not, add it
          sheet_name_2 <- "RR >= threshold (districts)"
          addWorksheet(wb, sheet_name_2)
          writeData(wb, sheet = sheet_name_2, x = "Table 2 - Percentage of districts with reporting rates >= threshold, by year", startRow = 1, startCol = 1)
          writeData(wb, sheet = sheet_name_2, x = district_rr_national, startCol = 1, startRow = 3)
        }
      )

      downloadExcel(
        id = 'download_districts',
        filename = paste0('district_low_reporting_rate_', input$year),
        data = district_low_rr,
        excel_write_function = function(wb) {
          low_districts <- district_low_rr()

          sheet_name_1 <- "Districts Low Reporting Rating"
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = paste0('Table 3 - Districts with Reporting Rate < ', input$threshold, ' for', input$indicator, ' in ', input$year), startCol = 1, startRow = 1)
          writeData(wb, sheet = sheet_name_1, x = low_districts, startCol = 1, startRow = 3)
        }
      )

      contentHeaderServer(
        'reporting_rate',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = 'Reporting Rate',
        md_file = 'quality_checks_reporting_rate.md'
      )
    }
  )
}
