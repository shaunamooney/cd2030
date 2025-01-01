library(plotly)

reportingRateUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('reporting_rate'), 'Reporting Rate'),
    contentBody(
      box(
        title = 'Average Reporting Rate',
        status = 'success',
        collapsible = TRUE,
        width = 12,
        fluidRow(
          column(3, numericInput(ns('threshold'), label = 'Performance Threshold', value = 90)),
        ),
        fluidRow(
          column(12, plotCustomOutput(ns('district_report_plot'))),
          column(4, downloadButtonUI(ns('download_plot'))),
          column(4, downloadButtonUI(ns('download_data')))
        )
      ),
      box(
        title = 'Districts with low reporting rate',
        width = 6,
        status = 'success',
        fluidRow(
          column(3, selectizeInput(ns('indicator'),
                                   label = 'Indicator',
                                   choices = c('ANC' = 'anc_rr',
                                               'Institutional Delivery' = 'idelv_rr',
                                               'Vaccination' = 'vacc_rr'))),
          column(3, selectizeInput(ns('year'),
                                   label = 'Year',
                                   choices =NULL)),
          column(3, offset = 3, downloadButtonUI(ns('download_districts'))),
          column(12, DTOutput(ns('low_reporting')))
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

      data <- reactive({
        req(cache())
        cache()$get_data()
      })

      threshold <- reactive({
        req(cache())
        cache()$get_threshold()
      })

      district_rr <- reactive({
        req(data())

        data() %>%
          calculate_district_reporting_rate(input$threshold)
      })

      average_rr <- reactive({
        req(data())

        data() %>%
          calculate_average_reporting_rate()
      })

      district_low_rr <- reactive({
        req(data(), input$indicator, input$threshold, input$year)

        data() %>%
          district_low_reporting(input$threshold) %>%
          filter(if_any(input$indicator, ~ .x < input$threshold), year == input$year)
      })

      observeEvent(input$threshold, {
        req(cache())
        cache()$set_threshold(input$threshold)
      })

      observe({
        req(cache())
        updateNumericInput(session, 'threshold', value = cache()$get_threshold())
      })

      observe({
        req(data())

        years <- data() %>% distinct(year) %>% arrange(desc(year)) %>% pull(year)

        updateSelectizeInput(session, 'year', choices = years)
      })

      output$district_report_plot <- renderCustomPlot({
        req(district_rr())
        plot(district_rr())
      })

      output$low_reporting <- renderDT({
        req(district_low_rr())

        dt <- district_low_rr()

        cols <- grep('_rr', colnames(dt))

        district_low_rr() %>%
          datatable(options = list(pageLength = 10)) %>%
          formatRound(columns = cols, digits = 0)
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
        md_title = 'Reporting Rate',
        md_file = 'quality_checks_reporting_rate.md'
      )
    }
  )
}
