library(plotly)

reportingRateUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('reporting_rate'), 'Reporting Rate'),
    contentBody(
      box(
        title = 'District Reporting Rate',
        status = 'success',
        collapsible = TRUE,
        width = 12,
        fluidRow(
          column(3, numericInput(ns('threshold'), label = 'Performance Threshold', value = 90)),
          column(3, selectizeInput(ns('indicator'),
                                   label = 'Indicator',
                                   choices = c('ANC' = 'anc_rr',
                                               'Institutional Delivery' = 'idelv_rr',
                                               'Vaccination' = 'vacc_rr'))),
          column(12, withSpinner(plotlyOutput(ns('district_missing_heatmap'))))
        )
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
        title = 'Districts with low reporting rate',
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
        req(data(), input$indicator, threshold(), input$year)

        data() %>%
          district_low_reporting(threshold()) %>%
          filter(year == input$year)
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
        req(data())

        indicator_groups <- attr(data(), 'indicator_groups')
        indicator_groups <- paste0(names(indicator_groups), '_rr')

        greater <- paste0('>= ', threshold())
        mid <- paste0(' >= 40 and < ', threshold())
        low <- '< 40'

        dt <- data() %>%
          select(district, year, any_of(input$indicator)) %>%
          summarise(
            value = mean(.data[[input$indicator]], na.rm = TRUE),
            .by = c(district, year)
          ) %>%
          mutate(
            color_category = case_when(
              round(value, 0) >= threshold() ~ greater,
              round(value, 0) >= 40 & round(value, 0) < threshold() ~ mid,
              round(value, 0) < 40 ~ low,
              .ptype = factor(levels = c(low, mid, greater))
            )
          )

        ggplotly(
          ggplot(dt, aes(x = district, y = year, fill = color_category)) +
            geom_tile(color = 'white') +
            scale_fill_manual(
              values = set_names(
                c("forestgreen", "orange", "red"),
                c(greater, mid, low)
              ),
              name = "Value Category",
              drop = FALSE
            ) +
            labs(title = NULL, x = 'District', y = 'Year', fill = 'Value') +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, size = 9, hjust = 1))
        )
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
            groupBy = c('district'),
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
