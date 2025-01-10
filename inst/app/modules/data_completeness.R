dataCompletenessUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('data_completeness'), 'Data Completeness'),
    contentBody(
      box(
        title = tags$span(icon('chart-line'), 'Indicators with Missing Data'),
        status = 'success',
        collapsible = TRUE,
        width = 12,
        fluidRow(
          column(3, selectizeInput(ns('year'), label = 'Year', choice = NULL)),
          column(3, selectizeInput(ns('indicator'), label = 'Indicator', choice = NULL)),
          column(12, withSpinner(plotlyOutput(ns('district_missing_heatmap')))),
          column(4, align = 'right', downloadButtonUI(ns('download_data')))
        )
      ),
      box(
        title = 'Districts with Missing Data',
        status = 'success',
        width = 6,
        fluidRow(
          column(4, downloadButtonUI(ns('download_incompletes')))
        ),
        fluidRow(
          column(12, reactableOutput(ns('incomplete_district')))
        )
      )
    )
  )
}

dataCompletenessServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      data <- reactive({
        req(cache())
        cache()$get_data()
      })

      vaccines_indicator <- reactive({
        req(cache())
        cache()$get_vaccine_indicators()
      })

      completeness_summary <- reactive({
        req(data())

        data() %>%
          select(district, year, month, vaccines_indicator()) %>%
          add_missing_column(vaccines_indicator())
      })

      incomplete_district <- reactive({
        req(completeness_summary(), input$indicator, input$year)

        mis_column <- paste0('mis_', input$indicator)
        selected_year <- as.numeric(input$year)

        completeness_summary() %>%
          filter(if (selected_year == 0) TRUE else year == selected_year, !!sym(mis_column) == 1) %>%
          select(district, year, month)
      })

      observe({
        req(data())

        vacc <- vaccines_indicator()
        updateSelectizeInput(session, 'indicator', choices = c('Select Indicator' = '', vacc))
      })

      observe({
        req(data())

        years <- data() %>%
          distinct(year) %>%
          arrange(desc(year)) %>%
          pull(year)

        updateSelectizeInput(session, 'year', choices = c('All Years' = 0, years))
      })

      output$incomplete_district <- renderReactable({
        req(completeness_summary())

        incomplete_district() %>%
          reactable(
            filterable = FALSE,
            minRows = 10,
            groupBy = c('district'),
            columns = list(
              year = colDef(
                aggregate = 'unique'
              ),
              month = colDef(
                aggregate = 'count',
                format = list(
                  aggregated = colFormat(suffix = ' month(s)')
                )
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

      output$district_missing_heatmap <- renderPlotly({
        req(completeness_summary())

        if (input$indicator == '' || is.null(input$indicator) || length(input$indicator) == 0) {
          selected_year <- as.numeric(input$year)

          dt <- completeness_summary() %>%
            filter(if (selected_year == 0) TRUE else year == selected_year) %>%
            summarise(across(starts_with('mis_'), ~ sum(.x, na.rm = TRUE)), .by = district) %>%
            pivot_longer(col = starts_with('mis_'), names_to = 'indicator') %>%
            mutate(indicator = str_remove(indicator, 'mis_'))

          ggplotly(
            ggplot(dt, aes(x = district, y = indicator, fill = value)) +
              geom_tile(color = 'white') +
              scale_fill_gradient2(low = 'forestgreen', mid = 'white', high = 'red3', midpoint = mean(dt$value, na.rm = TRUE)) +
              labs(title = NULL, x = 'Indicator', y = 'District', fill = 'Value') +
              theme_minimal() +
              theme(axis.text.x = element_text(angle = 45, size = 9, hjust = 1))
          )
        } else {
          dt <- completeness_summary() %>%
            select(district, year, any_of(paste0('mis_', input$indicator))) %>%
            summarise(
              value = sum(.data[[paste0('mis_', input$indicator)]], na.rm = TRUE),
              .by = c(district, year)
            ) %>%
            arrange(year)

          ggplotly(
            ggplot(dt, aes(x = district, y = year, fill = value)) +
              geom_tile(color = 'white') +
              scale_fill_gradient2(low = 'forestgreen', mid = 'white', high = 'red3', midpoint = mean(dt$value, na.rm = TRUE)) +
              labs(title = NULL, x = 'Indicator', y = 'District', fill = 'Value') +
              theme_minimal() +
              theme(axis.text.x = element_text(angle = 45, size = 9, hjust = 1))
          )
        }
      })

      downloadExcel(
        id = 'download_data',
        filename = 'checks_reporting_rate',
        data = data,
        excel_write_function = function(wb) {
          completeness_rate <- data() %>% calculate_completeness_summary()
          district_completeness_rate <- data() %>% calculate_district_completeness_summary()

          sheet_name_1 <- "Missings"
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = "Table 4a: Percentage of monthly values with complete data, by year", startCol = 1, startRow = 1)
          writeData(wb, sheet = sheet_name_1, x = completeness_rate, startCol = 1, startRow = 3)

          # Check if sheet exists; if not, add it
          sheet_name_2 <- "Missings (districts)"
          addWorksheet(wb, sheet_name_2)
          writeData(wb, sheet = sheet_name_2, x = "Table 4a: Percentage of Districts with complete data, by year", startRow = 1, startCol = 1)
          writeData(wb, sheet = sheet_name_2, x = district_completeness_rate, startCol = 1, startRow = 3)
        },
        label = 'Download Districts'
      )

      downloadExcel(
        id = 'download_incompletes',
        filename = paste0('checks_incomplete_districts_', input$indicator, '_', input$year),
        data = incomplete_district,
        excel_write_function = function(wb) {
          district_incompletes_sum <- incomplete_district()

          sheet_name_1 <- "Districts with Extreme Outliers"
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = paste0('Districts with incomplete data for ', input$indicator, ' in ', input$year), startCol = 1, startRow = 1)
          writeData(wb, sheet = sheet_name_1, x = district_incompletes_sum, startCol = 1, startRow = 3)
        },
        label = 'Download Districts'
      )

      contentHeaderServer(
        'data_completeness',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = 'Data Completeness',
        md_file = 'quality_checks_data_completeness.md'
      )
    }
  )
}
