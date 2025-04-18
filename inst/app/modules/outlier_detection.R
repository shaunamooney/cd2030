outlierDetectionUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('outlier_detection'), 'Outlier Detection'),
    contentBody(
      box(
        title = 'Outlier Options',
        status = 'success',
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          column(3, selectizeInput(ns('year'), label = 'Year', choice = NULL)),
          column(3, selectizeInput(ns('admin_level'), label = 'Admin Level',
                                   choice = c('Admin Level 1' = 'adminlevel_1',
                                              'District' = 'district'))),
          column(3, selectizeInput(ns('indicator'), label = 'Indicator', choice = NULL))
        )
      ),
      tabBox(
        title = tags$span(icon('chart-line'), 'Indicators with Outliers'),
        width = 12,

        tabPanel(title = 'Heat Map', fluidRow(
          column(12, withSpinner(plotlyOutput(ns('district_outlier_heatmap')))),
          column(4, downloadButtonUI(ns('download_data')))
        )),

        tabPanel(title = 'Vaccine Bar Graph', fluidRow(
          column(12, plotCustomOutput(ns('vaccine_bar_graph')))
        )),

        tabPanel(title = 'Region Bar Graph', fluidRow(
          column(12, plotCustomOutput(ns('region_bar_graph')))
        ))
      ),
      box(
        title = 'Districts with Outliers',
        status = 'success',
        collapsible = TRUE,
        width = 6,
        fluidRow(
          column(4, downloadButtonUI(ns('download_outliers'))),
          column(12, reactableOutput(ns('district_outlier_summary')))
        )
      ),
      box(
        title = 'Districts Trends',
        status = 'success',
        collapsible = TRUE,
        width = 6,
        fluidRow(
          column(6, selectizeInput(ns('district'), label = 'District', choice = NULL)),
          column(12, withSpinner(plotlyOutput(ns('district_trend'))))
        )
      )
    )
  )
}

outlierDetectionServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      data <- reactive({
        req(cache())
        cache()$countdown_data
      })

      vaccines_indicator <- reactive({
        req(cache())
        cache()$vaccine_indicators
      })

      outlier_summary <- reactive({
        req(data(), input$admin_level)

        data() %>%
          outliers_summary(input$admin_level)
      })

      region_cols <- reactive({
        req(input$admin_level)

        switch (
          input$admin_level,
          adminlevel_1 = 'adminlevel_1',
          district = c('adminlevel_1', 'district')
        )
      })

      outlier_summary_filtered <- reactive({
        req(outlier_summary(), region_cols)

        outlier_summary() %>%
          summarise(
            across(starts_with(input$indicator), ~ mean(.x, na.rm = TRUE)),
            .by = c(region_cols(), year, month)
          )
      })

      outlier_districts <- reactive({
        req(outlier_summary(), input$indicator, region_cols(), input$year)

        outlier_column <- paste0(input$indicator, '_outlier5std')
        selected_year <- as.numeric(input$year)

        outlier_summary_filtered() %>%
          filter(if (selected_year == 0) TRUE else year == selected_year, !!sym(outlier_column) == 1) %>%
          select(any_of(region_cols()), year, month, any_of(c(input$indicator, paste0(input$indicator, '_med'), paste0(input$indicator, '_mad'))))
      })

      observe({
        req(data())

        vaccs <- vaccines_indicator()
        updateSelectizeInput(session, 'indicator', choices = c('Select Indicator' = '', vaccs))
      })

      observe({
        req(data())

        years <- data() %>%
          distinct(year) %>%
          arrange(desc(year)) %>%
          pull(year)

        updateSelectizeInput(session, 'year', choices = c('All years' = 0, years))
      })

      observe({
        req(data(), input$admin_level)


        region <- data() %>%
          distinct(!!sym(input$admin_level)) %>%
          arrange(!!sym(input$admin_level)) %>%
          pull(!!sym(input$admin_level))

        updateSelectizeInput(session, 'district', choices = c('Select' = '', region))
      })


      output$district_outlier_summary <- renderReactable({
        req(outlier_districts())

        dt <- outlier_districts()

        outlier_districts() %>%
          reactable(
            filterable = FALSE,
            minRows = 10,
            groupBy = 'adminlevel_1',
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

      output$district_outlier_heatmap <- renderPlotly({
        req(outlier_summary(), region_cols())

        if (input$indicator == '' || is.null(input$indicator) || length(input$indicator) == 0) {
          selected_year <- as.numeric(input$year)

          dt <- outlier_summary() %>%
            filter(if (selected_year == 0) TRUE else year == selected_year) %>%
            summarise(across(ends_with('_outlier5std'), ~ sum(.x, na.rm = TRUE)), .by = region_cols()) %>%
            pivot_longer(col = ends_with('_outlier5std'), names_to = 'indicator') %>%
            mutate(indicator = str_remove(indicator, '_outlier5std'))

          ggplotly(
            ggplot(dt, aes(x = !!sym(input$admin_level), y = indicator, fill = value)) +
              geom_tile(color = 'white') +
              scale_fill_gradient2(low = 'forestgreen', mid = 'white', high = 'red3', midpoint = mean(dt$value, na.rm = TRUE)) +
              labs(title = NULL, y = 'Indicator', x = input$admin_level, fill = 'Value') +
              theme_minimal() +
              theme(axis.text.x = element_text(angle = 45, size = 9, hjust = 1))
          )
        } else {
          dt <- outlier_summary() %>%
            select(any_of(region_cols()), year, any_of(paste0(input$indicator, '_outlier5std'))) %>%
            summarise(
              value = sum(.data[[paste0(input$indicator, '_outlier5std')]], na.rm = TRUE),
              .by = c(region_cols(), year)
            ) %>%
            arrange(year)

          ggplotly(
            ggplot(dt, aes(x = !!sym(input$admin_level), y = year, fill = value)) +
              geom_tile(color = 'white') +
              scale_fill_gradient2(low = 'forestgreen', mid = 'white', high = 'red3', midpoint = mean(dt$value, na.rm = TRUE)) +
              labs(title = NULL, x = input$admin_level, y = 'Year', fill = 'Value') +
              theme_minimal() +
              theme(axis.text.x = element_text(angle = 45, size = 9, hjust = 1))
          )
        }
      })

      output$region_bar_graph <- renderCustomPlot({
        req(outlier_summary(), input$indicator)
        plot(outlier_summary(), 'region', input$indicator)
      })

      output$vaccine_bar_graph <- renderCustomPlot({
        req(outlier_summary(), input$indicator)
        plot(outlier_summary(), 'vaccine', input$indicator)
      })

      output$district_trend <- renderPlotly({
        req(outlier_summary(), input$district, input$indicator)

        indicator <- input$indicator
        mad <- paste0(indicator, '_mad')
        med <- paste0(indicator, '_med')

        dt <- outlier_summary() %>%
          select(district, year, month, any_of(c(indicator, med, mad))) %>%
          mutate(
            upper_bound = !!sym(med) + !!sym(mad) * 5,
            lower_bound = !!sym(med) - !!sym(mad) * 5,
            date = ym(paste0(year, '-', as.integer(month)))
          ) %>%
          filter(district == input$district)

        ggplotly(
          ggplot(dt, aes(date)) +
            geom_line(aes(y = !!sym(indicator)), colour = 'forestgreen') +
            geom_line(aes(y = !!sym(med)), colour = 'cyan', linetype = 'dashed') +
            geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "gray80", alpha = 0.5) +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
            scale_x_date(date_breaks = "3 months", date_labels = "%Y %b") +
            theme_minimal() +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust = 0.5, size = 16)
            )
        )
      })

      downloadExcel(
        id = 'download_data',
        filename = 'checks_outlier_detection',
        data = data,
        excel_write_function = function(wb) {
          completeness_rate <- data() %>% calculate_outliers_summary()
          district_completeness_rate <- data() %>% calculate_district_outlier_summary()

          sheet_name_1 <- 'Outliers'
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = 'Table 2a: Percentage of monthly values that are not extreme outliers, by year', startCol = 1, startRow = 1)
          writeData(wb, sheet = sheet_name_1, x = completeness_rate, startCol = 1, startRow = 3)

          # Check if sheet exists; if not, add it
          sheet_name_2 <- 'Outliers (districts)'
          addWorksheet(wb, sheet_name_2)
          writeData(wb, sheet = sheet_name_2, x = 'Table 2b: Percentage of districts with no extreme outliers, by year', startRow = 1, startCol = 1)
          writeData(wb, sheet = sheet_name_2, x = district_completeness_rate, startCol = 1, startRow = 3)
        }
      )

      downloadExcel(
        id = 'download_outliers',
        filename = paste0('checks_outlier_districts_', input$indicator, '_', input$year),
        data = outlier_districts,
        excel_write_function = function(wb) {
          district_outliers_sum <- outlier_districts()

          sheet_name_1 <- 'Districts with Extreme Outliers'
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = paste0('Districts with extreme outliers for ', input$indicator, ' in ', input$year), startCol = 1, startRow = 1)
          writeData(wb, sheet = sheet_name_1, x = district_outliers_sum, startCol = 1, startRow = 3)
        },
        label = 'Download Outliers'
      )

      contentHeaderServer(
        'outlier_detection',
        cache = cache,
        objects = pageObjectsConfig(input),
        md_title = 'Outlier Detection',
        md_file = 'quality_checks_outlier_detection.md'
      )
    }
  )
}
