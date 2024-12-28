outlierDetectionUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(2, offset = 10, helpButtonUI(ns('outlier_help')), style = 'margin-bottom: 10px;'),
    box(
      title = 'Indicators with Outlier',
      status = 'success',
      width = 6,
      fluidRow(
        column(12, gt_output(ns('outlier_summary'))),
        column(4, downloadButtonUI(ns('download_data'), label = 'Download Data'))
      )
    ),
    box(
      title = 'Districts with Outliers',
      status = 'success',
      width = 6,
      fluidRow(
        column(2, selectizeInput(ns('indicator'), label = 'Indicator', choice = NULL)),
        column(2, selectizeInput(ns('year'), label = 'Year', choice = NULL)),
        column(4, offset = 4, downloadButtonUI(ns('download_outliers'), label = 'Download Outliers'))
      ),
      fluidRow(
        column(12, gt_output(ns('district_outlier_summary')))
      )
    )
  )
}

outlierDetectionServer <- function(id, data) {
  stopifnot(is.reactive(data))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      vaccines_indicator <- reactive({
        req(data())

        indicator_groups <- attr(data(), 'indicator_groups')
        indicator_groups$vacc
      })

      outlier_summary <- reactive({
        req(data())

        data() %>%
          select(district, year, month, vaccines_indicator()) %>%
          cd2030:::add_outlier5std_column(vaccines_indicator()) %>%
          summarise(across(ends_with('_outlier5std'), ~ mean(.x, na.rm = TRUE)), .by = c(year, month, district))
      })

      observe({
        req(data())

        updateSelectizeInput(session, 'indicator', choices = vaccines_indicator())
      })

      observe({
        req(data())

        years <- unique(data()$year)

        updateSelectizeInput(session, 'year', choices = years)
      })

      district_outliers <- reactive({
        if (!isTruthy(input$indicator) || !isTruthy(input$year) || !isTruthy(outlier_summary())) return(NULL)

        selected_year <- as.numeric(input$year)
        outlier_column <- paste0(input$indicator, '_outlier5std')

        last_year <- max(data()$year, na.rm = TRUE)

        dt <- data() %>%
          mutate(
            district_mad =  round(mad(if_else(year < last_year, !!sym(input$indicator), NA_real_), na.rm = TRUE), 0),
            district_median = round(median(if_else(year < last_year, !!sym(input$indicator), NA_real_), na.rm = TRUE), 0),
            .by = district
          )

        outlier_summary() %>%
          filter(year == selected_year, !!sym(outlier_column) == 1) %>%
          left_join(dt, by = c('year', 'month', 'district')) %>%
          select(district, year, month, any_of(input$indicator), district_median, district_mad)
      })

      output$district_outlier_summary <- render_gt({
        req(district_outliers())

        district_outliers()
      })

      output$outlier_summary <- render_gt({
        outlier_summary() %>%
          summarize(
            across(
              ends_with('_outlier5std'),
              list(
                total =  ~ sum(.x, na.rm = TRUE),
                pct = ~ round((sum(.x, na.rm = TRUE) / n()) * 100, 1)
              ),
              .names = '{.col}-{.fn}'
            )
          ) %>%
          pivot_longer(
            cols = everything(),
            names_to = c("Indicator", "metric"),  # Split names into Indicator and Metric
            names_sep = "-"
          ) %>%
          pivot_wider(
            names_from = metric,  # Reshape Metric columns into separate columns
            values_from = value
          ) %>%
          mutate(
            Indicator = str_remove(Indicator, '_outlier5std')  # Remove "mis_" prefix for clarity
          ) %>%
          rename(
            `Months with Outliers` = total,
            `Percentage` = pct
          )
      })

      downloadButtonServer(
        id = 'download_data',
        filename = 'checks_outlier_detection',
        extension = 'xlsx',
        content = function(file) {
          wb <- createWorkbook()
          completeness_rate <- data() %>% calculate_outliers_summary()
          district_completeness_rate <- data() %>% calculate_district_outlier_summary()

          sheet_name_1 <- "Outliers"
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = "Table 2a: Percentage of monthly values that are not extreme outliers, by year", startCol = 1, startRow = 1)
          writeData(wb, sheet = sheet_name_1, x = completeness_rate, startCol = 1, startRow = 3)

          # Check if sheet exists; if not, add it
          sheet_name_2 <- "Outliers (districts)"
          addWorksheet(wb, sheet_name_2)
          writeData(wb, sheet = sheet_name_2, x = "Table 2b: Percentage of districts with no extreme outliers, by year", startRow = 1, startCol = 1)
          writeData(wb, sheet = sheet_name_2, x = district_completeness_rate, startCol = 1, startRow = 3)

          # Save the workbook
          saveWorkbook(wb, file, overwrite = TRUE)
        },
        data = data
      )

      downloadButtonServer(
        id = 'download_outliers',
        filename = paste0('checks_outlier_districts_', input$indicator, '_', input$year),
        extension = 'xlsx',
        content = function(file) {
          wb <- createWorkbook()
          district_outliers_sum <- district_outliers()

          sheet_name_1 <- "Districts with Extreme Outliers"
          addWorksheet(wb, sheet_name_1)
          writeData(wb, sheet = sheet_name_1, x = paste0('Districts with extreme outliers for ', input$indicator, ' in ', input$year), startCol = 1, startRow = 1)
          writeData(wb, sheet = sheet_name_1, x = district_outliers_sum, startCol = 1, startRow = 3)

          # Save the workbook
          saveWorkbook(wb, file, overwrite = TRUE)
        },
        data = district_outliers
      )

    }
  )
}
