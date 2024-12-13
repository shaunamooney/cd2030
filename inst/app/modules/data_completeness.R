dataCompletenessUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(2, offset = 10, helpButtonUI(ns('completeness_help')), style = 'margin-bottom: 10px;'),
    box(
      title = 'Indicators with Missing Data',
      status = 'success',
      width = 6,
      fluidRow(
        column(12, gt_output(ns('incomplete_district_summary'))),
        column(3, downloadButton(ns('download_data'), label = 'Download Data', style = 'color:#2196F3;width:100%;margin-top:10px;'))
      )
    ),
    box(
      title = 'Districts with Missing Data',
      status = 'success',
      width = 6,
      fluidRow(
        column(2, selectizeInput(ns('indicator'), label = 'Indicator', choice = NULL)),
        column(2, selectizeInput(ns('year'), label = 'Year', choice = NULL))
      ),
      fluidRow(
        column(12, gt_output(ns('incomplete_district')))
      )
    )
  )
}

dataCompletenessServer <- function(id, data) {
  stopifnot(is.reactive(data))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      vaccines_indicator <- reactive({
        req(data())

        indicator_groups <- attr(data(), 'indicator_groups')
        indicator_groups$vacc
      })

      completeness_summary <- reactive({
        req(data())

        data() %>%
          select(district, year, month, vaccines_indicator()) %>%
          mutate(
            # Flag missing values
            across(vaccines_indicator(), ~ if_else(is.na(.), 1,  0), .names = "mis_{.col}"),

            .by = district
          ) %>%
          summarise(across(starts_with('mis_'), ~ mean(.x, na.rm = TRUE)), .by = c(year, month, district))
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


      output$incomplete_district <- render_gt({
        req(input$indicator, input$year, completeness_summary())

        selected_year <- as.numeric(input$year)
        mis_column <- paste0('mis_', input$indicator)

        completeness_summary() %>%
          select(district, year, month, any_of(mis_column)) %>%
          filter(year == selected_year, !!sym(mis_column) == 1) %>%
          select(-any_of(mis_column))
      })

      output$incomplete_district_summary <- render_gt({
        completeness_summary() %>%
          summarize(
            across(
              starts_with('mis_'),
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
            Indicator = str_remove(Indicator, 'mis_')  # Remove "mis_" prefix for clarity
          ) %>%
          rename(
            `Missing Months` = total,
            `Percentage` = pct
          )
      })

      output$download_data <- downloadHandler(
        filename = function() { paste0("checks-reporting_rate_", Sys.Date(), ".xlsx") },
        content = function(file) {

          req(data())

          wb <- createWorkbook()
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

          # Save the workbook
          saveWorkbook(wb, file, overwrite = TRUE)
        }
      )
    }
  )
}
