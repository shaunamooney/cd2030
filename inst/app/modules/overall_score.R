overallScoreUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('overall_scoring'), 'Overall Score', include_buttons = FALSE),
    contentBody(
      box(
        title = 'Overall Score',
        status = 'success',
        width = 12,
        fluidRow(
          column(12, uiOutput(ns('overall_score')))
        )
      )
    )
  )
}

overallScoreServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      data <- reactive({
        req(cache())
        cache()$countdown_data
      })

      output$overall_score <- renderUI({
        req(data())

        years <- unique(data()$year)
        threshold <- cache()$performance_threshold

        data() %>%
          calculate_overall_score(threshold) %>%
          mutate(
            type = case_when(
              no %in% c("1a", "1b", "1c") ~ '1. Completeness of monthly facility reporting (Immunization)',
              no %in% c("2a", "2b") ~ '2. Extreme outliers (Common Vaccine antigens)',
              no %in% c("3a", "3b",'3f', '3g') ~ '3. Consistency of annual reporting'
            )
          ) %>%
          as_grouped_data(groups = 'type') %>%
          as_flextable() %>%
          bold(j = 1, i = ~ !is.na(type), bold = TRUE, part = "body") %>%
          bold(part = "header", bold = TRUE) %>%
          colformat_double(i = ~ is.na(type) & !no %in% c("3a", "3b"), j = as.character(years), digits = 0, big.mark = ",") %>%
          colformat_double(i = ~ is.na(type) & no %in% c("3a", "3b"), j = as.character(years), digits = 2) %>%
          bg(
            i = ~ is.na(type) & !no %in% c("3a", "3b"),
            j = as.character(years),
            bg = function(x) {
              result <- map_chr(as.list(x), ~ {
                case_when(
                  is.na(.x) || is.null(.x) || !is.numeric(.x) ~ 'white',
                  .x >= threshold ~ 'seagreen',
                  .x > 40 & .x < threshold ~ 'yellow',
                  .x <= 40 ~ 'red',
                  .default = 'white'
                )
              })
              return(result)
            },
            part = "body"
          ) %>%
          bg(
            i = ~ !is.na(type), part = "body",
            bg = 'lightgoldenrodyellow'
          ) %>%
          theme_vanilla() %>%
          autofit() %>%
          htmltools_value()
      })

      contentHeaderServer(
        'overall_scoring',
        cache = cache,
        md_title = 'Overall Score',
        md_file = '2_reporting_rate.md'
      )

    }
  )
}
