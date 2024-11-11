dataCompletenessUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    box(
      title = '% of Months with Complete Data',
      status = 'success',
      width = 12,
      fluidRow(
        column(2, offset = 10, helpButtonUI(ns('completeness_help'))),
        column(12, gt_output(ns('completeness_rate')))
      )
    ),
    box(
      title = '% of District with Complete Data',
      status = 'success',
      width = 12,
      fluidRow(
        column(2, offset = 10, helpButtonUI(ns('district_completeness_help'))),
        column(12, gt_output(ns('district_completeness_rate')))
      )
    )
  )
}

dataCompletenessServer <- function(id, data) {
  stopifnot(is.reactive(data))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      output$completeness_rate <- render_gt({

        data() %>%
          calculate_completeness_summary() %>%
          gt() %>%
          tab_header(
            title = 'Table 4a: Percentage of monthly values with complete data, by year'
          )

      })

      output$district_completeness_rate <- render_gt({

        data() %>%
          calculate_district_completeness_summary() %>%
          gt() %>%
          tab_header(
            title = 'Table 4a: Percentage of Districts with complete data, by year'
          )

      })
    }
  )
}
