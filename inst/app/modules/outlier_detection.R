outlierDetectionUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    box(
      title = '% of Months with Outlier',
      status = 'success',
      width = 12,
      fluidRow(
        column(2, offset = 10, helpButtonUI(ns('outlier_help'))),
        column(12, gt_output(ns('outlier_summary')))
      )
    ),
    box(
      title = '% of District with Complete Data',
      status = 'success',
      width = 12,
      fluidRow(
        column(2, offset = 10, helpButtonUI(ns('district_outlier_help'))),
        column(12, gt_output(ns('districtoutlier_summary')))
      )
    )
  )
}

outlierDetectionServer <- function(id, data) {
  stopifnot(is.reactive(data))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      output$outlier_summary <- render_gt({

        data() %>%
          calculate_outliers_summary() %>%
          gt() %>%
          tab_header(
            title = 'Table 2a: Percentage of monthly values that are not extreme outliers, by year'
          )

      })

      output$districtoutlier_summary <- render_gt({

        data() %>%
          calculate_district_outlier_summary() %>%
          gt() %>%
          tab_header(
            title = 'Table 2b: Percentage of districts with no extreme outliers, by year'
          )

      })

    }
  )
}
