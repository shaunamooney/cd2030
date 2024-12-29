overallScoreUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('overall_scoring'), 'Overall Score'),
    contentBody(
      box(
        title = 'Overall Score',
        status = 'success',
        width = 12,
        fluidRow(
          column(12, gt_output(ns('overall_score')))
        )
      )
    )
  )
}

overallScoreServer <- function(id, data) {
  stopifnot(is.reactive(data))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      output$overall_score <- render_gt({
        req(data())

        rows <- c("1a", "1b", "1c", "2a", "2b", "3f", "3g", "3h", "3i", "3j", '4')

        data() %>%
          calculate_overall_score() %>%
          gt() %>%
          tab_header(
            title = md("**Table 1a: Summary of data quality for reported immunization health facility data**")
          ) %>%
          tab_row_group(
            label = "3. Consistency of annual reporting",
            rows = no %in% c("3a", "3b", '3c', '3d', '3e', '3f', '3g', '3h', '3i', '3j')
          ) %>%
          tab_row_group(
            label = "2. Extreme outliers (Common Vaccine antigens)",
            rows = no %in% c("2a", "2b")
          ) %>%
          tab_row_group(
            label = "1. Completeness of monthly facility reporting (Immunization)",
            rows = no %in% c("1a", "1b", "1c")
          ) %>%
          tab_style(
            style = cell_fill(color = "lightgoldenrodyellow"),
            locations = cells_row_groups()
          ) %>%
          fmt_number(
            columns = starts_with("20"),
            rows = no %in% rows,
            decimals = 1
          ) %>%
          fmt_number(
            columns = starts_with("20"),
            rows = !no %in% rows,
            decimals = 4
          ) # %>%
        # tab_style(
        #   style = cell_fill(color = "darkgreen"),
        #   locations = cells_body(
        #     columns = starts_with("20"),
        #     rows = if_any(. > 80)
        #   )
        # ) %>%
        # tab_style(
        #   style = cell_fill(color = "yellow"),
        #   locations = cells_body(
        #     columns = starts_with("20"),
        #     rows = no %in% rows & between(.data[[as.character(no)]], 41, 80)
        #   )
        # ) %>%
        # tab_style(
        #   style = cell_fill(color = "red"),
        #   locations = cells_body(
        #     columns = starts_with("20"),
        #     rows = no %in% rows & .data[[as.character(no)]] <= 40
        #   )
        # )
      })

      contentHeaderServer(
        'overall_scoring',
        md_title = 'Overall Score',
        md_file = '2_reporting_rate.md'
      )

    }
  )
}
