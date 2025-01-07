removeYearsUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('remove_years'), 'Remove Years', include_buttons = FALSE),
    contentBody(
      fluidRow(
        column(
          8,
          offset = 2,
          box(
            title = 'Remove Years with Erratic Data',
            status = 'danger',
            width = 12,
            solidHeader = TRUE,
            fluidRow(
              column(12, tags$p(
                'This module allows you to remove years with erratic data from the dataset.
            Erratic data may result from low reporting completeness, extreme outliers, or inconsistent
            trends, which can compromise analysis integrity. Use this tool to ensure clean and reliable
            data for meaningful insights and decision-making.',
                style = "color: red; font-weight: bold; margin-bottom: 15px;"
              ))
            ),

            fluidRow(
              column(8, offset = 2, selectizeInput(ns('year_to_remove'),
                                                   label = 'Select Years to Remove:',
                                                   choices = NULL,
                                                   multiple = TRUE,
                                                   options = list(placeholder = 'Choose years to remove...'))),
            ),

            fluidRow(
              column(
                8,
                offset = 2,
                actionButton(ns('remove_year'),
                             label = 'Confirm Removal',
                             icon = icon('wrench'),
                             style = 'background-color: #FFEB3B;font-weight: 500;width:100%; margin-top: 15px;')
              ),
              column(8, offset = 2, messageBoxUI(ns('remove_feedback')))

            )
          )
        )
      )
    )
  )
}

removeYearsServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      messageBox <- messageBoxServer('remove_feedback',
                                     default_message = 'No years have been removed yet.')

      data <- reactive({
        req(cache())
        cache()$get_data()
      })

      excluded_years <- reactive({
        req(cache())
        cache()$get_excluded_years()
      })

      observe({
        req(cache())

        years <- data() %>%
          distinct(year) %>%
          pull(year)

        updateSelectInput(session, 'year_to_remove', choices = years)
      })

      observeEvent(excluded_years(), {
        req(cache(), data())

        if (length(excluded_years()) > 0) {
          updateSelectInput(session, 'year_to_remove', selected = excluded_years())
          messageBox$update_message(
            paste0('Data for the following years has been removed: ', paste(excluded_years(), collapse = ', ')),
            'success'
          )
        } else {
          messageBox$update_message('No years have been removed yet.', 'info')
        }
      })

      observeEvent(input$remove_year, {
        req(cache())
        cache()$set_excluded_years(as.numeric(input$year_to_remove))
      })

      contentHeaderServer(
        'remove_years',
        cache = cache,
        md_title = 'Remove Years',
        md_file = '2_reporting_rate.md'
      )
    }
  )
}
