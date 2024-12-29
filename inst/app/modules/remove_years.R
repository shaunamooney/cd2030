removeYearsUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('remove_years'), 'Remove Years'),
    contentBody(
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
          column(8, offset = 2, actionButton(ns('remove_year'),
                                             label = 'Confirm Removal',
                                             icon = icon('wrench'),
                                             style = 'background-color: #FFEB3B;font-weight: 500;width:100%; margin-top: 15px;'))
        ),

        fluidRow(
          column(12, offset = 2, uiOutput(ns("remove_feedback")), style = 'margin-top: 10px;')
        )
      )
    )
  )
}

removeYearsServer <- function(id, data) {
  stopifnot(is.reactive(data))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      removal_status <- reactiveVal(list(message = "No years have been removed yet.", color = "gray"))
      modified_data <- reactiveVal()

      observe({
        req(data())

        years <- data() %>%
          distinct(year) %>%
          pull(year)

        updateSelectInput(session, 'year_to_remove', choices = years)
      })

      observeEvent(input$remove_year, {

        req(data())

        new_data <- data() %>%
          filter(!year %in% input$year_to_remove)

        removal_status(list(
          message = paste0('Data for the following years has been removed: ', paste(input$year_to_remove, collapse = ', ')),
          color = "darkgreen"
        ))

        modified_data(new_data)
      })

      output$remove_feedback <- renderUI({
        status <- removal_status()
        tags$div(
          style = paste("color:", status$color, "; font-weight: bold;"),
          status$message
        )
      })

      contentHeaderServer(
        'remove_years',
        md_title = 'Remove Years',
        md_file = '2_reporting_rate.md'
      )

      return(reactive({
        if (is.null(modified_data())) data() else modified_data()
      }))
    }
  )
}
