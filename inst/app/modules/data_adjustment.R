dataAjustmentUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('data_adjustment'), 'Data Adjustments'),
    contentBody(
      box(
        title = 'K-Factors',
        status = 'success',
        solidHeader = TRUE,
        width = 6,
        fluidRow(
          column(4, selectizeInput(ns('k_anc'),
                                   selected = '0.25',
                                   label = 'ANC K-Factor',
                                   choices = c(0, 0.25, 0.5, 0.75))),
          column(4, selectizeInput(ns('k_delivery'),
                                   label = 'Delivery K-Factor',
                                   selected = '0.25',
                                   choices = c(0, 0.25, 0.5, 0.75))),
          column(4, selectizeInput(ns('k_vaccines'),
                                   label = 'Vaccines K-Factor',
                                   selected = '0.25',
                                   choices = c(0, 0.25, 0.5, 0.75)))
        )
      ),

      box(
        title = 'Adjust',
        status = 'danger',
        width = 6,
        solidHeader = TRUE,

        fluidRow(
          column(12, tags$p(
            "Adjusting the data applies custom corrections, such as removing specific
            years and scaling selected indicators. This process modifies the current
            dataset to align with predefined criteria for completeness and consistency.",
            style = "color: red; font-weight: bold; margin-bottom: 15px;"
          ))
        ),

        fluidRow(
          column(8, offset = 2, actionButton(ns('adjust_data'),
                                             label = 'Adjust Data',
                                             icon = icon('wrench'),
                                             style = 'background-color: #FFEB3B;font-weight: 500;width:100%; margin-top: 15px;')),
          column(12, offset = 2, uiOutput(ns("adjust_feedback")), style = 'margin-top: 10px;'),
          column(8, offset = 2, downloadButtonUI(ns('download_data'), label = "Download Adjusted Dataset"))
        )
      )
    )
  )
}

dataAdjustmentServer <- function(id, data) {
  stopifnot(is.reactive(data))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      adjustment_status <- reactiveVal(list(message = "Dataset not adjusted", color = "gray"))

      k_factors <- reactive({
        c(
          anc = as.integer(input$k_anc),
          idelv = as.integer(input$k_delivery),
          vacc = as.integer(input$k_vaccines)
        )
      })

      observe({
        req(data())

        indicator_groups <- attr(data(), 'indicator_groups')
        all_indicators <- list_c(indicator_groups)

        names(all_indicators) <- all_indicators
        all_indicators <- c('Select' = '0', all_indicators)

        updateSelectInput(session, 'indicator', choices = all_indicators)
      })

      output$adjust_feedback <- renderUI({
        status <- adjustment_status()
        tags$div(
          style = paste("color:", status$color, "; font-weight: bold;"),
          status$message
        )
      })

      modified_data <- reactiveVal()

      observeEvent(input$adjust_data, {

        req(data())

        new_data <- data() %>%
          adjust_service_data(adjustment = 'custom',
                              k_factors = k_factors())
        adjustment_status(list(
          message = 'Data Adjusted',
          color = "darkgreen"
        ))

        modified_data(new_data)
      })

      downloadButtonServer(
        id = 'download_data',
        filename = 'master_adj_dataset',
        extension = 'dta',
        content = function(file) {
          haven::write_dta(modified_data(), file)
        },
        data = modified_data
      )

      contentHeaderServer(
        'data_adjustment',
        md_title = 'Data Adjustments',
        md_file = '2_reporting_rate.md'
      )

      return(reactive({
        list(
          modified_data = if (is.null(modified_data())) data() else modified_data(),
          k_factors = k_factors()
        )
      }))
    }
  )
}
