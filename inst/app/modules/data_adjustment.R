dataAjustmentUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('data_adjustment'), 'Data Adjustments'),
    contentBody(
      fluidRow(
        column(
          8,
          offset = 2,
          box(
            title = 'K-Factors',
            status = 'success',
            solidHeader = TRUE,
            width = 12,
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
          )
        ),
        column(
          8,
          offset = 2,
          box(
            title = 'Adjust',
            status = 'danger',
            width = 12,
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
              column(8, offset = 2, messageBoxUI(ns('feedback'))),
              column(8, offset = 2, downloadButtonUI(ns('download_data')))
            )
          )
        )
      )
    )
  )
}

dataAdjustmentServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      messageBox <- messageBoxServer('feedback', default_message = 'Dataset not adjusted')

      data <- reactive({
        req(cache())
        cache()$get_data_with_excluded_years()
      })

      adjusted_flag <- reactive({
        req(cache())
        cache()$get_adjusted_flag()
      })

      modified_data <- reactive({
        req(cache())
        cache()$get_adjusted_data()
      })

      observe({
        req(cache())

        indicator_groups <- cache()$get_indicator_groups()
        all_indicators <- list_c(indicator_groups)

        names(all_indicators) <- all_indicators
        all_indicators <- c('Select' = '0', all_indicators)

        updateSelectInput(session, 'indicator', choices = all_indicators)
      })

      observe({
        req(cache(), adjusted_flag())
        if (adjusted_flag()) {
          new_data <- data() %>%
            adjust_service_data(adjustment = 'custom',
                                k_factors = cache()$get_k_factors())
          cache()$set_adjusted_data(new_data)

          messageBox$update_message('Data Adjusted', 'success')
        }
      })

      observe({
        req(cache(), input$k_anc, input$k_delivery, input$k_vaccines)
        cache()$set_k_factors(c(
          anc = as.integer(input$k_anc),
          idelv = as.integer(input$k_delivery),
          vacc = as.integer(input$k_vaccines)
        ))
      })

      observeEvent(input$adjust_data, {
        req(cache())
        cache()$toggle_adjusted_flag(TRUE)
      })

      downloadButtonServer(
        id = 'download_data',
        filename = 'master_adj_dataset',
        extension = 'dta',
        content = function(file) {
          haven::write_dta(modified_data(), file)
        },
        data = adjusted_flag,
        label = "Download Adjusted Dataset"
      )

      contentHeaderServer(
        'data_adjustment',
        md_title = 'Data Adjustments',
        md_file = '2_reporting_rate.md'
      )
    }
  )
}
