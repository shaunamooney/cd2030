dataAjustmentUI <- function(id) {
  ns <- NS(id)

  k_factor_options <- c(0, 0.25, 0.5, 0.75)
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
                                       label = 'ANC K-Factor',
                                       choices = k_factor_options)),
              column(4, selectizeInput(ns('k_delivery'),
                                       label = 'Delivery K-Factor',
                                       choices = k_factor_options)),
              column(4, selectizeInput(ns('k_vaccines'),
                                       label = 'Vaccines K-Factor',
                                       choices = k_factor_options))
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

      state <- reactiveValues(loaded = FALSE)
      messageBox <- messageBoxServer('feedback', default_message = 'Dataset not adjusted')

      data <- reactive({
        req(cache())
        cache()$get_data_with_excluded_years()
      })

      modified_data <- reactive({
        req(cache())
        cache()$get_adjusted_data()
      })

      adjusted_flag <- reactive({
        req(cache())
        cache()$get_adjusted_flag()
      })

      k_factors <- reactive({
        req(cache())
        cache()$get_k_factors()
      })

      observeEvent(data(), {
        req(data())
        state$loaded <- FALSE
      })

      observeEvent(c(input$k_anc, input$k_delivery, input$k_vaccines), {
        req(cache(), input$k_anc, input$k_delivery, input$k_vaccines)

        k <- k_factors()
        k['anc'] <- as.numeric(input$k_anc)
        k['idelv'] <- as.numeric(input$k_delivery)
        k['vacc'] <- as.numeric(input$k_vaccines)

        cache()$set_k_factors(k)
      })

      observe({
        req(cache(), !state$loaded)

        k <- k_factors()
        updateSelectInput(session, 'k_anc', selected = as.character(unname(k['anc'])))
        updateSelectInput(session, 'k_delivery', selected = as.character(unname(k['idelv'])))
        updateSelectInput(session, 'k_vaccines', selected = as.character(unname(k['vacc'])))

        state$loaded <- TRUE
      })

      observeEvent(input$adjust_data, {
        req(cache())
        messageBox$update_message('Adjusting ...', 'info')
        cache()$toggle_adjusted_flag(FALSE)
        cache()$toggle_adjusted_flag(TRUE)
      })

      observeEvent(adjusted_flag(), {
        req(data())
        if (adjusted_flag()) {
          new_data <- data() %>%
            adjust_service_data(adjustment = 'custom',
                                k_factors = k_factors())
          cache()$set_adjusted_data(new_data)

          messageBox$update_message('Data Adjusted', 'success')
        } else {
          messageBox$update_message('Dataset not adjusted', 'info')
        }
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
        cache = cache,
        md_title = 'Data Adjustments',
        md_file = '2_reporting_rate.md'
      )
    }
  )
}
