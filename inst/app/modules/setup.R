setupUI <- function(id) {
  ns <- NS(id)

  fluidRow(

    column(4, offset = 8, helpButtonUI(ns('upload_data')), style = 'margin-bottom: 10px;'),

    box(
      title = 'Setup National Rates',
      status = 'success',
      solidHeader = TRUE,
      width = 12,
      fluidRow(
        column(3, offset = 1, numericInput(ns('neonatal_mortality_rate'), 'Neonatal Mortality Rate',
                                          min = 0, max = 0.05, value = 0.025, step = 0.001)),
        column(3, offset = 0, numericInput(ns('post_neonatal_mortality_rate'), 'Post Neonatal Mortality Rate',
                                          min = 0, max = 0.05, value = 0.024, step = 0.001)),
        column(3, offset = 0, numericInput(ns('twin_rate'), 'Twin Rate',
                                          min = 0, max = 0.05, value = 0.015, step = 0.001))
      ),
      fluidRow(
        column(3, offset = 1, numericInput(ns('pregnancy_loss'), 'Pregnancy Loss',
                              min = 0, max = 0.05, value = 0.03, step = 0.001)),
        column(3, offset = 0, numericInput(ns('stillbirth_rate'), 'Still Birth Rate',
                              min = 0, max = 0.05, value = 0.02, step = 0.001)),
        column(3, offset = 0, numericInput(ns('penta1_mortality_rate'), 'ANC1 to Penta1 Mortality Rate',
                              min = 0, max = 0.05, value = 0.025, step = 0.001))
      ),
      fluidRow(
        column(3, offset = 1, numericInput(ns('anc1_prop'), 'ANC1 Survey',
                                          min = 0, max = 100, value = 0, step = 1)),
        column(3, offset = 0, numericInput(ns('penta1_prop'), 'Penta1 Survey',
                                          min = 0, max = 100, value = 0, step = 1)),
      )
    ),

    box(
      title = 'Upload Survey Data',
      status = 'success',
      solidHeader = TRUE,
      width = 12,

      fluidRow(
        column(
          6,
          fileInput(
            inputId = ns('un_data'),
            label = 'Upload UN Estimates data',
            buttonLabel = 'Browse or Drop...',
            placeholder = 'Supported formats: .dta'
          ),

          uiOutput(ns("un_enhanced_feedback"))
        ),
        column(
          6,
          fileInput(
            inputId = ns('wuenic_data'),
            label = 'Upload WUENIC data',
            buttonLabel = 'Browse or Drop...',
            placeholder = 'Supported formats: .dta'
          ),

          uiOutput(ns("wuenic_enhanced_feedback"))
        ),
        column(
          6,
          fileInput(
            inputId = ns('national_survey_data'),
            label = 'Upload National Survey Data',
            buttonLabel = 'Browse or Drop...',
            placeholder = 'Supported formats: .dta'
          ),

          uiOutput(ns("survdata_enhanced_feedback"))
        ),
        column(
          6,
          fileInput(
            inputId = ns('regional_survey_data'),
            label = 'Upload Regional Survey Data',
            buttonLabel = 'Browse or Drop...',
            placeholder = 'Supported formats: .dta'
          ),

          uiOutput(ns("gregion_enhanced_feedback"))
        )
      ),

      fluidRow(
        column(3, selectizeInput(ns('gregion_scale'),
                                 label = 'Regional Survey Scale',
                                 choices = c(1, 100)))
      )
    )
  )
}

setupServer <- function(id, data, survey_data) {
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(survey_data))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      un_path <- reactiveVal()
      wuenic_path <- reactiveVal()
      surv_path <- reactiveVal()
      gregion_path <- reactiveVal()

      un_estimates <- reactiveVal()
      wuenic_data <- reactiveVal()
      survdata <- reactiveVal()
      gregion_data <- reactiveVal()

      upload_status <- reactiveValues(un =list(message = "Awaiting file upload...", color = "gray"),
                                      wuenic = list(message = "Awaiting file upload...", color = "gray"),
                                      survdata = list(message = "Awaiting file upload...", color = "gray"),
                                      gregion = list(message = "Awaiting file upload...", color = "gray"),)

      country <- reactive({
        req(data())
        attr(data(), 'country')
      })

      country_iso <- reactive({
        req(data())
        attr(data(), 'iso3')
      })

      setup_values <- reactive({
        list(
          rates = list(
            nmr = input$neonatal_mortality_rate,
            pnmr = input$post_neonatal_mortality_rate,
            twin_rate = input$twin_rate,
            preg_loss = input$pregnancy_loss,
            sbr = input$stillbirth_rate,
            penta1_mort_rate = input$penta1_mortality_rate,
            anc1 = input$anc1_prop / 100,
            penta1 = input$penta1_prop / 100
          ),
          data = list(
            un = un_estimates(),
            wuenic = wuenic_data(),
            survdata = survdata(),
            gregion = gregion_data()
          )
        )
      })

      observe({
        req(country_iso())

        dt <- survey_data()

        updateNumericInput(session, 'anc1_prop', value = dt$anc1)
        updateNumericInput(session, 'penta1_prop', value = dt$penta1)
      })

      observeEvent(data(), {
        start_year <- min(data()$year)
        end_year <- max(data()$year)

        if (!is.null(un_path())) {
          un <- load_un_estimates(un_path(), country_iso(), start_year, end_year)
          un_estimates(un)
        }

        if (!is.null(wuenic_path())) {
          wuenic <- load_wuenic_data(wuenic_path(), country_iso())
          wuenic_data(wuenic)
        }

        surv_path(NULL)
        survdata(NULL)
        upload_status$survdata <- list(
          message = paste("Awaiting file upload..."),
          color = "gray"
        )

        gregion_path(NULL)
        gregion_data(NULL)
        upload_status$gregion <- list(
          message = paste("Awaiting file upload..."),
          color = "gray"
        )
      })

      observeEvent(input$un_data, {
        req(country_iso())

        file_name <- input$un_data$name
        un_path(input$un_data$datapath)

        tryCatch({
          start_year <- min(data()$year)
          end_year <- max(data()$year)
          dt <- load_un_estimates(un_path(), country_iso(), start_year, end_year)

          upload_status$un <- list(
            message = paste("Upload successful: File", file_name, "is ready."),
            color = "darkgreen"
          )
          un_estimates(dt)
        }, error = function(e) {
          upload_status$un <- list(
            message = "Upload failed: Check the file format and try again.",
            color = "red"
          )
          un_estimates(NULL)
        })
      })


      observeEvent(input$wuenic_data, {
        req(country_iso())

        file_name <- input$wuenic_data$name
        wuenic_path(input$wuenic_data$datapath)

        tryCatch({
          dt <- load_wuenic_data(wuenic_path(), country_iso())

          upload_status$wuenic <- list(
            message = paste("Upload successful: File", file_name, "is ready."),
            color = "darkgreen"
          )

          wuenic_data(dt)
        }, error = function(e) {
          upload_status$wuenic <- list(
            message = "Upload failed: Check the file format and try again.",
            color = "red"
          )
          wuenic_data(NULL)
        })
      })

      observeEvent(input$national_survey_data, {
        req(country_iso())

        file_name <- input$national_survey_data$name
        surv_path(input$national_survey_data$datapath)

        tryCatch({
          dt <- load_survey_data(surv_path(), country_iso(), 'national')

          upload_status$survdata <- list(
            message = paste("Upload successful: File", file_name, "is ready."),
            color = "darkgreen"
          )
          survdata(dt)
        }, error = function(e) {
          upload_status$survdata <- list(
            message = "Upload failed: Check the file format and try again.",
            color = "red"
          )
          survdata(NULL)
        })
      })

      observeEvent(input$regional_survey_data, {
        req(country_iso())

        file_name <- input$regional_survey_data$name
        gregion_path(input$regional_survey_data$datapath)

        tryCatch({
          dt <- load_survey_data(gregion_path(),
                                 country_iso = country_iso(),
                                 admin_level = 'admin_level_1',
                                 scale = as.integer(input$gregion_scale))

          upload_status$gregion <- list(
            message = paste("Upload successful: File", file_name, "is ready."),
            color = "darkgreen"
          )
          gregion_data(dt)
        }, error = function(e) {
          upload_status$gregion <- list(
            message = "Upload failed: Check the file format and try again.",
            color = "red"
          )
          gregion_data(NULL)
        })
      })

      observeEvent(input$gregion_scale, {
        req(gregion_path(), country_iso(), input$gregion_scale)

        dt <- load_survey_data(gregion_path(),
                               country_iso = country_iso(),
                               admin_level = 'admin_level_1',
                               scale = as.integer(input$gregion_scale))
        gregion_data(dt)
      })

      output$un_enhanced_feedback <- renderUI({
        status <- upload_status$un
        tags$div(
          style = paste("color:", status$color, "; font-weight: bold;"),
          status$message
        )
      })

      output$wuenic_enhanced_feedback <- renderUI({
        status <- upload_status$wuenic
        tags$div(
          style = paste("color:", status$color, "; font-weight: bold;"),
          status$message
        )
      })

      output$survdata_enhanced_feedback <- renderUI({
        status <- upload_status$survdata
        tags$div(
          style = paste("color:", status$color, "; font-weight: bold;"),
          status$message
        )
      })

      output$gregion_enhanced_feedback <- renderUI({
        status <- upload_status$gregion
        tags$div(
          style = paste("color:", status$color, "; font-weight: bold;"),
          status$message
        )
      })

      helpButtonServer('upload_data', 'Upload required Data', 'l', '5_upload_data.md')

      return(setup_values)
    }
  )
}
