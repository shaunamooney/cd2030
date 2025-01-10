reportButtonUI <- function(id) {
  ns <- NS(id)
  actionButton(
    inputId = ns('generate_report'),
    label = 'Generate Report',
    icon = icon('file-alt'),
    class ='btn bg-olive btn-flat btn-sm'
  )
}

reportButtonServer <- function(id, cache, report_name) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      rv <- reactiveValues(generating = FALSE, future = NULL, file_path = NULL)

      country <- reactive({
        req(cache())
        cache()$get_country()
      })

      extension <- reactive({
        req(input$format)

        switch(input$format,
               'word_document' = 'docx',
               'pdf_document' = 'pdf',
               'html_document' = 'html')
      })

      observeEvent(input$generate_report, {
        req(cache())

        showModal(
          modalDialog(
            title = 'Download Options',
            selectizeInput(
              ns('format'), 'Select Format:',
              choices = c('Word' = 'word_document', 'PDF' = 'pdf_document')
            ),
            footer = tagList(
              modalButton('Cancel'),
              actionButton(ns("start_generate"), "Generate Report", class = 'btn bg-olive btn-flat')
            )
          )
        )
      })

      observeEvent(input$start_generate, {
        req(input$format)

        params <- list(
          format = input$format
        )

        removeModal()
        showModal(
          modalDialog(
            title = tagList(
              div(
                class = "text-center",
                h4("Generating Report")
              )
            ),
            div(
              class = "text-center",
              icon("file-alt", class = "fa-5x text-primary mb-3"), # Add a large icon
              p("Your report is being generated. This might take a few moments.", class = "lead"),
              div(class = "spinner-border text-primary", role = "status", span(class = "sr-only", "Loading..."))
            ),
            footer = NULL,
            easyClose = FALSE
          )
        )

        rv$generating <- TRUE
        rv$future <- future({
          temp_file <- tempfile(fileext = paste0(".", extension()))
          generate_final_report(
            cache = cache(),  # Use the minimized data
            output_file = temp_file,
            report_name = report_name,
            output_format = params$format
          )
          temp_file
        }, globals = list(cache = cache, params = params, generate_final_report = generate_final_report))

        rv$future %...>% {
          rv$generating <- FALSE
          rv$file_path <- .
          removeModal()

          # Update modal to show download button
          showModal(
            modalDialog(
              title = "Download Ready",
              div(
                class = "text-center",
                icon("check-circle", class = "fa-5x text-success mb-3"),
                h4("Success!"),
                p("The report has been successfully generated. Click below to download it."),
                p("This dialog will remain open until you download the report or click 'Dismiss.'")
              ),
              footer = tagList(
                fluidRow(
                  column(6, downloadButtonUI(ns("download_data"), "Download Report")),
                  column(6, modalButton("Dismiss"))
                )
              ),
              easyClose = FALSE # Prevent accidental dismissal by clicking outside the modal
            )
          )
        } %...!% {
          # Handle errors
          print(.)
          rv$generating <- FALSE
          removeModal()
          showModal(
            modalDialog(
              title = "Error",
              "An error occurred while generating the report. Please try again.",
              easyClose = TRUE,
              footer = modalButton("OK")
            )
          )
        }
      })

      downloadButtonServer(
        id = 'download_data',
        filename = paste0(country(), '_', report_name, '_countdown_report'),
        extension = extension(),
        content = function(file) {
          req(rv$file_path)
          file.copy(rv$file_path, file)
        },
        data = cache,
        label = 'Download Report',
        message = 'Generating report ...'
      )
    }
  )
}
