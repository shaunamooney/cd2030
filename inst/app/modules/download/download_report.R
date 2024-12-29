downloadReportUI <- function(id) {
  ns <- NS(id)

  actionLink(
    inputId = ns('download'),
    label = 'Download Report',
    icon = icon('download')
  )
}

downloadReportServer <- function(id, data, national_values, k_factors, report_year) {
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(national_values))
  stopifnot(is.reactive(k_factors))
  stopifnot(is.reactive(report_year))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      country <- reactive({
        req(data())
        attr(data(), 'country')
      })

      extension <- reactive({
        req(input$format)

        switch(input$format,
               'word_document' = 'docx',
               'pdf_document' = 'pdf',
               'html_document' = 'html')
      })

      observeEvent(input$report, {

        if (!isTruthy(data()) || !isTruthy(national_values()$data$un)) {
          # Show an error dialog if data is not available
          showModal(
            modalDialog(
              title = 'Error',
              'The necessary data for generating the report is not available. Please ensure that the data is uploaded and processed correctly.',
              easyClose = TRUE,
              footer = modalButton('OK')
            )
          )
        } else {
          ns <- NS(id)
          showModal(
            modalDialog(
              title = 'Download Options',
              selectizeInput(
                ns('denominator'), 'Select Denominator:',
                choices = c('DHIS 2' = 'dhis2', 'ANC 1' = 'anc1', 'Penta 1' = 'penta1')
              ),
              selectizeInput(
                ns('format'), 'Select Format:',
                choices = c('Word' = 'word_document', 'PDF' = 'pdf_document')
              ),
              footer = tagList(
                fluidRow(
                  column(6, align = 'left', modalButton('Cancel')),
                  column(6, align = 'right', downloadButtonUI(ns('download_data'), 'Download Report'))
                )
              )
            )
          )
        }
      })

      downloadButtonServer(
        id = 'download_data',
        filename = paste0(country(), '_countdown_report'),
        extension = extension(),
        content = function(file) {
          generate_checks_report(data(), file,
                                 survey_values = national_values(),
                                 k_factors = k_factors(),
                                 country = country(),
                                 output_format = input$format,
                                 denominator = input$denominator,
                                 survey_start_year = report_year())
        },
        data = data
      )

    }
  )
}
