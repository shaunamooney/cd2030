downloadReportUI <- function(id) {
  ns <- NS(id)

  tags$li(
    actionLink(
      inputId = ns('download'),
      label = 'Download Report',
      icon = icon('download')
    ),
    class = 'dropdown'
  )
}

downloadReportServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      data <- reactive({
        req(cache())
        cache()$get_adjusted_data()
      })

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

      observeEvent(input$download, {
        req(cache())

        if (is.null(cache()$get_un_estimates())) {
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
                  column(6, align = 'right', downloadButtonUI(ns('download_data')))
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
          print(file)
          generate_final_report(cache = cache(),
                                output_file = file,
                                output_format = input$format,
                                denominator = input$denominator)
        },
        data = cache,
        label = 'Download Report'
      )

    }
  )
}
