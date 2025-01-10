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
      ns <- session$ns
      rv <- reactiveValues(generating = FALSE, future = NULL, file_path = NULL)

      data <- reactive({
        req(cache())
        cache()$get_adjusted_data()
      })

      observeEvent(input$download, {
        req(cache())

        if (is.null(cache()$get_un_estimates()) || is.null(cache()$get_wuenic_estimates()) ||
            is.null(cache()$get_national_survey()) || is.null(cache()$get_regional_survey())) {
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
                ns('format'), 'Select Format:',
                choices = c('Word' = 'word_document', 'PDF' = 'pdf_document')
              ),
              footer = tagList(
                modalButton('Cancel'),
                reportButtonUI(ns('report'))
              )
            )
          )
        }
      })

      reportButtonServer('report', cache, 'final_report')

    }
  )
}
