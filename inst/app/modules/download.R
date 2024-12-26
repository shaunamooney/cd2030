downloadUI <- function(id, label = "Download") {
  ns <- NS(id)
  tagList(
    downloadButton(outputId = ns("download_button"), label = label, style = 'color:#2196F3;width:100%;margin-top:10px;')
  )
}

downloadServer <- function(id, filename, extension, content, data) {
  stopifnot(is.reactive(data))

  moduleServer(id, function(input, output, session) {

    data_available <- reactive({ isTruthy(data()) })

    observe({
      if (!data_available()) {
        hide('download_button')
      } else {
        show('download_button')
      }
    })

    output$download_button <- downloadHandler(
      filename = function() {
        paste0(filename, '_', format(Sys.time(), '%Y%m%d%H%M'), '.', extension)
      },
      content = function(file) {
        if (!data_available()) {
          message("No data available for download.")
          stop("No data available for download.")
        }

        content(file)
      }
    )
  })
}
