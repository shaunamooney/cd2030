downloadButtonUI <- function(id, label = 'Download') {
  ns <- NS(id)
  uiOutput(ns("download_ui"))
}

downloadButtonServer <- function(id, filename, extension, content, data, label = 'Download') {
  stopifnot(is.reactive(data))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      output$download_ui <- renderUI({
        req(data())

        ns <- session$ns
        downloadButton(ns('download_button'),
                       label = label,
                       style = 'color:#2196F3;width:100%;margin-top:10px;')
      })

      output$download_button <- downloadHandler(
        filename = function() {
          paste0(filename, '_', format(Sys.time(), '%Y%m%d%H%M'), '.', extension)
        },
        content = function(file) {
          content(file)
        }
      )
    }
  )
}
