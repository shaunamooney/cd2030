downloadButtonUI <- function(id, label = 'Download') {
  ns <- NS(id)
  uiOutput(ns('download_ui'))
}

downloadButtonServer <- function(id, filename, extension, content, data, label = 'Download', message = 'Downloading...') {
  stopifnot(is.reactive(data))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      output$download_ui <- renderUI({
        req(data())

        downloadButton(ns('download_button'),
                       label = label,
                       icon = icon(name = NULL, class = "bi bi-download"),
                       class = 'btn btn-default btn-flat',
                       style = 'width:100%;margin-top:10px;')
      })

      output$download_button <- downloadHandler(
        filename = function() {
          paste0(filename, '_', format(Sys.time(), '%Y%m%d%H%M'), '.', extension)
        },
        content = function(file) {
          session$sendCustomMessage(
            type = 'starting_download',
            list(id = ns('download_button'), message = message)
          )
          content(file)
          session$sendCustomMessage(
            type = 'end_download',
            list(id = ns('download_button'), label = label)
          )
        }
      )
    }
  )
}
