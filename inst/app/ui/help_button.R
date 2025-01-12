helpButtonUI <- function(id, text = 'Get Help') {
  ns <- NS(id)
  actionButton(
    inputId = ns('help'),
    label = text,
    icon = shiny::icon('question'),
    class ='btn bg-aqua btn-flat btn-sm',
    style = 'margin-left:4px;'
  )
}

helpButtonServer <- function(id, title, size = 'l', md_file) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      observeEvent(input$help, {
        showModal(modalDialog(
          title = tags$div(class = "text-info", title),
          size = size,
          div(class = "modal-rmd-content", includeMarkdown(file.path('help', md_file))),
          easyClose = TRUE,
          fade = FALSE
        ))
      })

    }
  )
}
