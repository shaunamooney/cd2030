helpButtonUI <- function(id, text = 'Get Help') {
  ns <- NS(id)
  actionButton(
    inputId = ns('help'),
    label = text,
    icon = shiny::icon('question'),
    class ='btn-sm',
    # style = 'background-color: #8bc34a;width:100%;',
    style = 'color: white; background-color: #8bc34a; margin-left:4px;'
  )
}

helpButtonServer <- function(id, title, size, md_file) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      observeEvent(input$help, {
        showModal(modalDialog(
          title = title,
          size = size,
          # APP_wd could be package app folder or just app folder depend on loading method
          fluidPage(includeMarkdown(paste0('help/', md_file))),
          easyClose = TRUE,
          fade = FALSE
        ))
      })

    }
  )
}
