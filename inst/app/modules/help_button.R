helpButtonUI <- function(id, text = 'Help') {
  ns <- NS(id)

  actionButton(ns('help'),
               text,
               icon = shiny::icon('question', verify_fa = FALSE),
               style = 'background-color: #8bc34a;width:100%;')
}

helpButtonServer <- function(id, title, size, file) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      observeEvent(input$help, {
        showModal(modalDialog(
          title = title,
          size = size,
          # APP_wd could be package app folder or just app folder depend on loading method
          fluidPage(includeMarkdown(paste0('help/', file))),
          easyClose = TRUE,
          fade = FALSE
        ))
      })

    }
  )
}
