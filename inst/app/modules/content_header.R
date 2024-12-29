contentHeader <- function(id, title) {
  ns <- NS(id)
  div(
    h1(title),
    div(
      documentationButtonUI(ns('add_notes')),
      helpButtonUI(ns('get_help')),
      class = 'right-buttons'
    ),
    class = 'content-header'
  )
}

contentHeaderServer <- function(id, md_title, md_file) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      helpButtonServer(
        id = 'get_help',
        title = md_title,
        size = 'l',
        md_file = md_file)

      documentationButtonServer(
        id = 'add_notes',
        page_name = md_title,
        parameters_reactive = reactive()
      )
    }
  )
}
