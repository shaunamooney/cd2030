contentHeader <- function(id, title, include_buttons = TRUE) {
  ns <- NS(id)
  div(
    class = 'content-header',
    h1(title),
    if (include_buttons) {
      div(
        actionButton(
          inputId = ns('generate_report'),
          label = 'Generate Report',
          icon = icon('file-alt'),
          class ='btn bg-olive btn-flat btn-sm'
        ),
        documentationButtonUI(ns('add_notes')),
        helpButtonUI(ns('get_help')),
        class = 'right-buttons'
      )
    }
  )
}

contentHeaderServer <- function(id, cache, objects = NULL, md_title, md_file) {
  stopifnot(is.reactive(cache))

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
        cache = cache,
        document_objects = if (!is.null(objects)) objects[[id]] else NULL,
        page_id = id,
        page_name = md_title
      )
    }
  )
}
