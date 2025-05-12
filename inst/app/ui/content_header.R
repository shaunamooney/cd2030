contentHeader <- function(id, title, i18n, include_buttons = TRUE) {
  ns <- NS(id)
  div(
    class = 'content-header',
    h1(title),
    if (include_buttons) {
      div(
        reportButtonUI(ns('report'), label = i18n$t("btn_generate_report")),
        documentationButtonUI(ns('add_notes'), i18n),
        helpButtonUI(ns('get_help'), name = i18n$t('btn_help')),
        class = 'right-buttons'
      )
    }
  )
}

contentHeaderServer <- function(id, cache, objects = NULL, md_title, md_file, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      reportButtonServer(
        id = 'report',
        cache = cache,
        report_name = reactive(id),
        i18n = i18n,
        adminlevel_1 = reactive(NULL)
      )

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
        page_name = md_title,
        i18n = i18n
      )
    }
  )
}
