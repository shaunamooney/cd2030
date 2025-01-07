source('modules/setup/national_rates.R')
source('modules/setup/file_upload.R')

setupUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('analysis_setup'), 'Analysis Setup', include_buttons = FALSE),
    contentBody(
      nationalRatesUI(ns('national_rates')),
      fileUploadUI(ns('file_uploads'))
    )
  )
}

setupServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      nationalRatesServer('national_rates', cache)
      fileUploadServer('file_uploads', cache)

      contentHeaderServer(
        'analysis_setup',
        cache = cache,
        md_title = 'Analysis Setup',
        md_file = '5_upload_data.md'
      )
    }
  )
}
