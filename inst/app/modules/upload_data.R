source('modules/upload_data/upload_box.R')
source('modules/upload_data/dhis2_box.R')

uploadDataUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    uploadBoxUI(ns('upload_box')),
    dhis2BoxUI(ns('dhis2_box'))
  )
}

uploadDataServer <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      data <- reactiveVal()

      dhis2_dt <- dhis2BoxServer('dhis2_box')
      upload_dt <- uploadBoxServer('upload_box')

      observeEvent(dhis2_dt(), {
        data(dhis2_dt())
      })

      observeEvent(upload_dt(), {
        data(upload_dt())
      })

      return(data)
    }
  )
}
