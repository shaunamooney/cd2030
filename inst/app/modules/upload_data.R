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
      dhis2BoxServer('dhis2_box')
      data <- uploadBoxServer('upload_box')
      return(data)
    }
  )
}
