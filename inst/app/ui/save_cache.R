source('modules/setup/file_upload_helpers.R')

saveCacheUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns('button'), container = tags$li, class = 'dropdown')
}

saveCacheServe <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      output$button <- renderUI({
        req(cache())

        if (is.null(cache()$cache_path) || !file.exists(cache()$cache_path)) {
          shinyDirLink(
            session$ns('choose_dir'),
            label = 'Set Cache Directory',
            title = '',
            icon = icon('folder-open')
          )
        } else {
          actionLink(
            inputId = session$ns('save'),
            label = 'Save Cache',
            icon = icon('save')
          )
        }
      })

      shinyDirChoose(input, 'choose_dir', roots = getRoots(), allowDirCreate = TRUE)

      # Set directory logic
      selected_dir <- eventReactive(input$choose_dir, {
        req(cache(), input$choose_dir)
        parseDirPath(getRoots(), input$choose_dir)
      })

      observeEvent(selected_dir(), {
        req(cache())

        path <- selected_dir()
        if (is.null(path) || length(path) == 0 || nchar(path) == 0) {
          showNotification("Invalid directory selected. Try again.", type = "error")
          return()
        }

        # Set the cache path
        cache()$set_cache_path(file.path(path, paste0(cache()$country, '_', format(Sys.time(), '%Y%m%d%H%M'), ".rds")))
        showNotification("Directory set successfully!", type = "message")
      })

      observeEvent(input$save, {
        req(cache())
        cache()$save_to_disk()
        showNotification("Cache saved successfully!", type = "message")
      })

    }
  )
}
