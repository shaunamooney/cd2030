source('modules/setup/file_upload_helpers.R')
source('modules/setup/modal_helpers.R')

fileUploadUI <- function(id) {
  ns <- NS(id)

  box(
    title = 'Upload Survey Data',
    status = 'success',
    solidHeader = TRUE,
    width = 12,

    fluidRow(
      column(
        6,
        fileInput(
          inputId = ns('un_data'),
          label = 'Upload UN Estimates data',
          buttonLabel = 'Browse or Drop...',
          placeholder = 'Supported formats: .dta',
          accept = '.dta'
        ),

        messageBoxUI(ns('un_feedback'))
      ),
      column(
        6,
        fileInput(
          inputId = ns('wuenic_data'),
          label = 'Upload WUENIC estimates',
          buttonLabel = 'Browse or Drop...',
          placeholder = 'Supported formats: .dta',
          accept = '.dta'
        ),

        messageBoxUI(ns('wuenic_feedback'))
      )
    ),
    fluidRow(
      column(
        6,
        tags$label("Survey Folder", style = 'font-weight: bold; margin-top: 10px; margin-bottom: 5px; display: block;'),
        shinyDirButton(
          id = ns('directory'),
          label = 'Browse or Select...',
          title = 'Choose a survey folder'
        ),

        verbatimTextOutput(ns('selected_dir')) %>%
          tagAppendAttributes(style = "border: 1px solid #ddd; padding: 5px; border-radius: 4px; margin-top: 10px")
      ),
      column(
        6,
        tags$label("Subnational Manual Data Mapping", style = 'font-weight: bold; margin-top: 10px; margin-bottom: 5px; display: block;'),
        fluidRow(
          column(6, mappingModalUI(ns('survey_data'), 'Map Survey Data')),
          column(6, mappingModalUI(ns('map_data'), 'Map Mapping Data')),
          column(12, messageBoxUI(ns('survey_feedback'))),
          column(12, messageBoxUI(ns('map_feedback')))
        )
      )
    )
  )
}

fileUploadServer <- function(id, cache) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      un_message_box <- messageBoxServer('un_feedback')
      wuenic_message_box <- messageBoxServer('wuenic_feedback')
      survey_message_box <- messageBoxServer('survey_feedback')
      map_message_box <- messageBoxServer('map_feedback')
      log_messages <- reactiveVal('Survey folder not set')

      state <- reactiveValues(loaded = FALSE)

      data <- reactive({
        req(cache())
        cache()$get_data()
      })

      country_iso <- reactive({
        req(cache())
        cache()$get_country_iso()
      })

      shinyDirChoose(input, 'directory', roots = getRoots(), allowDirCreate = FALSE,
                     filetypes = c('', 'dta'))

      survey_file_list <- reactive({
        req(cache())
        prefix <- list('all_country.dta', 'gregion_country.dta', 'area_country.wide.dta', 'meduc_country.wide.dta', 'wiq_country.wide.dta')
        gsub('country', cache()$get_country(), prefix)
      })

      selected_dir <- eventReactive(input$directory, {
        req(input$directory, cache())

        path <- parseDirPath(getRoots(), input$directory)

        if (!is.null(path) && length(path) > 0 && nchar(path) > 0) {
          log_messages(paste0('Survey Folder: ', path))
          return(path)
        } else {
          log_messages('Survey folder not set')
          return(NULL)
        }
      })

      observeEvent(input$un_data, {
        req(data(), input$un_data)
        file_name <- input$un_data$name

        tryCatch({
          start_year <- min(data()$year)
          end_year <- max(data()$year)
          dt <- load_un_estimates(input$un_data$datapath, country_iso(), start_year, end_year)
          cache()$set_un_estimates(dt)
          un_message_box$update_message(paste("Upload successful: File", file_name, "is ready."), 'success')
        },
        error = function(e) {
          print(clean_error_message(e))
          un_message_box$update_message("Upload failed: Check the file format and try again.", 'error')
        })
      })

      observeEvent(data(), {
        req(data())
        state$loaded <- FALSE
      })

      observe({
        req(data(), !state$loaded)
        if (is.null(input$un_data$name) && !is.null(cache()$get_un_estimates())) {
          un_message_box$update_message(paste("Upload successful: File loaded from cache"), 'success')
        } else {
          un_message_box$update_message(paste("Awaiting file upload..."), 'info')
        }
      })

      observeEvent(input$wuenic_data, {
        req(cache())

        file_name <- input$wuenic_data$name

        tryCatch({
          dt <- load_wuenic_data(input$wuenic_data$datapath, country_iso())
          cache()$set_wuenic_estimates(dt)
          wuenic_message_box$update_message(paste("Upload successful: File", file_name, "is ready."), 'success')
        },
        error = function(e) {
          wuenic_message_box$update_message("Upload failed: Check the file format and try again.", 'error')
        })
      })

      observe({
        req(data(), !state$loaded)
        if (is.null(input$wuenic_data$name) && !is.null(cache()$get_wuenic_estimates())) {
          wuenic_message_box$update_message(paste("Upload successful: File loaded from cache"), 'success')
        } else {
          wuenic_message_box$update_message(paste("Awaiting file upload..."), 'info')
        }
      })

      observe({
        req(data(), !state$loaded)

        state$loaded <- TRUE

        log_messages(NULL)
        if (!is.null(cache()$get_national_survey())) {
          new_log <- paste0('Loaded national survey data from cache.')
          log_messages(paste(log_messages(), new_log, sep = "\n"))
        }
        if (!is.null(cache()$get_regional_survey())) {
          new_log <- paste0('Loaded regional survey data from cache.')
          log_messages(paste(log_messages(), new_log, sep = "\n"))
        }
        if (!is.null(cache()$get_wiq_survey())) {
          new_log <- paste0('Loaded wealth index quintile survey data from cache.')
          log_messages(paste(log_messages(), new_log, sep = "\n"))
        }
        if (!is.null(cache()$get_area_survey())) {
          new_log <- paste0('Loaded area survey data from cache.')
          log_messages(paste(log_messages(), new_log, sep = "\n"))
        }
        if (!is.null(cache()$get_education_survey())) {
          new_log <- paste0('Loaded maternal education survey data from cache.')
          log_messages(paste(log_messages(), new_log, sep = "\n"))
        }

        if (is.null(log_messages())) {
          log_messages('Survey folder not set')
        }
      })

      observeEvent(selected_dir(), {
        req(cache())

        walk(isolate(survey_file_list()), ~ {

          tryCatch({
            file_path <- file.path(selected_dir(), .x)

            tryCatch(
              check_file_path(file_path),
              error = function(e) {
                new_log <- clean_error_message(e)
                log_messages(paste(log_messages(), new_log, sep = "\n"))
                return()
              }
            )

            if (grepl('^all_', .x)) {
              nat_survey <- load_survey_data(file_path, country_iso())
              cache()$set_national_survey(nat_survey)
              new_log <- paste0('Loaded national survey data: "', .x, '".')
            } else if (grepl('^gregion_', .x)) {
              gregion <- load_survey_data(file_path, country_iso(), admin_level = 'adminlevel_1')
              cache()$set_regional_survey(gregion)
              new_log <- paste0('Loaded regional survey data: "', .x, '".')
            } else if (grepl('^area_', .x)) {
              area <- load_equity_data(file_path)
              cache()$set_area_survey(area)
              new_log <- paste0('Loaded area survey data: "', .x, '".')
            } else if (grepl('^meduc_', .x)) {
              meduc <- load_equity_data(file_path)
              cache()$set_education_survey(meduc)
              new_log <- paste0('Loaded maternal education survey data: "', .x, '".')
            } else if (grepl('^wiq', .x)) {
              wiq <- load_equity_data(file_path)
              cache()$set_wiq_survey(wiq)
              new_log <- paste0('Loaded wealth index quintile survey data: "', .x, '".')
            } else {
              new_log <- paste0('File "', .x, '" does not match any known pattern.')
            }

            log_messages(paste(log_messages(), new_log, sep = "\n"))
          },
          error = function(e) {
            clean_message <- clean_error_message(e)
            new_log <- paste0('Error processing file "', .x, '": ', clean_message)
            log_messages(paste(log_messages(), new_log, sep = "\n"))
          })
        })
      })

      survey_data <- reactive({
        req(data())
        cache()$get_regional_survey()
      })

      survey_map <- reactive({
        req(data())
        cache()$get_survey_mapping()
      })

      observe({
        req(data())
        if (is.null(survey_map())) {
          survey_message_box$update_message('Survey mapping data is not uploaded', 'info')
        } else {
          survey_message_box$update_message('Survey mapping data is uploaded', 'success')
        }
      })

      mappingModalServer(
        id = 'survey_data',
        cache = cache,
        survey_data = survey_data,
        survey_map = survey_map,
        title = 'Manual Mapping of Survey Data',
        show_col = 'adminlevel_1',
        type = 'survey_mapping'
      )

      map_data <- reactive({
        req(country_iso())
        get_country_shapefile(country_iso(), 'admin_level_1')
      })

      map_map <- reactive({
        req(data())
        cache()$get_map_mapping()
      })

      observe({
        req(data())
        if (is.null(map_map())) {
          map_message_box$update_message('Map mapping data is not uploaded', 'info')
        } else {
          map_message_box$update_message('Map mapping data is uploaded', 'success')
        }
      })

      mappingModalServer(
        id = 'map_data',
        cache = cache,
        survey_data = map_data,
        survey_map = map_map,
        title = 'Manual Mapping of Map Data',
        show_col = 'NAME_1',
        type = 'map_mapping'
      )

      output$selected_dir <- renderText({
        log_messages()
      })

    }
  )
}
