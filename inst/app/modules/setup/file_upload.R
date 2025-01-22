source('modules/setup/file_upload_helpers.R')
source('modules/setup/modal_helpers.R')
source('ui/directory-input.R')

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
        directoryInput(
          ns('directory_select'),
          label = 'Upload Survey Data',
          buttonLabel = 'Browse or Drop...',
          accept = '.dta'
        ),
        verbatimTextOutput(ns('selected_dir')) %>%
          tagAppendAttributes(style = "border: 1px solid #ddd; padding: 5px; border-radius: 4px; margin-top: 10px")
      ),
      column(
        6,
        tags$label("Subnational Manual Data Mapping (Optional)", style = 'font-weight: bold; margin-top: 10px; margin-bottom: 5px; display: block;'),
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
        cache()$countdown_data
      })

      country <- reactive({
        req(cache())
        cache()$country
      })

      country_iso <- reactive({
        req(cache())
        cache()$country_iso
      })

      observeEvent(input$un_data, {
        req(data(), input$un_data)
        file_name <- input$un_data$name

        tryCatch({
          start_year <- min(data()$year)
          end_year <- max(data()$year)
          un <- load_un_estimates(input$un_data$datapath, country_iso(), start_year, end_year)
          cache()$set_un_estimates(un)
          un_message_box$update_message(paste("Upload successful: File", file_name, "is ready."), 'success')
        },
        error = function(e) {
          un_message_box$update_message("Upload failed: Check the file format and try again.", 'error')
        })
      })

      observeEvent(data(), {
        req(data())
        state$loaded <- FALSE
      })

      observe({
        req(data(), !state$loaded)
        if (is.null(input$un_data$name) && !is.null(cache()$un_estimates)) {
          un_message_box$update_message(paste("Upload successful: File loaded from cache"), 'success')
        } else {
          un_message_box$update_message(paste("Awaiting file upload..."), 'info')
        }
      })

      observeEvent(input$wuenic_data, {
        req(cache())

        file_name <- input$wuenic_data$name

        tryCatch({
          wuenic <- load_wuenic_data(input$wuenic_data$datapath, country_iso())
          cache()$set_wuenic_estimates(wuenic)
          wuenic_message_box$update_message(paste("Upload successful: File", file_name, "is ready."), 'success')
        },
        error = function(e) {
          wuenic_message_box$update_message("Upload failed: Check the file format and try again.", 'error')
        })
      })

      observe({
        req(data(), !state$loaded)
        if (is.null(input$wuenic_data$name) && !is.null(cache()$wuenic_estimates)) {
          wuenic_message_box$update_message(paste("Upload successful: File loaded from cache"), 'success')
        } else {
          wuenic_message_box$update_message(paste("Awaiting file upload..."), 'info')
        }
      })

      observe({
        req(data(), !state$loaded)

        state$loaded <- TRUE

        log_messages(NULL)
        if (!is.null(cache()$national_survey)) {
          new_log <- paste0('Loaded national survey data from cache.')
          log_messages(paste(log_messages(), new_log, sep = "\n"))
        }
        if (!is.null(cache()$regional_survey)) {
          new_log <- paste0('Loaded regional survey data from cache.')
          log_messages(paste(log_messages(), new_log, sep = "\n"))
        }
        if (!is.null(cache()$wiq_survey)) {
          new_log <- paste0('Loaded wealth index quintile survey data from cache.')
          log_messages(paste(log_messages(), new_log, sep = "\n"))
        }
        if (!is.null(cache()$area_survey)) {
          new_log <- paste0('Loaded area survey data from cache.')
          log_messages(paste(log_messages(), new_log, sep = "\n"))
        }
        if (!is.null(cache()$education_survey)) {
          new_log <- paste0('Loaded maternal education survey data from cache.')
          log_messages(paste(log_messages(), new_log, sep = "\n"))
        }

        if (is.null(log_messages())) {
          log_messages('Survey folder not set')
        }
      })

      observeEvent(input$directory_select, {
        req(cache(), input$directory_select)

        required_files <- c(
          "all_data.dta",               # National data
          "gregion_data.dta",           # Regional data
          "area_data.wide.dta",         # Area data
          "meduc_data.wide.dta",        # Maternal education data
          "wiq_data.wide.dta"           # Wealth index quintile data
        )
        country <- gsub(' ', '_', country())
        required_files <- gsub('data', country, required_files)

        uploaded_files <- input$directory_select %>%
          filter(name %in% required_files)

        missing_files <- setdiff(required_files, uploaded_files$name)

        log_messages('Files uploaded')

        # Log missing files and exit if any are not found
        if (length(missing_files) > 0) {
          log_messages(paste(
            log_messages(),
            paste("Missing required files:", paste(missing_files, collapse = ", ")),
            sep = "\n"
          ))
          return()  # Exit early if required files are missing
        }

        uploaded_files %>%
          split(seq_len(nrow(.))) %>%
          walk(~ {

          tryCatch({
            file_path <- .x$datapath

            tryCatch(
              cd2030:::check_file_path(file_path),
              error = function(e) {
                new_log <- clean_error_message(e)
                log_messages(paste(log_messages(), new_log, sep = "\n"))
                return()
              }
            )

            if (grepl('^all_', .x$name)) {
              survdata <- load_survey_data(file_path, country_iso())
              cache()$set_national_survey(survdata)
              new_log <- paste0('Loaded national survey data: "', .x$name, '".')
            } else if (grepl('^gregion_', .x$name)) {
              gregion <- load_survey_data(file_path, country_iso(), admin_level = 'adminlevel_1')
              cache()$set_regional_survey(gregion)
              new_log <- paste0('Loaded regional survey data: "', .x$name, '".')
            } else if (grepl('^area_', .x$name)) {
              area <- load_equity_data(file_path)
              cache()$set_area_survey(area)
              new_log <- paste0('Loaded area survey data: "', .x$name, '".')
            } else if (grepl('^meduc_', .x$name)) {
              educ <- load_equity_data(file_path)
              cache()$set_education_survey(educ)
              new_log <- paste0('Loaded maternal education survey data: "', .x$name, '".')
            } else if (grepl('^wiq_', .x$name)) {
              wiq <- load_equity_data(file_path)
              cache()$set_wiq_survey(wiq)
              new_log <- paste0('Loaded wealth index quintile survey data: "', .x$name, '".')
            } else {
              new_log <- paste0('File "', .x$name, '" does not match any known pattern.')
            }

            log_messages(paste(log_messages(), new_log, sep = "\n"))
          },
          error = function(e) {
            clean_message <- clean_error_message(e)
            new_log <- paste0('Error processing file "', .x$name, '": ', clean_message)
            log_messages(paste(log_messages(), new_log, sep = "\n"))
          })
        })
      })

      survey_data <- reactive({
        req(data())
        cache()$regional_survey
      })

      survey_map <- reactive({
        req(data())
        cache()$survey_mapping
      })

      observe({
        req(data())
        if (is.null(survey_map())) {
          survey_message_box$update_message('Survey mapping has not been done', 'info')
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
        cache()$map_mapping
      })

      observe({
        req(data())
        if (is.null(map_map())) {
          map_message_box$update_message('Map mapping has not been done', 'info')
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
