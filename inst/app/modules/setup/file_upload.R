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
      ),
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
        tags$label("Subnational Data Mapping", style = 'font-weight: bold; margin-top: 10px; margin-bottom: 5px; display: block;'),
        fluidRow(
          column(6, actionButton(ns('survey_data'), 'Map Survey Data')),
          column(6, actionButton(ns('map_data'), 'Map Mapping Data'))
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
      log_messages <- reactiveVal('Survey folder not set')

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
        req(cache())
        data <- cache()$get_data()
        file_name <- input$un_data$name
        # un_path(input$un_data$datapath)

        tryCatch({
          start_year <- min(data$year)
          end_year <- max(data$year)
          dt <- load_un_estimates(input$un_data$datapath, cache()$get_country_iso(), start_year, end_year)
          cache()$set_un_estimates(dt)
          un_message_box$update_message(paste("Upload successful: File", file_name, "is ready."), 'success')
        },
        error = function(e) {
          un_message_box$update_message("Upload failed: Check the file format and try again.", 'error')
        })
      })

      observeEvent(input$wuenic_data, {
        req(cache())

        file_name <- input$wuenic_data$name
        # wuenic_path(input$wuenic_data$datapath)

        tryCatch({
          dt <- load_wuenic_data(input$wuenic_data$datapath, cache()$get_country_iso())
          cache()$set_wuenic_estimates(dt)
          wuenic_message_box$update_message(paste("Upload successful: File", file_name, "is ready."), 'success')
        },
        error = function(e) {
          wuenic_message_box$update_message("Upload failed: Check the file format and try again.", 'error')
        })
      })

      observeEvent(selected_dir(), {
        req(cache())

        walk(isolate(survey_file_list()), ~ {

          tryCatch({
            file_path <- file.path(selected_dir(), .x)

            if (!file.exists(file_path)) {
              new_log <- paste0('Error: File "', .x, '" was not found in the folder.')
              log_messages(paste(log_messages(), new_log, sep = "\n"))
              return()
            }

            if (grepl('^all_', .x)) {
              nat_survey <- load_survey_data(file_path, cache()$get_country_iso())
              cache()$set_national_survey(nat_survey)
              new_log <- paste0('Loaded national survey data: "', .x, '".')
            } else if (grepl('^gregion_', .x)) {
              gregion <- load_survey_data(file_path, cache()$get_country_iso(), admin_level = 'adminlevel_1')
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

      gregion_levels <- reactive({
        req(cache())

        print(glimpse(cache()$get_data()))

        cache()$get_data() %>%
          distinct(adminlevel_1) %>%
          arrange(adminlevel_1) %>%
          pull(adminlevel_1)
      })

      observeEvent(input$survey_data, {
        req(gregion_levels(), cache()$get_regional_survey())

        gregion <- gregion_levels()

        dt <- cache()$get_regional_survey() %>%
          distinct(adminlevel_1) %>%
          arrange(adminlevel_1) %>%
          pull(adminlevel_1)
        print(dt)

        createMappingModal('survey_data', 'Manual Mapping of Survey Data', gregion, dt, type = 'save_survey_mapping')
      })

      observeEvent(input$save_survey_mapping, {
        req(cache())

        mapped_to <- map(gregion_levels(), ~ input[[.x]])
        mapping_result <- tibble(
          admin_level_1 = gregion_levels(),
          adminlevel_1 = unlist(mapped_to, use.names = FALSE)
        )
        cache()$set_survey_mapping(mapping_result)
        removeModal()  # Close the modal dialog
      })

      observeEvent(input$map_data, {
        req(cache())

        gregion <- if (cache()$get_country_iso() == 'KEN') {
          cache()$get_data() %>%
            distinct(district) %>%
            arrange(district) %>%
            pull(district)
        } else {
          gregion_levels()
        }

        req(gregion)

        dt <- get_country_shapefile(cache()$get_country_iso(), 'admin_level_1') %>%
          distinct(NAME_1) %>%
          arrange(NAME_1) %>%
          pull(NAME_1)

        createMappingModal('map_data', 'Manual Mapping of Map Data', gregion, dt, type = 'save_map_mapping')
      })

      observeEvent(input$save_map_mapping, {
        req(cache())
        mapped_to <- map(gregion_levels(), ~ input[[.x]])
        mapping_result <- tibble(
          adminlevel_1 = gregion_levels(),
          NAME_1 = unlist(mapped_to, use.names = FALSE)
        )
        cache()$set_map_mapping(mapping_result)
        removeModal()  # Close the modal dialog
      })

      output$selected_dir <- renderText({
        log_messages()
      })

    }
  )
}
