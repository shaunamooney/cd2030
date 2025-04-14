source('modules/setup/file_upload_helpers.R')
source('modules/setup/modal_helpers.R')
source('ui/directory-input.R')

fileUploadUI <- function(id, i18n) {
  ns <- NS(id)

  box(
    title = i18n$t("title_upload_survey"),
    status = 'success',
    solidHeader = TRUE,
    width = 12,

    fluidRow(
      column(
        6,
        fileInput(
          inputId = ns('un_data'),
          label = i18n$t("title_upload_un_estimates"),
          buttonLabel = i18n$t("btn_browse_or_drop"),
          placeholder = 'Supported formats: .dta',
          accept = '.dta'
        ),

        messageBoxUI(ns('un_feedback'))
      ),
      column(
        6,
        fileInput(
          inputId = ns('wuenic_data'),
          label = i18n$t("title_upload_wuenic"),
          buttonLabel = i18n$t("btn_browse_or_drop"),
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
          label = i18n$t("title_upload_survey"),
          buttonLabel = i18n$t("btn_browse_or_drop"),
          accept = '.dta'
        ),

        messageBoxUI(ns('selected_dir_feedback'))
      ),
      column(
        6,
        tags$label(i18n$t("title_subnational_mapping_opt"), style = 'font-weight: bold; margin-top: 10px; margin-bottom: 5px; display: block;'),
        fluidRow(
          column(6, mappingModalUI(ns('survey_data'), i18n$t('btn_map_survey'))),
          column(6, mappingModalUI(ns('map_data'), i18n$t('btn_map_mapping'))),
          column(12, messageBoxUI(ns('survey_feedback'))),
          column(12, messageBoxUI(ns('map_feedback')))
        )
      )
    )
  )
}

fileUploadServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      un_message_box <- messageBoxServer('un_feedback', i18n = i18n)
      wuenic_message_box <- messageBoxServer('wuenic_feedback', i18n = i18n)
      survey_message_box <- messageBoxServer('survey_feedback', i18n = i18n)
      map_message_box <- messageBoxServer('map_feedback', i18n = i18n)
      selected_dir_box <- messageBoxServer('selected_dir_feedback', 'Survey folder not set', i18n = i18n)

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

          un_message_box$update_message(str_glue(i18n$t('msg_upload_success')), 'success')
        },
        error = function(e) {
          un_message_box$update_message(i18n$t('error_upload_failed_unsupported_format'), 'error')
        })
      })

      observeEvent(data(), {
        req(data())
        state$loaded <- FALSE
        if (is.null(cache()$national_survey)) {
          shinyjs::reset('directory_select')
        }
        if (is.null(cache()$un_estimates)) {
          shinyjs::reset('un_data')
        }
        if (is.null(cache()$wuenic_estimates)) {
          shinyjs::reset('wuenic_data')
        }
      })

      observe({
        req(data(), !state$loaded)
        if (is.null(input$un_data$name) && !is.null(cache()$un_estimates)) {
          un_message_box$update_message(paste('Upload successful: File loaded from cache'), 'success')
        } else {
          un_message_box$update_message(i18n$t('msg_awaiting_upload'), 'info')
        }
      })

      observeEvent(input$wuenic_data, {
        req(cache())

        file_name <- input$wuenic_data$name

        tryCatch({
          wuenic <- load_wuenic_data(input$wuenic_data$datapath, country_iso())
          cache()$set_wuenic_estimates(wuenic)
          wuenic_message_box$update_message(str_glue(i18n$t("msg_upload_success")), 'success')
        },
        error = function(e) {
          wuenic_message_box$update_message(i18n$t('error_upload_failed_unsupported_format'), 'error')
        })
      })

      observe({
        req(data(), !state$loaded)
        if (is.null(input$wuenic_data$name) && !is.null(cache()$wuenic_estimates)) {
          wuenic_message_box$update_message(paste('Upload successful: File loaded from cache'), 'success')
        } else {
          wuenic_message_box$update_message(i18n$t('msg_awaiting_upload'), 'info')
        }
      })

      observe({
        req(data(), !state$loaded)

        state$loaded <- TRUE

        if (all(!is.null(cache()$national_survey), !is.null(cache()$regional_survey),
                !is.null(cache()$wiq_survey), !is.null(cache()$area_survey),
                !is.null(cache()$education_survey))) {
          selected_dir_box$update_message('Loading from cache', 'success')
        } else {
          selected_dir_box$update_message(i18n$t('msg_awaiting_upload'), 'info')
        }

        if (!is.null(cache()$national_survey)) {
          selected_dir_box$add_message('Loaded national survey data from cache.', 'success')
        }
        if (!is.null(cache()$regional_survey)) {
          selected_dir_box$add_message('Loaded regional survey data from cache.', 'success')
        }
        if (!is.null(cache()$wiq_survey)) {
          selected_dir_box$add_message('Loaded wealth index quintile survey data from cache.', 'success')
        }
        if (!is.null(cache()$area_survey)) {
          selected_dir_box$add_message('Loaded area survey data from cache.', 'success')
        }
        if (!is.null(cache()$education_survey)) {
          selected_dir_box$add_message('Loaded maternal education survey data from cache.', 'success')
        }
      })

      observeEvent(input$directory_select, {
        req(cache(), input$directory_select)

        required_files <- c(
          'all_data.dta',               # National data
          'gregion_data.dta',           # Regional data
          'area_data.wide.dta',         # Area data
          'meduc_data.wide.dta',        # Maternal education data
          'wiq_data.wide.dta'           # Wealth index quintile data
        )
        country <- gsub(' ', '_', country())
        required_files <- gsub('data', country, required_files)

        uploaded_files <- input$directory_select %>%
          filter(name %in% required_files)

        missing_files <- setdiff(required_files, uploaded_files$name)

        selected_dir_box$update_message('Files uploaded.', 'success')

        # Log missing files and exit if any are not found
        if (length(missing_files) > 0) {
          msg <- paste('Missing required files:', paste(missing_files, collapse = ', '))
          selected_dir_box$add_message(msg, 'error')
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
                selected_dir_box$add_message(clean_error_message(e), 'error')
                return()
              }
            )

            if (grepl('^all_', .x$name)) {
              survdata <- load_survey_data(file_path, country_iso())
              cache()$set_national_survey(survdata)
              new_log <- paste0('Loaded national survey data: ', .x$name, '.')
            } else if (grepl('^gregion_', .x$name)) {
              gregion <- load_survey_data(file_path, country_iso(), admin_level = 'adminlevel_1')
              cache()$set_regional_survey(gregion)
              new_log <- paste0('Loaded regional survey data: ', .x$name, '.')
            } else if (grepl('^area_', .x$name)) {
              area <- load_equity_data(file_path)
              cache()$set_area_survey(area)
              new_log <- paste0('Loaded area survey data: ', .x$name, '.')
            } else if (grepl('^meduc_', .x$name)) {
              educ <- load_equity_data(file_path)
              cache()$set_education_survey(educ)
              new_log <- paste0('Loaded maternal education survey data: ', .x$name, '.')
            } else if (grepl('^wiq_', .x$name)) {
              wiq <- load_equity_data(file_path)
              cache()$set_wiq_survey(wiq)
              new_log <- paste0('Loaded wealth index quintile survey data: ', .x$name, '.')
            } else {
              new_log <- paste0('File "', .x$name, '" does not match any known pattern.')
            }

            selected_dir_box$add_message(new_log, 'success')
          },
          error = function(e) {
            clean_message <- clean_error_message(e)
            new_log <- paste0('Error processing file ', .x$name, ': ', clean_message)
            selected_dir_box$add_message(new_log, 'error')
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
          survey_message_box$update_message(i18n$t('msg_survey_mapping_not_done'), 'info')
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
          map_message_box$update_message(i18n$t('msg_map_mapping_not_done'), 'info')
        } else {
          map_message_box$update_message(i18n$t('msg_map_mapping_uploaded'), 'success')
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
    }
  )
}
