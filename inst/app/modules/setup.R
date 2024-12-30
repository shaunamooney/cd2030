setupUI <- function(id) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('analysis_setup'), 'Analysis Setup'),
    contentBody(
      box(
        title = 'Setup National Rates',
        status = 'success',
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          column(3, offset = 1, numericInput(ns('neonatal_mortality_rate'), 'Neonatal Mortality Rate',
                                            min = 0, max = 0.05, value = 0.025, step = 0.001)),
          column(3, offset = 0, numericInput(ns('post_neonatal_mortality_rate'), 'Post Neonatal Mortality Rate',
                                            min = 0, max = 0.05, value = 0.024, step = 0.001)),
          column(3, offset = 0, numericInput(ns('twin_rate'), 'Twin Rate',
                                            min = 0, max = 0.05, value = 0.015, step = 0.001))
        ),
        fluidRow(
          column(3, offset = 1, numericInput(ns('pregnancy_loss'), 'Pregnancy Loss',
                                min = 0, max = 0.05, value = 0.03, step = 0.001)),
          column(3, offset = 0, numericInput(ns('stillbirth_rate'), 'Still Birth Rate',
                                min = 0, max = 0.05, value = 0.02, step = 0.001)),
          column(3, offset = 0, numericInput(ns('penta1_mortality_rate'), 'ANC1 to Penta1 Mortality Rate',
                                min = 0, max = 0.05, value = 0.025, step = 0.001))
        ),
        fluidRow(
          column(3, offset = 1, numericInput(ns('anc1_prop'), 'ANC1 Survey',
                                            min = 0, max = 100, value = 0, step = 1)),
          column(3, offset = 0, numericInput(ns('penta1_prop'), 'Penta1 Survey',
                                            min = 0, max = 100, value = 0, step = 1)),
        )
      ),

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

            uiOutput(ns("un_enhanced_feedback")) %>%  tagAppendAttributes(style = "margin-top: 10px;")
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

            uiOutput(ns("wuenic_enhanced_feedback")) %>%  tagAppendAttributes(style = "margin-top: 10px;")
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
    )
  )
}

setupServer <- function(id, data, survey_data) {
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(survey_data))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      log_messages <- reactiveVal('Survey folder not set')

      un_path <- reactiveVal()
      wuenic_path <- reactiveVal()
      surv_path <- reactiveVal()
      gregion_path <- reactiveVal()
      area_path <- reactiveVal()
      meduc_path <- reactiveVal()
      wiq_path <- reactiveVal()

      un_estimates <- reactiveVal()
      wuenic_data <- reactiveVal()
      survdata <- reactiveVal()
      gregion_data <- reactiveVal()
      area_data <- reactiveVal()
      meduc_data <- reactiveVal()
      wiq_data <- reactiveVal()
      survey_mapping <- reactiveVal()
      map_mapping <- reactiveVal()

      upload_status <- reactiveValues(un =list(message = "Awaiting file upload...", color = "gray"),
                                      wuenic = list(message = "Awaiting file upload...", color = "gray"))

      country <- reactive({
        req(data())
        attr(data(), 'country')
      })

      country_iso <- reactive({
        req(data())
        attr(data(), 'iso3')
      })

      # Set up roots for Windows, macOS, and Linux systems
      roots <- if (.Platform$OS.type == "windows") {
        drives <- system("wmic logicaldisk get name", intern = TRUE)
        drives <- gsub("\\s+", "", drives)  # Trim whitespace
        drives <- drives[nchar(drives) > 0] # Remove empty strings
        drives <- drives[grepl(":", drives)]
        set_names(drives, drives)
      } else {
        c(home = "~", root = "/") # For macOS and Linux
      }

      shinyDirChoose(input, 'directory', roots = roots, allowDirCreate = FALSE,
                     filetypes = c('', 'dta'))

      selected_dir <- eventReactive(input$directory, {
        req(input$directory)

        path <- parseDirPath(roots, input$directory)

        if (!is.null(path) && length(path) > 0 && nchar(path) > 0) {
          log_messages(paste0('Survey Folder: ', path))
          return(path)
        } else {
          log_messages('Survey folder not set')
          return(NULL)
        }
      })


      survey_file_list <- reactive({
        req(country())
        prefix <- list('all_country.dta', 'gregion_country.dta', 'area_country.wide.dta', 'meduc_country.wide.dta', 'wiq_country.wide.dta')
        gsub('country', country(), prefix)
      })

      observeEvent(selected_dir(), {

        walk(isolate(survey_file_list()), ~ {

          tryCatch({
            file_path <- file.path(selected_dir(), .x)

            if (!file.exists(file_path)) {
              new_log <- paste0('Error: File "', .x, '" was not found in the folder.')
              log_messages(paste(log_messages(), new_log, sep = "\n"))
              return()
            }

            if (grepl('^all_', .x)) {
              surv_path(file_path)
              nat_survey <- load_survey_data(surv_path(), country_iso())
              survdata(nat_survey)
              new_log <- paste0('Loaded national survey data: "', .x, '".')
            } else if (grepl('^gregion_', .x)) {
              gregion_path(file_path)
              gregion <- load_survey_data(gregion_path(), country_iso(), admin_level = 'adminlevel_1')
              gregion_data(gregion)
              new_log <- paste0('Loaded regional survey data: "', .x, '".')
            } else if (grepl('^area_', .x)) {
              area_path(file_path)
              area <- load_equity_data(area_path())
              area_data(area)
              new_log <- paste0('Loaded area survey data: "', .x, '".')
            } else if (grepl('^meduc_', .x)) {
              meduc_path(file_path)
              meduc <- load_equity_data(meduc_path())
              meduc_data(meduc)
              new_log <- paste0('Loaded maternal education survey data: "', .x, '".')
            } else if (grepl('^wiq', .x)) {
              wiq_path(file_path)
              wiq <- load_equity_data(wiq_path())
              wiq_data(wiq)
              new_log <- paste0('Loaded wealth index quintile survey data: "', .x, '".')
            } else {
              new_log <- paste0('File "', .x, '" does not match any known pattern.')
            }

            log_messages(paste(log_messages(), new_log, sep = "\n"))
          }, error = function(e) {
            new_log <- paste0('Error processing file "', .x, '": ', e$message)
            log_messages(paste(log_messages(), new_log, sep = "\n"))
          })
        })
      })

      gregion_levels <- reactive({
        req(data())

        data() %>%
          distinct(adminlevel_1) %>%
          arrange(adminlevel_1) %>%
          pull(adminlevel_1)
      })

      createMappingModal <- function(id, title, gregion, choices, type) {
        showModal(
          modalDialog(
            title = title,
            size = 'l',
            fluidPage(
              map(gregion, ~ {
                tagList(
                  fluidRow(
                    column(6, strong(.x)),
                    column(6, selectizeInput(
                      inputId = NS(id, .x),
                      label = NULL,
                      choices = c('Select' = "", choices),
                      selected = ""
                    ))
                  ),
                  tags$hr(style = "margin: 0 0 10px;")
                )
              })
            ),
            footer = tagList(
              actionButton(NS(id, type), "Save Mapping"),
              modalButton("Cancel")
            )
          ))
      }


      observeEvent(input$survey_data, {
        req(gregion_levels())

        gregion <- gregion_levels()

        dt <- gregion_data() %>%
          distinct(adminlevel_1) %>%
          arrange(adminlevel_1) %>%
          pull(adminlevel_1)

        createMappingModal(id, 'Manual Mapping of Survey Data', gregion, dt, type = 'save_survey_mapping')
      })

      observeEvent(input$save_survey_mapping, {
        mapped_to <- map(gregion_levels(), ~ input[[.x]])
        mapping_result <- tibble(
          admin_level_1 = gregion_levels(),
          adminlevel_1 = unlist(mapped_to, use.names = FALSE)
        )

        survey_mapping(mapping_result)
        removeModal()  # Close the modal dialog
      })

      observeEvent(input$map_data, {
        gregion <- if (country_iso() == 'KEN') {
          data() %>%
            distinct(district) %>%
            arrange(district) %>%
            pull(district)
        } else {
          gregion_levels()
        }

        req(gregion,  data())

        dt <- get_country_shapefile(country_iso(), 'admin_level_1') %>%
          distinct(NAME_1) %>%
          arrange(NAME_1) %>%
          pull(NAME_1)

        createMappingModal(id, 'Manual Mapping of Map Data', gregion, dt, type = 'save_map_mapping')
      })

      observeEvent(input$save_map_mapping, {
        mapped_to <- map(gregion_levels(), ~ input[[.x]])
        mapping_result <- tibble(
          adminlevel_1 = gregion_levels(),
          NAME_1 = unlist(mapped_to, use.names = FALSE)
        )

        map_mapping(mapping_result)
        removeModal()  # Close the modal dialog
      })

      output$selected_dir <- renderText({
        log_messages()
      })

      setup_values <- reactive({
        list(
          rates = list(
            nmr = input$neonatal_mortality_rate,
            pnmr = input$post_neonatal_mortality_rate,
            twin_rate = input$twin_rate,
            preg_loss = input$pregnancy_loss,
            sbr = input$stillbirth_rate,
            penta1_mort_rate = input$penta1_mortality_rate,
            anc1 = input$anc1_prop / 100,
            penta1 = input$penta1_prop / 100
          ),
          data = list(
            un = un_estimates(),
            wuenic = wuenic_data(),
            survdata = survdata(),
            gregion = gregion_data(),
            area = area_data(),
            meduc = meduc_data(),
            wiq = wiq_data(),
            survey_map = survey_mapping(),
            map_map = map_mapping()
          )
        )
      })

      observe({
        req(country_iso())

        dt <- survey_data()

        updateNumericInput(session, 'anc1_prop', value = dt$anc1)
        updateNumericInput(session, 'penta1_prop', value = dt$penta1)
      })

      observeEvent(data(), {
        start_year <- min(data()$year)
        end_year <- max(data()$year)

        if (!is.null(un_path())) {
          un <- load_un_estimates(un_path(), country_iso(), start_year, end_year)
          un_estimates(un)
        }

        if (!is.null(wuenic_path())) {
          wuenic <- load_wuenic_data(wuenic_path(), country_iso())
          wuenic_data(wuenic)
        }
      })

      observeEvent(input$un_data, {
        req(country_iso())

        file_name <- input$un_data$name
        un_path(input$un_data$datapath)

        tryCatch({
          start_year <- min(data()$year)
          end_year <- max(data()$year)
          dt <- load_un_estimates(un_path(), country_iso(), start_year, end_year)

          upload_status$un <- list(
            message = paste("Upload successful: File", file_name, "is ready."),
            color = "darkgreen"
          )
          un_estimates(dt)
        }, error = function(e) {
          upload_status$un <- list(
            message = "Upload failed: Check the file format and try again.",
            color = "red"
          )
          un_estimates(NULL)
        })
      })


      observeEvent(input$wuenic_data, {
        req(country_iso())

        file_name <- input$wuenic_data$name
        wuenic_path(input$wuenic_data$datapath)

        tryCatch({
          dt <- load_wuenic_data(wuenic_path(), country_iso())

          upload_status$wuenic <- list(
            message = paste("Upload successful: File", file_name, "is ready."),
            color = "darkgreen"
          )

          wuenic_data(dt)
        }, error = function(e) {
          upload_status$wuenic <- list(
            message = "Upload failed: Check the file format and try again.",
            color = "red"
          )
          wuenic_data(NULL)
        })
      })

      output$un_enhanced_feedback <- renderUI({
        status <- upload_status$un
        tags$div(
          style = paste("color:", status$color, "; font-weight: bold;"),
          status$message
        )
      })

      output$wuenic_enhanced_feedback <- renderUI({
        status <- upload_status$wuenic
        tags$div(
          style = paste("color:", status$color, "; font-weight: bold;"),
          status$message
        )
      })

      contentHeaderServer(
        'analysis_setup',
        md_title = 'Analysis Setup',
        md_file = '5_upload_data.md'
      )

      return(setup_values)
    }
  )
}
