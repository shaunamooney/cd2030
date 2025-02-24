dhis2BoxUI <- function(id) {
  ns <- NS(id)

  box(title = 'Import from DHIS2',
      status = 'info',
      solidHeader = TRUE,
      width = 12,
      fluidRow(
        column(2, offset = 10, helpButtonUI(ns('dhis2_download')), align = 'right')
      ),
      fluidRow(
        column(6, selectizeInput(ns('country'), 'Country', choices = NULL)),
      ),
      fluidRow(
        column(12, dateRangeInput(ns('date'), 'Period',
                                  start = Sys.Date()-1825,
                                  end = Sys.Date(),
                                  format = 'MM yyyy',
                                  separator = ' to ',
                                  startview = "year"))
      ),
      fluidRow(
        column(6, textInput(ns('username'), label = NULL, placeholder = 'DHIS2 User Name')),
        column(6, passwordInput(ns('password'), label = NULL, placeholder = 'DHIS2 Password'))
      ),
      fluidRow(
        column(4, actionButton(ns('login'), 'Login', icon = icon('sign-in-alt'),
                               style = 'background-color: #FFEB3B;font-weight: 500;width:100%;')),

        column(12, uiOutput(ns("enhanced_feedback")))
      ),
      fluidRow(
        column(4, downloadButtonUI(ns('master_file'))),
        column(4, downloadButtonUI(ns('excel_file')))
      )
  )
}

dhis2BoxServer <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      file_status <- reactiveVal(list(message = "Waiting ...", color = "gray"))

      dhis2_data <- reactiveVal()

      countries_df <- reactive({
        cd2030:::countries %>%
          filter(!is.na(dhis2_url))
      })

      observe({
        countries <- countries_df()$iso3
        names(countries) <- countries_df()$country

        countries <- c('Select Country' = '0', countries)
        updateSelectizeInput(session, 'country', choices = countries)
      })

      cache <- eventReactive(input$login, {

        if (input$country == '0' || length(input$country) == 0) {
          file_status(list(
            message = paste0('Error: Country must be selected'),
            color = "red"
          ))
          return(NULL)
        }

        if (input$username == '' || length(input$username) == 0) {
          file_status(list(
            message = paste0('Error: Username must be provided'),
            color = "red"
          ))
          return(NULL)
        }

        if (input$password == '' || length(input$password) == 0) {
          file_status(list(
            message = paste0('Error: Password must be provided'),
            color = "red"
          ))
          return(NULL)
        }

        # updateActionButton(session, 'login', disabled = TRUE)

        file_status(list(
          message = "Connecting to DHIS2...",
          color = "blue"
        ))

        server <- countries_df() %>%
          filter(iso3 == input$country) %>%
          pull(dhis2_url)

        merged_data <- tryCatch({
          khis_cred(username = input$username, password = input$password, server = server)

          file_status(list(
            message = "Fetching data...",
            color = "blue"
          ))

          start_date <- input$date[1]
          end_date <- input$date[2]

          dt <- get_dhis2_hfd(input$country, start_date, end_date, timeout = 300)

          khis_cred_clear()

          file_status(list(
            message = "Saving data...",
            color = "blue"
          ))

          dhis2_data(dt)

          dt <- cd2030::save_dhis2_master_data(dt)

          cache_instance <- init_CacheConnection(
            rds_path = dt,
            countdown_data = NULL
          )$reactive()

          file_status(list(
            message = "Data successfully downloaded!",
            color = "green"
          ))

          return(cache_instance())
        },
        error = function(e) {
          file_status(list(
            message = paste0("Download Error: ", e$message),
            color = "red"
          ))

          return(NULL)
        })

        # updateActionButton(session, 'login', disabled = FALSE)

        return(merged_data)
      })

      output$enhanced_feedback <- renderUI({
        status <- file_status()
        tags$div(
          style = paste("color:", status$color, "; font-weight: bold;",
                        'border: 1px solid ', status$color, ';',
                        'padding: 10px; margin-top: 10px; border-radius: 5px;'),
          status$message
        )
      })

      downloadButtonServer(
        id = 'master_file',
        filename = reactive('master_dataset'),
        extension = reactive('dta'),
        content = function(file) {
          save_data(cache()$countdown_data, file)
        },
        data = cache,
        label = 'Download Master File'
      )

      downloadButtonServer(
        id = 'excel_file',
        filename = reactive('master_dataset'),
        extension = reactive('xlsx'),
        content = function(file) {
          save_dhis2_excel(dhis2_data(), file)
        },
        data = dhis2_data,
        label = 'Download Excel File'
      )

      helpButtonServer(
        id = 'dhis2_download',
        title = 'DHIS2 Download',
        md_file = 'load_data_dhis2_download.md'
      )

      return(cache)
    }
  )
}
