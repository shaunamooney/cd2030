dhis2BoxUI <- function(id) {
  ns <- NS(id)

  box(title = 'Import from DHIS2',
      status = 'info',
      solidHeader = TRUE,
      width = 12,
      fluidRow(
        column(3, offset = 9, helpButtonUI(ns('dhis2_download')))
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
        column(4, downloadButton(ns('master_file'), 'Download Master File', style = 'color:#2196F3;width:100%;margin-top:10px;')),
        column(4, downloadButton(ns('excel_file'), 'Download Excel File', style = 'color:#2196F3;width:100%;margin-top:10px;'))
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

      data <- reactive({
        req(input$login)

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

          dt <- cd2030::get_dhis2_hfd(input$country, start_date, end_date, timeout = 300)

          khis_cred_clear()

          file_status(list(
            message = "Saving data...",
            color = "blue"
          ))

          dhis2_data(dt)

          dt <- cd2030::save_dhis2_master_data(dt)

          file_status(list(
            message = "Data successfully downloaded!",
            color = "green"
          ))

          return(dt)
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

      helpButtonServer('dhis2_download',
                       'DHIS2 Download',
                       'l',
                       '1_dhis2_login.md')

      output$enhanced_feedback <- renderUI({
        status <- file_status()
        tags$div(
          style = paste("color:", status$color, "; font-weight: bold;"),
          status$message
        )
      })

      output$master_file <- downloadHandler(
        filename = function() { paste0("master_dataset", Sys.Date(), ".dta") },
        content = function(file) {
          req(data())
          save_data(data(), file)
        }
      )

      output$excel_file <- downloadHandler(
        filename = function() { paste0("Country", Sys.Date(), ".xlsx") },
        content = function(file) {
          req(data())
          save_dhis2_excel(dhis2_data(), file)
        }
      )

      return(data)
    }
  )
}
