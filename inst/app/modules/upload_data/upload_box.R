uploadBoxUI <- function(id) {
  ns <- NS(id)

  box(
    title = 'Upload Data',
    status = 'success',
    solidHeader = TRUE,
    width = 12,
    fluidRow(
      column(3, h4(icon('upload'), 'Upload')),
      column(2, offset = 7, helpButtonUI(ns('upload_data')))
    ),
    fluidRow(
      column(
        12,
        fileInput(
          inputId = ns('hfd_file'),
          label = 'Upload HFD data',
          buttonLabel = 'Browse or Drop...',
          placeholder = 'Supported formats: .xls, .xlsx, .dta, .rds'
        ),

        uiOutput(ns("enhanced_feedback"))
      )
    ),
    fluidRow(
      column(
        3,
        downloadButton(ns('download_data'), label = "Download Master Dataset", style = "color:#2196F3;width:100%;margin-top:10px;")
      )
    )
  )
}

uploadBoxServer <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      file_status <- reactiveVal(list(message = "Awaiting file upload...", color = "gray"))

      helpButtonServer(
        id = 'upload_data',
        title = 'Upload Data',
        size = 'l',
        file = '1_upload_data.md'
      )

      data <- reactive({
        req(input$hfd_file)

        file_path <- input$hfd_file$datapath
        file_name <- input$hfd_file$name
        file_type <- tools::file_ext(file_name)

        valid_types <- c('xls', 'xlsx', 'dta', 'rds')
        if (!file_type %in% valid_types) {
          file_status(list(
            message = "Upload failed: Unsupported file format.",
            color = "red"
          ))
        }

        tryCatch({
          dt <- if (file_type %in% c('xls', 'xlsx')) {
            cd2030::load_excel_data(file_path)
          } else {
            cd2030::load_data(file_path)
          }

          file_status(list(
            message = paste("Upload successful: File", file_name, "is ready."),
            color = "darkgreen"
          ))

          return(dt)
        }, error = function(e) {
          file_status(list(
            message = "Upload failed: Check the file format and try again.",
            color = "red"
          ))
          return(NULL)
        })
      })

      output$enhanced_feedback <- renderUI({
        status <- file_status()
        tags$div(
          style = paste("color:", status$color, "; font-weight: bold;"),
          status$message
        )
      })

      output$download_data <- downloadHandler(
        filename = function() { paste0("master_dataset", Sys.Date(), ".dta") },
        content = function(file) {
          req(!is.null(data())) # Ensure data is available
          haven::write_dta(data(), file)
        }
      )

      return(data)
    }
  )
}
