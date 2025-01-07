uploadBoxUI <- function(id) {
  ns <- NS(id)

  box(
    title = 'Upload Data',
    status = 'success',
    solidHeader = TRUE,
    width = 12,
    fluidRow(
      column(3, h4(icon('upload'), 'Upload')),
      column(2, offset = 7, helpButtonUI(ns('upload_data')), align = 'right')
    ),
    fluidRow(
      column(
        12,
        fileInput(
          inputId = ns('hfd_file'),
          label = 'Upload HFD data',
          buttonLabel = 'Browse or Drop...',
          placeholder = 'Supported formats: .xls, .xlsx, .dta, .rds',
          accept = c('.xls', '.xlsx', '.dta', '.rds')
        ),

        messageBoxUI(ns('feedback'))
      )
    ),
    fluidRow(
      column(4, downloadButtonUI(ns('download_data')))
    )
  )
}

uploadBoxServer <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      messageBox <- messageBoxServer('feedback')

      cache <- eventReactive(input$hfd_file, {
        req(input$hfd_file)

        file_path <- input$hfd_file$datapath
        file_name <- input$hfd_file$name
        file_type <- tools::file_ext(file_name)

        valid_types <- c('xls', 'xlsx', 'dta', 'rds')
        if (!file_type %in% valid_types) {
          messageBox$update_message('Upload failed: Unsupported file format.', 'error')
          return(NULL)
        }

        tryCatch({
          dt <- if (file_type %in% c('xls', 'xlsx')) {
            load_excel_data(file_path)
          } else if (file_type == 'dta') {
            load_data(file_path)
          } else {
            NULL
          }

          rds_path <- if (file_type == 'rds') {
            file_path
          } else {
            NULL
          }

          messageBox$update_message(
            paste("Upload successful: File", file_name, "is ready."),
            'success'
          )

          cache_instance <- init_CacheConnection(
            countdown_data = dt,
            rds_path = rds_path
          )$reactive()

          cache_instance()
        },
        error = function(e) {
          clean_message <- clean_error_message(e)
          messageBox$update_message(
            paste("Upload failed: ", clean_message),
            'error'
          )
          NULL
        })
      })

      downloadButtonServer(
        id = 'download_data',
        filename = 'master_dataset',
        extension = 'dta',
        content = function(file) {
          haven::write_dta(cache()$get_data(), file)
        },
        data = cache,
        label = "Download Master Dataset"
      )

      helpButtonServer(
        id = 'upload_data',
        title = 'Upload Data',
        md_file = 'load_data_upload_files.md'
      )

      return(cache)
    }
  )
}
