uploadBoxUI <- function(id) {
  ns <- NS(id)

  box(title = 'Upload Data',
      status = 'success',
      solidHeader = TRUE,
      width = 12,
      fluidRow(
        column(3, h4(icon('upload'), 'Upload')),
        column(2, offset = 7, helpButtonUI(ns('upload_data')))
      ),
      fluidRow(
        column(6, fileInput(ns('hfd_file'),
                            label = 'Upload HFD data',
                            buttonLabel = 'Browse or Drop...',
                            placeholder = '.xls, .xlsx, .dta, or .rds')),
        column(6, fileInput(ns('load_saved_data'),
                            label = 'Restore Progress',
                            buttonLabel = 'Browse or Drop...',
                            placeholder = 'Previously saved zip'))
      )

  )
}

uploadBoxServer <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      helpButtonServer('upload_data', 'Upload Data', 'l', '1_upload_data.md')

      data <- reactive({
        req(input$hfd_file)

        file_path <- input$hfd_file$datapath
        file_type <- tools::file_ext(input$hfd_file$name)

        # Read the file based on its format
        if (file_type == 'xls' || file_type == 'xlsx') {
          # Read Excel file
          dt <- cd2030::load_excel_data(file_path)
        } else if (file_type == 'dta' || file_type == 'rds') {
          # Read Stata file
          dt <-  cd2030::load_data(file_path)
        } else {
          stop('Unsupported file format.')
        }

        return(dt)
      })

      return(data)
    }
  )
}
