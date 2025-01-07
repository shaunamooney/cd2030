downloadCoverageUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(3, downloadButtonUI(ns('plot'))),
    column(3, downloadButtonUI(ns('data')))
  )
}

downloadCoverageServer <- function(id, data_fn, filename, sheet_name = 'Coverage') {
  stopifnot(is.reactive(data_fn))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      downloadPlot(
        id = 'plot',
        filename = filename,
        data = data_fn,
        plot_function = function() {
          plot(data_fn())
        }
      )

      downloadExcel(
        id = 'data',
        filename = filename,
        data = data_fn,
        excel_write_function = function(wb) {
          coverage <- data_fn()

          addWorksheet(wb, sheet_name)
          writeData(wb, sheet = sheet_name, x = coverage, startCol = 1, startRow = 1)
        }
      )

    }
  )

}
