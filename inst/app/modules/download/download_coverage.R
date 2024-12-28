downloadCoverageUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(3, downloadButtonUI(ns('plot'), label = 'Download Plot')),
    column(3, downloadButtonUI(ns('data'), label = 'Download Data'))
  )
}

downloadCoverageServer <- function(id, data_fn, filename, sheet_name = 'Coverage') {
  stopifnot(is.reactive(data_fn))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      downloadButtonServer(
        id = 'plot',
        filename = filename,
        extension = 'png',
        content = function(file) {
          plot(data_fn())
          ggsave(file, width = 1920, height = 1080, dpi = 150, units = 'px')
        },
        data = data_fn
      )

      downloadButtonServer(
        id = 'data',
        filename = filename,
        extension = 'xlsx',
        content = function(file) {
          wb <- createWorkbook()
          coverage <- data_fn()

          addWorksheet(wb, sheet_name)
          writeData(wb, sheet = sheet_name, x = coverage, startCol = 1, startRow = 1)

          saveWorkbook(wb, file, overwrite = TRUE)
        },
        data = data_fn
      )

    }
  )

}
