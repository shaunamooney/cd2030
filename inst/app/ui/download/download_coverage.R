downloadCoverageUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(3, downloadButtonUI(ns('plot'))),
    column(3, downloadButtonUI(ns('data')))
  )
}

downloadCoverageServer <- function(id, data, filename, indicator, denominator,
                                   data_fn, region = NULL, sheet_name = 'Coverage') {
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(indicator))
  stopifnot(is.reactive(denominator))
  stopifnot(is.function(data_fn))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      download_data <- reactive({
        req(data(), indicator(), denominator())

        data() %>%
          data_fn(indicator = indicator(), denominator = denominator(), region = region)
      })

      downloadPlot(
        id = 'plot',
        filename = filename,
        data = download_data,
        plot_function = function() {
          plot(data(), indicator = indicator(), denominator = denominator(), region = region)
        }
      )

      downloadExcel(
        id = 'data',
        filename = filename,
        data = download_data,
        excel_write_function = function(wb) {
          coverage <- download_data()

          addWorksheet(wb, sheet_name)
          writeData(wb, sheet = sheet_name, x = coverage, startCol = 1, startRow = 1)
        }
      )

    }
  )

}
