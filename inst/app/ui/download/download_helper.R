downloadPlot <- function(id, filename, data, plot_function, label = 'Download Plot') {

  downloadButtonServer(
    id = id,
    filename = filename,
    extension = 'png',
    content = function(file) {
      plot_function()
      ggsave(file, width = 3840, height = 2160, dpi = 300, units = 'px')
    },
    data = data,
    label = label
  )
}

downloadExcel <- function(id, filename, data, excel_write_function, label = 'Download Data') {
  downloadButtonServer(
    id = id,
    filename = filename,
    extension = 'xlsx',
    content = function(file) {
      wb <- createWorkbook()

      excel_write_function(wb)

      # Save the workbook
      saveWorkbook(wb, file, overwrite = TRUE)
    },
    data = data,
    label = label
  )
}
