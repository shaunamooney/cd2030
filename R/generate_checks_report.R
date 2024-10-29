#' Generate and Export Checks Report
#'
#' The `generate_checks_report` function generates a data quality report based on a specified dataset,
#' including metrics such as reporting rates, internal consistency, adequate ratios, outliers,
#' and missing values. The report is rendered using an RMarkdown template and can be output
#' in either HTML or Word format.
#'
#' @param .data A data frame or list object containing the dataset to be analyzed.
#' @param output_file A character string specifying the name and path of the output file.
#' @param output_format A character vector specifying the output format, either `"html_document"` or `"word_document"`.
#'   Default is `c("html_document", "word_document")`.
#' @param threshold A numeric value indicating the threshold for reporting rates in percentage (default is 90).
#' @return The function renders the report to the specified file in the chosen format.
#' @examples
#' \dontrun{
#'   # Generate a report for the dataset 'data' in Word format with a threshold of 85%
#'   checks_report(data, output_file = "data_quality_report.docx",
#'     output_format = "word_document", threshold = 85)
#' }
#' @export
generate_checks_report <- function(.data,
                          output_file,
                          output_format = c('html_document', 'word_document'),
                          threshold = 90) {

  check_cd_data(.data)
  check_required(output_file)
  format <- arg_match(output_format)

  # If output_file is just a file name, save it in the working directory
  if (!dirname(output_file) %in% c(".", "")) {
    output_path <- output_file  # Use full path if provided
  } else {
    output_path <- file.path(getwd(), output_file)  # Default to working directory
  }

  # Create a unique temporary directory for rendering
  temp_dir <- tempfile("report_render_")
  dir.create(temp_dir)

  render(
    input = paste0(system.file(package = "cd2030"), "/rmd/check_template.Rmd"),
    output_format = format,
    output_file = output_path,
    params = list(data = .data, threshold = threshold),
    encoding = "UTF-8",
    runtime = 'static',
    intermediates_dir = temp_dir,    # Set unique temp directory
    clean = TRUE                     # Clean up intermediate files
  )

  # Open the generated file automatically
  if (file.exists(output_path)) {
    if (format == 'html_document') {
      browseURL(output_path)
    }
  } else {
    message("Report generation failed: File not found.")
  }
}
