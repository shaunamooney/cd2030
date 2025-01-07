#' Generate and Export Checks Report
#'
#' The `generate_final_report` function generates the final report based on a
#' specified dataset and parameters. The report is rendered using an RMarkdown
#' template and can be output in either Word, pdf of html formats.
#'
#' @param .data A data frame or list object containing the dataset to be analyzed.
#' @param output_file A character string specifying the name and path of the output file.
#' @param output_format A character vector specifying the output format, either
#'   `'html_document'` or `'word_document'`.
#' @param denominator A character string or numeric value defining the denominator
#'   used for rate calculations, such as population size or number of facilities.
#' @param survey_start_year Numeric. The starting year of the survey data used in
#'   the analysis.
#'
#' @return The function renders the report to the specified file in the chosen format.
#'
#' @examples
#' \dontrun{
#'   # Generate a report for the dataset 'data' in Word format with a threshold of 85%
#'   checks_report(data, output_file = 'data_quality_report.docx',
#'     output_format = 'word_document', threshold = 85)
#' }
#' @export
generate_final_report <- function(cache,
                          output_file,
                          denominator,
                          output_format = c('word_document', 'pdf_document', 'html_document')) {

  # check_cd_data(.data)
  check_required(cache)
  check_required(output_file)
  check_required(denominator)

  format <- arg_match(output_format)

  # If output_file is just a file name, save it in the working directory
  if (!dirname(output_file) %in% c('.', '')) {
    output_path <- output_file  # Use full path if provided
  } else {
    output_path <- file.path(getwd(), output_file)  # Default to working directory
  }

  # Create a unique temporary directory for rendering
  temp_dir <- tempfile('report_render_')
  dir.create(temp_dir)

  render(
    input = paste0(system.file(package = 'cd2030'), '/rmd/final_report_template.Rmd'),
    output_format = format,
    output_file = output_path,
    params = list(cache = cache, denominator = denominator, country = cache$get_country()),
    encoding = 'UTF-8',
    runtime = 'auto',
    intermediates_dir = temp_dir,    # Set unique temp directory
    clean = TRUE                     # Clean up intermediate files
  )

  # Open the generated file automatically
  tryCatch({
    check_file_path(output_path)
    if (format == 'html_document') {
      utils::browseURL(output_path)
    }
    showNotification('Report generation Succeeded.', type = 'message')
  },
  error = function(e){
    error <- clean_error_message(e)
    showNotification(paste0('Report generation failed: ', error), type = 'error')
  })
}
