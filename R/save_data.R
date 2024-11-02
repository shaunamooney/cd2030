#' Save Processed Countdown 2030 Data to File
#'
#' `save_data` saves processed Countdown 2030 data to either an RDS or DTA file.
#' It checks that the input data is of class `cd_data` before saving.
#'
#' @param .data A tibble of class `cd_data` containing the processed data to be saved.
#' @param file A string. The path to the file where the data should be saved, with
#'   extension `.rds` or `.dta` depending on the desired format. Default is `master_dataset.dta`
#'
#' @return No return value. This function is called for its side effects (saving
#'   the data to an RDS or DTA file).
#'
#' @details
#' The function verifies that the `data` argument is of class `cd_data`. It then
#' saves the data in the specified file format based on the file extension.
#'
#' @examples
#' \dontrun{
#'   # Save processed Countdown 2030 data to an RDS file
#'   save_data(cd_data, "processed_cd2030_data.rds")
#'
#'   # Save processed Countdown 2030 data to a DTA file
#'   save_data(cd_data, "processed_cd2030_data.dta")
#' }
#'
#' @export
save_data <- function(.data, file = 'master_dataset.dta') {

  check_cd_data(.data)

  # Get file extension and determine save format
  file_extension <- file_ext(file)

  # Save based on the specified extension
  switch(
    file_extension,
    'rds' = saveRDS(.data, file = file),
    'dta' = haven::write_dta(.data, path = file),
    cd_abort(
      c('x' = 'Unsupported file format. Please use an ".rds" or ".dta" extension.')
    )
  )
}
