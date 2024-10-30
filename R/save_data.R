#' Save Processed Countdown 2030 Data to File
#'
#' This function saves the processed Countdown 2030 data to either an RDS or DTA file.
#' It checks that the input data is of class `cd_data` before saving.
#'
#' @param .data A tibble of class `cd_data` containing the processed data to be saved.
#' @param file A string. The path to the file where the data should be saved.
#'   Defaults to "master_dataset.dta" if not specified.
#'
#' @return No return value. This function is called for its side effects (saving
#'   the data to an RDS or DTA file).
#'
#' @details The function only saves the main `data` portion of the `cd_data`
#'   object to the specified file. If the input data does not belong to the
#'   `"cd_data"` class, an error is thrown. The file format is determined by
#'   the file extension: `.rds` for RDS format and `.dta` for DTA format.
#'
#' @examples
#' \dontrun{
#'   # Save processed Countdown 2030 data to an DTA file
#'   save_data(cd_data)
#'
#'   # Save processed Countdown 2030 data to a RDS file
#'   save_data(cd_data, "master_dataset.rds")
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
    'dta' = haven::write_dta(.data, file = file),
    cd_abort(
      c('x' = 'Unsupported file format. Please use an ".rds" or ".dta" extension.')
    )
  )
}
