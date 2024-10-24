#' Save Processed Countdown 2030 Data to an RDS File
#'
#' This function saves the processed Countdown 2030 data to an RDS file. It
#'   checks that the input data is of class `cd_data` before saving.
#'
#' @param data A list of class `cd_data` containing the processed data to be saved.
#' @param file A string. The path to the RDS file where the data should be saved.
#'
#' @return No return value. This function is called for its side effects (saving
#'   the data to an RDS file).
#'
#' @details The function only saves the `merged_data` portion of the `cd_data`
#'   object to the specified file. If the input data does not belong to the \code{"cd_data"} class, an error is thrown.
#'
#' @examples
#' \dontrun{
#'   # Save processed Countdown 2030 data to an RDS file
#'   save_data(cd_data, "processed_cd2030_data.rds")
#' }
#'
#' @export
save_data <- function(data, file) {
  if (!inherits(data, "cd_data")) stop("The data object must be of class 'cd_data'.")

  # Save the merged_data portion of the object to an RDS file
  saveRDS(data$merged_data, file = file)
}
