#' Clean and Validate Error Messages
#'
#' `clean_error_message` processes and cleans error messages to remove unnecessary
#' details such as ANSI escape codes, CLI-specific bullets, stack traces, and
#' technical context. It ensures the input is a valid error object before processing
#' and returns a blank string (`""`) if the input is not an error.
#'
#' @param error_message The input to be processed. This should be an object of
#'   class `"error"`. Non-error inputs will result in a blank string being returned.
#'
#' @return A cleaned, user-friendly error message as a string. Returns an empty
#'   string if the input is not an error object.
#'
#' @examples
#' # Processing a Valid Error Message
#' tryCatch({
#'   stop("Something went wrong.")
#' }, error = function(e) {
#'   cleaned <- clean_error_message(e)
#'   print(cleaned)
#' })
#'
#' # Handling Non-Error Input
#' cleaned <- clean_error_message("This is not an error object")
#' print(cleaned)
#'
#' @export
clean_error_message <- function(error_message) {
  # Check if input is an error object
  if (!inherits(error_message, "error")) {
    return("") # Return blank if not a valid error
  }

  # Extract and clean the error message
  clean_message <- cli::ansi_strip(conditionMessage(error_message))

  # Remove specific CLI bullets (✖, ✔, ℹ, ⚠, !)
  # clean_message <- gsub("[✖✔ℹ⚠!]", "", clean_message)
  # clean_message <- gsub("[\\u2716\\u2714\\u2139\\u26A0!]", "", clean_message)
  clean_message <- gsub("[✖✔ℹ⚠!]", "", clean_message, useBytes = TRUE)

  # Remove "In index: ..." patterns
  clean_message <- gsub("In index: \\d+\\.?", "", clean_message)

  # Remove "Caused by error in ..." patterns
  clean_message <- gsub("Caused by error in `.*?\\(\\)`:?", "", clean_message)

  # Trim leading and trailing whitespace
  clean_message <- trimws(clean_message)

  return(clean_message)
}
