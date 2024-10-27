check_cd_data <- function(.data, call = caller_env()) {

  check_required(.data, call = call)

  if (!inherits(.data, 'cd_data')) {
    cd_abort(c('x' = 'The data object must be of class "cd_data".'), call = call)
  }

}
