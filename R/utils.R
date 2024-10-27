check_cd_data <- function(.data, call = caller_env()) {

  check_required(.data, call = call)

  if (!inherits(.data, 'cd_data')) {
    cd_abort(c('x' = 'The data object must be of class "cd_data".'), call = call)
  }

}

check_ratio_pairs <- function(.list, arg = call_args(.list), call = caller_env()) {

  check_required(.data, call = call)

  # Check that ratio_pairs is a named list with each element as a character vector of length 2
  is_ratio_pairs <- all(
    is.list(.list),
    all(lengths(.list) == 2),
    all(map_lgl(.list, ~ is.character(.x) && length(.x) == 2))
  )

  if (!is_ratio_pairs) {
    cd_abort(c('x' = '{.arg arg} is not a proper ratio pair.'), call = call)
  }

}
