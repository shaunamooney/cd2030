check_file_path <- function(path, call = caller_env()) {

  check_required(.data, call = call)

  # Validate if the file exists
  if (!file.exists(path)) {
    cd_abort(
      c("x" = "The specified file {.val {path}} does not exist. Please provide a valid file path."),
      call = call
    )
  }
}

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


cd_theme <- function() {
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),

    legend.background = element_rect(color = "black",size = 0.5),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),

    plot.title = element_text(size = 10, hjust = 0.5),
    plot.caption = element_text(hjust = 0),

    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),

    strip.background = element_blank(),
    strip.text = element_text(size = 10)
  )
}
