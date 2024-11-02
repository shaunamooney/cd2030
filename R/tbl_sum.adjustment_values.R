#' Summary for `cd_adjustment_values` Objects
#'
#' Provides a custom summary for objects of class `cd_adjustment_values`, typically
#' displaying unadjusted and adjusted values in a structured format.
#'
#' @param x An object of class `cd_adjustment_values`, usually created by
#'   `generate_adjustment_values`, containing unadjusted and adjusted service counts.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @details This function overrides the default `tbl_sum` method for objects of
#'   class `cd_adjustment_values`. It returns a named character vector summarizing
#'   the table as "Unadjusted and adjusted values" along with any additional details
#'   provided by the `NextMethod()` function.
#'
#' @return A character vector with a custom table summary for `cd_adjustment_values`
#'   objects.
#'
#' @seealso [generate_adjustment_values()] for generating the `cd_adjustment_values`
#'   object.
#'
#' @export
tbl_sum.cd_adjustment_values <- function(x, ...) {
  c(
    'Table 1' = 'Unadjusted and adjusted values',
    NextMethod()
  )
}
