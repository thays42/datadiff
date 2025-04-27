#' Equality Test
#'
#' @param x,y Vectors to compare.
#' @param tol Numeric tolerance for comparison.
#'
#' @return TRUE if equal (within tolerance), FALSE otherwise.
#' @export
is_equal <- function(x, y, tol = 1e-5) {
  if (is.numeric(x)) {
    (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & abs(x - y) < tol)
  } else {
    (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
  }
}
