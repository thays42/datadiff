#' Equality Test
#'
#' @param x,y Vectors to compare.
#' @param tol Numeric tolerance for comparison.
#'
#' @return TRUE if equal (within tolerance), FALSE otherwise.
#' @export
is_equal <- function(x, y, tol = .Machine$double.eps^0.5) {
  stopifnot(
    "tol must be a single non-negative finite number" =
      is.numeric(tol) && length(tol) == 1 && !is.na(tol) && tol >= 0 && is.finite(tol)
  )
  if (is.numeric(x) != is.numeric(y)) {
    return(FALSE)
  } else if (is.numeric(x) && is.numeric(y)) {
    (
      # both NA
      (is.na(x) & is.na(y)) |

        # both Inf or -Inf
        (is.infinite(x) & is.infinite(y) & sign(x) == sign(y)) |

        # both finite, within tolerance
        (!is.na(x) & !is.na(y) & abs(x - y) <= tol)
    )
  } else {
    (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
  }
}
