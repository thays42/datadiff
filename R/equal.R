#' Vectorized Equality Test
#'
#' Element-wise equality comparison that handles `NA` and `Inf` values correctly,
#' and supports numeric tolerance for floating-point comparisons.
#'
#' @param x,y Vectors to compare.
#' @param tol Numeric tolerance for comparison. Only applies to numeric vectors.
#' @return A logical vector the same length as `x` and `y`, where each element
#'   is `TRUE` if the corresponding elements are equal (within tolerance for
#'   numeric values) and `FALSE` otherwise.
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
