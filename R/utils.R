#' Assert that an object belongs to a specified class
#'
#' @param x The object to check
#' @param class A character vector of class names
#' @param arg_name Optional name of the argument for more informative error messages,
#'                defaults to the deparsed name of the object
#'
#' @return Invisibly returns the object if it passes the check, otherwise aborts
#' @examples
#' x <- data.frame(a = 1)
#' assert_class(x, "data.frame")  # Passes
#' \dontrun{
#' assert_class(x, "matrix")      # Errors
#' assert_class(x, c("matrix", "list"))  # Passes (data.frame inherits from list)
#' }
assert_class <- function(x, class, arg_name = deparse(substitute(x))) {
  if (!inherits(x, class)) {
    class_str <- if (length(class) == 1) {
      paste0("class '", class, "'")
    } else {
      paste0("any of classes: ", paste0("'", class, "'", collapse = ", "))
    }

    actual_class <- paste(class(x), collapse = ", ")

    rlang::abort(
      message = glue::glue(
        "'{arg_name}' must be {class_str}, not '{actual_class}'"
      ),
      call = rlang::caller_env()
    )
  }

  invisible(x)
}

#' Assert that an object has a specified length or range of lengths
#'
#' This function checks if an object's length matches specified constraints and
#' throws an error if it doesn't.
#'
#' @param x The object to check
#' @param exact Exact length the object should have (optional)
#' @param min Minimum length the object should have (optional)
#' @param max Maximum length the object should have (optional)
#' @param arg_name Optional name of the argument for more informative error messages,
#'                defaults to the deparsed name of the object
#'
#' @return Invisibly returns the object if it passes the check, otherwise aborts
#' @examples
#' x <- c(1, 2, 3)
#' assert_length(x, exact = 3)  # Passes
#' assert_length(x, min = 2)    # Passes
#' assert_length(x, max = 5)    # Passes
#' assert_length(x, min = 2, max = 5)  # Passes
#' \dontrun{
#' assert_length(x, exact = 4)  # Errors
#' assert_length(x, min = 4)    # Errors
#' assert_length(x, max = 2)    # Errors
#' }
#' @export
assert_length <- function(
  x,
  exact = NULL,
  min = NULL,
  max = NULL,
  arg_name = deparse(substitute(x))
) {
  actual_length <- length(x)

  # Check if the user provided any constraints
  if (is.null(exact) && is.null(min) && is.null(max)) {
    rlang::abort(
      "At least one of 'exact', 'min', or 'max' must be specified"
    )
  }

  # Check for conflicting constraints
  if (!is.null(exact) && (!is.null(min) || !is.null(max))) {
    rlang::abort(
      "Cannot specify both 'exact' and 'min'/'max' constraints",
    )
  }

  # Perform the length checks
  if (!is.null(exact)) {
    if (actual_length != exact) {
      rlang::abort(
        glue::glue(
          "'{arg_name}' must have length {exact}, not {actual_length}"
        ),
        call = rlang::caller_env()
      )
    }
  } else {
    # Check min constraint if provided
    if (!is.null(min) && actual_length < min) {
      rlang::abort(
        glue::glue(
          "'{arg_name}' must have length at least {min}, not {actual_length}"
        ),
        call = rlang::caller_env()
      )
    }

    # Check max constraint if provided
    if (!is.null(max) && actual_length > max) {
      rlang::abort(
        glue::glue(
          "'{arg_name}' must have length at most {max}, not {actual_length}"
        ),
        call = rlang::caller_env()
      )
    }
  }

  # Return the object invisibly if all checks pass
  invisible(x)
}

#' Assert that a numeric vector is bounded by a min and/or max value
#'
#' @param x The numeric vector to check, or an object to pass to `fn`
#' @param exact Exact value `x` should have (optional)
#' @param min Minimum value `x` should have (optional)
#' @param max Maximum value `x` should have (optional)
#' @param fn Function to apply to x that returns the numeric vector to check (optional)
#' @param arg_name Optional name of the argument for more informative error messages,
#'                defaults to the deparsed name of the object
#'
#' @return Invisibly returns the object if it passes the check, otherwise aborts
assert_bounds <- function(
  x,
  exact = NULL,
  min = NULL,
  max = NULL,
  fn = NULL,
  label = NULL
) {
  # Check if the user provided any constraints
  if (is.null(exact) && is.null(min) && is.null(max)) {
    rlang::abort(
      "At least one of 'exact', 'min', or 'max' must be specified"
    )
  }

  # Check for conflicting constraints
  if (!is.null(exact) && (!is.null(min) || !is.null(max))) {
    rlang::abort(
      "Cannot specify both 'exact' and 'min'/'max' constraints",
    )
  }

  if (!is.null(fn)) {
    if (!is.function(fn)) rlang::abort("'fn' must be a function")
    actual <- fn(x)
    if (!is.numeric(actual))
      rlang::abort("'fn(x)' must return a numeric vector")

    label <- label %||%
      paste0(
        deparse(substitute(fn)),
        "(",
        deparse(substitute(x)),
        ")"
      )
  } else if (!is.numeric(x)) {
    rlang::abort("`x` must be a numeric vector")
  } else {
    actual <- x
    label <- label %||% deparse(substitute(x))
  }

  if (!is.null(exact)) {
    if (actual != exact) {
      rlang::abort(
        glue::glue(
          "'{label}' must be {exact}, not {actual}"
        ),
        call = rlang::caller_env()
      )
    }
  } else {
    # Check min constraint if provided
    if (!is.null(min) && actual < min) {
      rlang::abort(
        glue::glue(
          "'{label}' must be at least {min}, not {actual}"
        ),
        call = rlang::caller_env()
      )
    }

    # Check max constraint if provided
    if (!is.null(max) && actual > max) {
      rlang::abort(
        glue::glue(
          "'{label}' must be at most {max}, not {actual}"
        ),
        call = rlang::caller_env()
      )
    }
  }

  # Return the object invisibly if all checks pass
  invisible(x)
}

col_class <- function(x) {
  x |>
    class() |>
    paste0(collapse = "/")
}
