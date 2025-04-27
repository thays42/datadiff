col_class <- function(x) {
  x |>
    class() |>
    str_c(collapse = "/")
}
