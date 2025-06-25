col_class <- function(x) {
  x |>
    class() |>
    paste0(collapse = "/")
}
