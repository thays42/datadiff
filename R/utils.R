col_class <- function(x) {
  x |>
    class() |>
    paste0(collapse = "/")
}

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".join_type",
    ".diff_type",
    ".source",
    ".rn",
    ".rn.x",
    ".rn.y"
  ))
}
