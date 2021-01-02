# inner functions of tibble

#' @importFrom rlang %||%
dim_desc <- function (x) {
  dim <- dim(x) %||% length(x)
  format_dim <- map_chr(dim, big_mark)
  paste0(format_dim, collapse = spaces_around(mult_sign()))
}

spaces_around <- function (x) {
  paste0(" ", x, " ")
}

mult_sign <- function() {
  "x"
}

big_mark <- function (x, ...) {
  mark <- if (identical(getOption("OutDec"), ","))
    "."
  else ","
  ret <- formatC(x, big.mark = mark, format = "d", ...)
  ret[is.na(x)] <- "??"
  ret
}
