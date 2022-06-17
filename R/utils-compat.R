# nocov start - compat-purrr (last updated: rlang 0.3.2.9000)
# ET: added some tibble inner functions, dplyr-like functions

# This file serves as a reference for compatibility functions for
# purrr. They are not drop-in replacements but allow a similar style
# of programming. This is useful in cases where purrr is too heavy a
# package to depend on. Please find the most recent version in rlang's
# repository.

map_lgl <- function(.x, .f, ...) {
  map_mold(.x, .f, logical(1), ...)
}

map <- function(.x, .f, ...) {
  lapply(.x, .f, ...)
}

map_mold <- function(.x, .f, .mold, ...) {
  out <- vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
  names(out) <- names(.x)
  out
}

map_int <- function(.x, .f, ...) {
  map_mold(.x, .f, integer(1), ...)
}

map_dbl <- function(.x, .f, ...) {
  map_mold(.x, .f, double(1), ...)
}

map2 <- function(.x, .y, .f, ...) {
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    stats::setNames(out, names(.x))
  } else {
    stats::setNames(out, NULL)
  }
}

imap <- function(.x, .f, ...) {
  map2(.x, vecpurrr_index(.x), .f, ...)
}

vecpurrr_index <- function(x) {
  names(x) %||% seq_along(x)
}

imap_chr <- function(.x, .f, ...) {
  as.vector(map2(.x, vecpurrr_index(.x), .f, ...), "character")
}

map_chr <- function (.x, .f, ...) {
  map_mold(.x, .f, character(1), ...)
}

#### tibble
# inner functions of tibble

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


### dplyr

pull <- function(data, var = -1, name = NULL, ...) {
  var <- tidyselect::vars_pull(names(data), !!enquo(var))
  name <- enquo(name)
  if (quo_is_null(name)) {
    return(data[[var]])
  }
  name <- tidyselect::vars_pull(names(data), !!name)
  set_names(data[[var]], nm = data[[name]])
}
