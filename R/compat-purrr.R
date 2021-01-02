# nocov start - compat-purrr (last updated: rlang 0.3.2.9000)

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


#' @importFrom rlang set_names
map2 <- function(.x, .y, .f, ...) {
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    set_names(out, names(.x))
  } else {
    set_names(out, NULL)
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
