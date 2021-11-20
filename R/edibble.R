
# `is` functions ------------------------------------------------------------

#' Test and get edibble objects
#'
#' @description
#' The `is` functions tests if an object (or an object in its attribute)
#' inherits particular class and returns `TRUE` if it does, otherwise `FALSE`.
#'
#' * `is_edibble_design` checks if it inherits `edbl_design`.
#' * `is_edibble_graph` checks if it inherits `edbl_graph`.
#' * `is_edibble_table` checks if it inherits `edbl_table`
#' * `is_edibble` checks if the object inherits `edbl`.
#'  The search is quite simple, it checks if
#' the object is `edbl_design`, failing that it looks to see if the
#' attribute "design" of the object is `edbl_design`.
#' * `is_named_design` check if it inherits `NamedDesign`.
#'
#' The `get` functions extracts the requested edibble component (table, graph,
#' or design) from the object if possible.
#'
#' * `edbl_design` tries to get `edbl_design`.
#' * `edbl_table` tries to get `edbl_table` with no design attribute.
#' * `edbl_graph` tries to get `edbl_graph`.
#'
#' @param x An object.
#' @name design-helpers
#' @export
is_edibble_design <- function(x) {
  inherits(x, "edbl_design")
}

#' @rdname design-helpers
#' @export
is_named_design <- function(x) {
  inherits(x, "named_design")
}

#' @rdname design-helpers
#' @export
is_edibble_table <- function(x) {
  inherits(x, "edbl_table")
}

#' @rdname design-helpers
#' @export
is_edibble_graph <- function(x) {
  inherits(x, "edbl_graph")
}

#' @rdname design-helpers
#' @export
is_edibble <- function(x) {
  inherits(x, "edbl")
}


# `get` functions ---------------------------------------------------------

#' @rdname design-helpers
#' @export
edbl_design <- function(x) {
  if(is_edibble_design(x)) {
    x
  } else if(is_edibble_table(x)) {
    attr(x, "design")
  } else {
    abort(sprintf("An edibble design is not available in %s.",
                  deparse(substitute(x))))
  }
}

#' @rdname design-helpers
#' @export
edbl_table <- function(x) {
  if(is_edibble_design(x)) {
    return(x$table)
  } else if(is_edibble_table(x)) {
    return(x)
  } else {
    abort(sprintf("Do not know how to get table from %s.",
                  deparse(substitute(x))))
  }
}



# `not` functions -----------------------------------------------------------

not_edibble <- function(x) {
  if (!is_edibble(x)) {
    abort(sprintf("%s is not an edibble.", deparse(substitute(x))))
  }
}


#' An edibble table constructor
#'
#' @description
#' This helps to construct a new edibble table which is a special type
#' of tibble.
#'
#' @param .data data frame or list of the same size.
#' @param ... Passed to `new_tibble`.
#' @param graph An edibble graph object.
#' @param class Subclasses for edibble table. The default is NULL.
#' @importFrom tibble new_tibble
#' @importFrom vctrs vec_size_common
#'
#' @export
new_edibble <- function(.data, ..., graph = NULL, class = NULL) {
  new_tibble(.data, ..., nrow = vec_size_common(!!!.data),
             class = c("edbl_table", class), graph = graph)
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.edbl_table <- function(.data) {
  head_meta <- c("An edibble" = dim_desc(.data))
  head_meta
}


# as_edibble <- function(.data, ...) {
#   UseMethod("as_edibble")
# }
#
# as_edibble.default <- function(.data, ...) {
#   edibble(.data, ...)
# }




#' Restart the edibble design
#'
#' @description
#' This restarts the edibble design after initiating the design using
#' [edibble()].
# restart_design <- function(.data) {
#   not_edibble_table(.data)
#   attr(.data, "design")
# }

