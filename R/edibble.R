
#' Start the edibble design
#'
#' @description
#' This function doesn't really do much besides create a new EdibbleDesign.
#'
#' @param name Optional name used as title for printing the design.
#' @return An `EdibbleGraph` object which is a special type of `R6` class.
#' @examples
#' start_design("My design")
#' @seealso Add variables to this design with [set_units()], [set_trts()], and
#' [measure_units()].
#' @family user-facing functions
#' @export
start_design <- function(name = NULL) {
  EdibbleDesign$new(name = name)
}



# `is` functions ------------------------------------------------------------


#' Test and get edibble objects
#'
#' @description
#' The `is` functions tests if an object (or an object in its attribute)
#' inherits particular class and returns `TRUE` if it does, otherwise `FALSE`.
#'
#' * `is_edibble_design` checks if it inherits `EdibbleDesign`.
#' * `is_edibble_graph` checks if it inherits `edbl_graph`.
#' * `is_edibble_df`, `is_edibble_table` checks if it inherits `edbl_table`
#' * `is_edibble` checks if the object contains `EdibbleDesign`.
#'  The search is quite simple, it checks if
#' the object is `EdibbleDesign`, failing that it looks to see if the
#' attribute "design" of the object is `EdibbleDesign`.
#' * `is_named_design` check if it inherits `NamedDesign`.
#'
#' The `get` functions extracts the requested edibble component (table, graph,
#' or design) from the object if possible.
#'
#' * `get_edibble_design` tries to get `EdibbleDesign`.
#' * `get_edibble_table` tries to get `edbl_table` with no design attribute.
#' * `get_edibble_graph` tries to get `edbl_graph`.
#'
#' @param x An object.
#' @examples
#' is_edibble_df(mtcars) # FALSE
#' is_named_design(code_classical_rcbd(.quiet = TRUE)) # TRUE
#' @name design-helpers
#' @export
is_edibble_design <- function(x) {
  inherits(x, "EdibbleDesign")
}

#' @rdname design-helpers
#' @export
is_named_design <- function(x) {
  inherits(x, "NamedDesign")
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
  if(is_edibble_design(x)) return(TRUE)
  if(is_edibble_design(attr(x, "design"))) return(TRUE)
  FALSE
}


# `get` functions ---------------------------------------------------------

#' @rdname design-helpers
#' @export
get_edibble_design <- function(x) {
  if(is_edibble_design(x)) {
    x
  } else {
    attr(x, "design")
  }
}

#' @rdname design-helpers
#' @export
get_edibble_table <- function(x) {
  if(is_edibble_design(x)) {
    x$table
  } else if(is_edibble_df(x)) {
    attr(x, "design") <- NULL
    x
  } else {
    abort(sprintf("Do not know how to get table from %s.",
                  deparse(substitute(x))))
  }
}

#' @rdname design-helpers
#' @export
get_edibble_graph <- function(x) {
  if(is_edibble_design(x)) {
    x$graph
  } else {
    .design <- attr(x, "design")
    .design$graph
  }
}



# not functions -----------------------------------------------------------

not_edibble <- function(x) {
  if (!is_edibble(x)) {
    abort(sprintf("%s is not an edibble.", deparse(substitute(x))))
  }
}

not_edibble_graph <- function(x) {
  if(!is_edibble_graph(x)) {
    abort(sprintf("%s is not an edibble graph.", deparse(substitute(x))))
  }
}

not_edibble_design <- function(x) {
  if (!is_edibble_design(x)) {
    abort(sprintf("%s is not an edibble design.", deparse(substitute(x))))
  }
}

not_edibble_table <- function(x) {
  if(!is_edibble_table(x)) {
    abort(sprintf("%s is not an edibble table.", deparse(substitute(x))))
  }
}



# edibble constructor -----------------------------------------------------


#' Make input edibble
#'
#' @description
#' **(WIP)** If variables are already defined as external data then you can import this
#' data as edibble.
#'
#' @param .data A named list of variables or data frame.
#' @param ... Passed to `new_tibble`.
#' @param units A character vector specifying variables names in `.data`
#'   which are the unit variables.
#' @param trts A character vector specifying variables names in `.data`
#'   which are the treatment variables.
#' @seealso See [start_design()] for initiating design from scratch.
#'
#' @export
edibble <- function(.data, name = NULL, ..., units = NULL, trts = NULL) {
  stopifnot(is.list(.data))

  name <- name %||% "An edibble design"

  out <- new_edibble(.data, ..., design = start_design(name =  name))
  if(!is_null(units)) out <- set_units(out, expr(units))
  if(!is_null(trts)) out <- set_trts(out, expr(trts))
  out
}

#' @rdname edibble
#' @export
as_edibble <- function(.data, ...) {
  UseMethod("as_edibble")
}

as_edibble.default <- function(.data, ...) {
  edibble(.data, ...)
}




#' Restart the edibble design
#'
#' @description
#' This restarts the edibble design after initiating the design using
#' [edibble()].
restart_design <- function(.data) {
  not_edibble_table(.data)
  attr(.data, "design")
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
#' @importFrom rlang !!!
#'
#' @export
new_edibble <- function(.data, ..., graph = NULL, class = NULL) {
  new_tibble(.data, ..., nrow = vec_size_common(!!!.data),
                     class = "edbl_table", graph = graph)
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.edbl_table <- function(.data) {
  head_meta <- c("An edibble" = dim_desc(.data))
  head_meta
}



