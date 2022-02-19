#' Set edibble variables
#'
#' @description
#' Adds variable and their level nodes in an edibble graph.
#'
#' @inheritParams set_units
#' @param .class A class for the variables.
#' @seealso [set_units()] and [set_trts()] for setting special types of nodes.
#' @importFrom vctrs vec_as_names
#' @importFrom cli col_grey
#' @importFrom tidyselect eval_select
set_vars <- function(.edibble, ..., .class = NULL,
                     .name_repair = c("check_unique", "unique", "universal", "minimal"),
                     .code = NULL) {

  not_edibble(.edibble)

  .name_repair <- match.arg(.name_repair)
  prep <- cook_design(.edibble)

  if(is_edibble_design(.edibble)) {

    dots <- enquos(..., .named = TRUE, .homonyms = "error", .check_assign = TRUE)
    fnames_new <- names(dots)
    fnames_old <- names(prep$design)
    fnames <- vec_as_names(c(fnames_old, fnames_new), repair = .name_repair)

    for(i in seq_along(dots)) {
      fname <- fnames[i + length(fnames_old)]
      fresh <- eval_tidy(dots[[i]], data = c(prep$fct_levels(), list(prep = prep, .fname = fname)))
      prep$add_anatomy(fresh, fname, .class)
      prep$setup_data(fresh, fname, .class)
    }

  # } else if(is_edibble_table(.edibble)) {
  #   dots <- enquos(..., .named = TRUE)
  #   # FIXME
  #   loc <- eval_select(expr(!!names(dots)), .edibble)
  #   for(i in seq_along(loc)) {
  #     var <- .edibble[[loc[i]]]
  #     lvls <- unique(as.character(var))
  #     fname <- names(loc)[i]
  #     .edibble[[loc[i]]] <- ...
  #     .edibble <- add_graph(lvls, fname, .class, res)
  #   }
  }
  prep$design
}














#' Constructor for an edibble variable
#' @importFrom vctrs new_vctr
new_edibble_var <- function(labels = character(), levels = unique(labels),
                            name = character(), rep = NULL, ..., class = NULL) {
  x <- new_vctr(labels, levels = levels, name = name,
                ..., class = c("edbl_var", "character"))
  class(x) <- c(class, class(x))
  x
}


#' Utility functions for edibble variable
#'
#' @description
#' The S3 methods for `edbl_var` objects have
#' the same expected output that of a factor.
#'
#' Other functions are utility functions related to `edbl_var` object.
#'
#' @param x An `edbl_var` object.
#' @param ... Ignored.
#'
#' @name utility-edibble-var
#' @export
as.character.edbl_var <- function(x, ...) {
  #unname(levels(x)[x])
  out <- unclass(x)
  attributes(out) <- NULL
  out
}

#' @export
as.character.edbl_lvls <- function(x, ...) {
  format(x)
}

#' @export
as.integer.edbl_lvls <- function(x, ...) {
  out <- as.integer(as.factor(as.character(x)))
  attributes(out) <- NULL
  out
}

#' @rdname utility-edibble-var
#' @export
as.integer.edbl_var <- function(x, ...) {
  out <- as.integer(as.factor(as.character(x)))
  attributes(out) <- NULL
  out
}

#' @export
levels.edbl_var <- function(x) {
  if(inherits(x, "edbl_rcrd")) {
    unique(attr(x, "unit_values"))
  } else {
    attr(x, "levels")
  }
}

#' @rdname utility-edibble-var
#' @export
is_edibble_var <- function(x) {
  inherits(x, "edbl_var")
}

#' @rdname utility-edibble-var
#' @export
is_edibble_unit <- function(x) {
  inherits(x, "edbl_unit")
}

#' @rdname utility-edibble-var
#' @export
is_edibble_trt <- function(x) {
  inherits(x, "edbl_trt")
}

#' @rdname utility-edibble-var
#' @export
is_edibble_rcrd <- function(x) {
  inherits(x, "edbl_rcrd")
}


#' @importFrom vctrs vec_math
#' @export
vctrs::vec_math

#' @method vec_math edbl_var
#' @export
vec_math.edbl_var <- function(.fn, .x, ...) {
  if(.fn %in% c("is.nan", "is.infinite")) return(rep_len(FALSE, length(.x)))
  if(.fn == "is.finite") return(rep_len(TRUE, length(.x)))
  out <- lapply(as.character(.x), get(.fn), ...)
  vctrs::vec_restore(out, .x)
}


