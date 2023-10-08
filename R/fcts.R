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
#' @noRd
set_fcts <- function(.edibble, ..., .class = NULL,
                     .name_repair = c("check_unique", "unique", "universal", "minimal")) {

  not_edibble(.edibble)

  .name_repair <- match.arg(.name_repair)
  prov <- activate_provenance(.edibble)

  if(is_edibble_design(.edibble)) {

    dots <- enquos(..., .named = TRUE, .homonyms = "error", .check_assign = TRUE)
    fnames_new <- names(dots)
    fnames_old <- prov$fct_nodes$name
    fnames <- vec_as_names(c(fnames_old, fnames_new), repair = .name_repair)

    for(i in seq_along(dots)) {
      fname <- fnames[i + length(fnames_old)]
      input <- eval_tidy(dots[[i]], data = c(prov$fct_levels(return = "value"), list(prov = prov, .fname = fname)))
      .edibble$anatomy <- add_anatomy(.edibble$anatomy, input, fname, .class)
      graph_input(input, prov, fname, .class)
    }

  } else if(is_edibble_table(.edibble)) {
    loc <- eval_select(expr(c(...)), .edibble)
    for(i in seq_along(loc)) {
      var <- .edibble[[loc[i]]]
      lvls <- sort(unique(var))
      fname <- names(loc)[i]
      .edibble[[loc[i]]] <- new_edibble_fct(labels = var,
                                            levels = lvls,
                                            class = .class,
                                            name = fname)
      graph_input.default(lvls, prov, fname, .class)
    }

  }
  return_edibble_with_graph(.edibble, prov)
}





#' Constructor for an edibble variable
#' @importFrom vctrs new_vctr
#' @noRd
new_edibble_fct <- function(labels = character(), levels = unique(labels),
                            name = character(), rep = NULL, ..., class = NULL) {
  # don't make the attribute name
  # as this triggers the warning message in ggplot2:
  # In attr(x, "n") : partial match of 'n' to 'name'
  x <- new_vctr(labels, levels = levels, fname = name,
                ..., class = c("edbl_fct", class(labels)))
  class(x) <- c(class, class(x))
  x
}


#' Utility functions for edibble variable
#'
#' @description
#' The S3 methods for `edbl_fct` objects have
#' the same expected output that of a factor.
#'
#' Other functions are utility functions related to `edbl_fct` object.
#'
#' @param x An `edbl_fct` object.
#' @param ... Ignored.
#'
#' @name utility-edibble-var
#' @return A character vector.
#' @export
as.character.edbl_fct <- function(x, ...) {
  #unname(levels(x)[x])
  out <- unclass(x)
  attributes(out) <- NULL
  as.character(out)
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
as.integer.edbl_fct <- function(x, ...) {
  out <- as.integer(as.factor(as.character(x)))
  attributes(out) <- NULL
  out
}

#' @export
levels.edbl_fct <- function(x) {
  if(inherits(x, "edbl_rcrd")) {
    unique(attr(x, "unit_values"))
  } else {
    attr(x, "levels")
  }
}

#' @rdname utility-edibble-var
#' @export
is_fct <- function(x) {
  inherits(x, "edbl_fct")
}

#' @rdname utility-edibble-var
#' @export
is_unit <- function(x) {
  inherits(x, "edbl_unit")
}

#' @rdname utility-edibble-var
#' @export
is_trt <- function(x) {
  inherits(x, "edbl_trt")
}

#' @rdname utility-edibble-var
#' @export
is_rcrd <- function(x) {
  inherits(x, "edbl_rcrd")
}





#' @importFrom vctrs vec_math
#' @method vec_math edbl_fct
#' @export
vec_math.edbl_fct <- function(.fn, .x, ...) {
  if(.fn %in% c("is.nan", "is.infinite")) return(rep_len(FALSE, length(.x)))
  if(.fn == "is.finite") return(rep_len(TRUE, length(.x)))
  get(.fn)(unclass(.x))
}

#' @importFrom vctrs vec_ptype2
#' @export
vec_ptype2.edbl_unit.character <- function(x, y, ...) y
#' @export
vec_ptype2.character.edbl_unit <- function(x, y, ...) x
#' @export
vec_ptype2.edbl_unit.double <- function(x, y, ...) y
#' @export
vec_ptype2.double.edbl_unit <- function(x, y, ...) x
#' @export
vec_ptype2.edbl_unit.integer <- function(x, y, ...) y
#' @export
vec_ptype2.integer.edbl_unit <- function(x, y, ...) x

#' @export
vec_ptype2.edbl_trt.character <- function(x, y, ...) y
#' @export
vec_ptype2.character.edbl_trt <- function(x, y, ...) x
#' @export
vec_ptype2.edbl_trt.double <- function(x, y, ...) y
#' @export
vec_ptype2.double.edbl_trt <- function(x, y, ...) x
#' @export
vec_ptype2.edbl_trt.integer <- function(x, y, ...) y
#' @export
vec_ptype2.integer.edbl_trt <- function(x, y, ...) x

#' @importFrom vctrs vec_cast
#' @export
vec_cast.edbl_trt.double <- function(x, to, ...) to
#' @export
vec_cast.double.edbl_trt <- function(x, to, ...) x
#' @export
vec_cast.edbl_trt.integer <- function(x, to, ...) to
#' @export
vec_cast.integer.edbl_trt <- function(x, to, ...) x
#' @export
vec_cast.edbl_trt.character <- function(x, to, ...) to
#' @export
vec_cast.character.edbl_trt <- function(x, to, ...) x

#' @export
vec_cast.edbl_unit.double <- function(x, to, ...) to
#' @export
vec_cast.double.edbl_unit <- function(x, to, ...) x
#' @export
vec_cast.edbl_unit.integer <- function(x, to, ...) to
#' @export
vec_cast.integer.edbl_unit <- function(x, to, ...) x
#' @export
vec_cast.edbl_unit.character <- function(x, to, ...) to
#' @export
vec_cast.character.edbl_unit <- function(x, to, ...) x



# ADDME add_units(exist = TRUE), reset_units(exist = FALSE)

