
#' Set records for given unit
#'
#' @description
#' This function creates new nodes to edibble graph with the name
#' corresponding to either the intended response that will be measured or
#' a variable to be recorded.
#'
#' @inheritParams design-context
#' @param ... Name-value pair. The name should correspond to the name of the
#'  unit defined in `set_units`. The value should be a vector of new variables
#'  names.
#' @family user-facing functions
#' @examples
#'
#' @export
set_rcrds <- function(.edibble, ...,
                         .name_repair = c("check_unique", "unique", "universal", "minimal")) {

  not_edibble(.edibble)

  .name_repair <- match.arg(.name_repair)
  dots <- enquos(...)

  .design <- get_edibble_design(.edibble)

  if(is_edibble_design(.edibble)) {
    vnames_unit <- names(dots)
    vnames_now <- names_vars(.design)
    if(!all(ind <- vnames_unit %in% vnames_now)) {
      abort("The units", vnames_unit[!ind], "are not defined in the design.")
    }
    attr <- vertex_attr_opt("rcrd")

    for(i in seq_along(dots)) {
      vnames_new <- all.vars(dots[[i]])
      .design$add_record_node(vnames_new, vnames_unit[i], attr)
    }
  }

  # TODO add for edibble_table method

  update_design(.edibble, .design)
}

# Maybe change this to set_rcrds
#' @export
record_vars <- function(.edibble, ...,
                        .name_repair = c("check_unique", "unique", "universal", "minimal")) {
  set_rcrds(.edibble, ..., .name_repair)
}

#' Set the expected values for recording variables
#'
#' @inheritParams design-context
#' @param ... Name-value pairs with the name belonging to the variable
#'  that are plan to be recorded from `set_rcrds()` and the values are
#'  the expected types and values set by helper functions, see `?expect-rcrds`.
#' @family user-facing functions
#' @export
expect_rcrds <- function(.edibble, ...) {
  not_edibble(.edibble)
  .design <- get_edibble_design(.edibble)
  .design$append_validation(list2(...))
  update_design(.edibble, .design)
}

#' @export
expect_vars <- function(.edibble, ...) {
  expect_rcrds(.edibble, ...)
}

has_record <- function(.design) {
  "edbl_rcrd" %in% V(.design$graph)$class
}


#' Expected type of data entry
#'
#' @description
#' These functions should be used within `expect_vars` where variables that
#' are to be recorded are constraint to the expected values when exported
#' as an xlsx file by `export_design().` The functions to set a particular
#' value type (numeric, integer, date, time and character) are preceded by
#' "to_be_" where the corresponding restriction set by `with_value()`.
#'
#' @param value A vector of possible values for entry.
#' @param range,length A named list with two elements: "operator" and "value" as
#'  provided by helper `with_value()` that gives the possible range of values
#'  that the expected type can take.
#' @name expect-vars
#' @export
to_be_numeric <- function(range) {
  c(list(type = "decimal", record = "numeric"), range)
}

#' @rdname expect-vars
#' @export
to_be_integer <- function(range) {
  c(list(type = "whole", record = "integer"), range)
}

#' @rdname expect-vars
#' @export
to_be_date <- function(range) {
  c(list(type = "date", record = "date"), range)
}

#' @rdname expect-vars
#' @export
to_be_time <- function(range) {
  c(list(type = "time", record = "time"), range)
}

#' @rdname expect-vars
#' @export
to_be_character <- function(length) {
  c(list(type = "textLength", record = "text"), length)
}

#' @rdname expect-vars
#' @export
to_be_factor <- function(levels) {
  list(type = "list", record = "factor", values = levels)
}



#' Validation values
#'
#' This creates a list that is used later for creating data validation rules
#' when the data is exported.
#'
#' @param operator Operator to apply.
#' @param value An optional value related to operator
#' @param between,not_between An optional numerical vector of size two where the
#'  first entry is the minimum value and the second entry is the maximum value.
#'  For `between`, the value is valid if within the range of minimum and maximum
#'  value inclusive. For `not_between`, the value must lie outside of these values.
#' @return A list with two elements `operator` and `value`.
#' @export
with_value <- function(operator = c("=", "==", ">=", "<=", "<", ">", "!="),
                     value = NULL, between = NULL, not_between = NULL) {
  operator <- match.arg(operator)
  if(!is_null(between) & !is_null(not_between)) {
    abort("You cannot define `between` and `not_between` simultaneously.")
  }
  if(!is_null(between)) {
    return(list(operator = "between", value = between))
  }
  if(!is_null(not_between)) {
    return(list(operator = "notBetween", value = not_between))
  }
  list(operator = switch(operator,
                     "=" = "equal",
                     "==" = "equal",
                     ">=" = "greaterThanOrEqual",
                     ">" = "greaterThan",
                     "<=" = "lessThanOrEqual",
                     "<" = "lessThan",
                     "!=" = "notEqual"),
       value = value)
}


new_edibble_rcrd <- function(n, unit, class = NULL) {
  v <- rep("x", n)
  loc <- match(unique(unit), unit)
  v[loc] <- "■"
  x <- new_vctr(v, class = "edbl_rcrd")
  class(x) <- c(class, class(x))
  x
}

#' @importFrom pillar pillar_shaft new_pillar_shaft_simple style_subtle
#' @export
pillar_shaft.edbl_rcrd <- function(x, ...) {
  out <- as.character(x)
  out <- ifelse(out=="x", style_subtle("x"), out)
  new_pillar_shaft_simple(out, align = "right")
}

#' @export
as.character.edbl_rcrd <- function(x, ...) {
  out <- unclass(x)
  attributes(out) <- NULL
  out
}


#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.edbl_rcrd <- function(x, ...)  {
  "rcrd"
}

#' @importFrom vctrs vec_ptype_full
#' @export
vec_ptype_full.edbl_rcrd <- function(x, ...) "rcrd"

# see scabbiness.R
#' Derive variables from other variables.
#'
#' @description
#' This is used to specify the excel formula for variables that
#' are derived based on other variables.
#'
#' @seealso See [calculate()], [calculate2()] and [pcalculate()] to
#' specify the excel formula.
derive_vars <- function(.edibble, ...) {
  dots <- enquos(...)
  dots_names <- names(dots)

  .design <- get_edibble_design(.edibble)

  if(is_edibble_design(.edibble)) {
    attr <- vertex_attr_opt("rcrd")
    for(i in seq_along(dots)) {
      vname_new <- dots_names[i]
      vname_on <- dots[[i]] # TODO get first arguments
      .design$add_record_node(vname_new, vname_on, attr)
    }
  }
  update_design(.edibble, .design)
}

#' Specify the calculation to derive variables
#'
#' @description
#' This function specifies the excel formula that should
#' be stored when the design is exported with [export_design()].
#' The functions must be translatable to excel formula. Mappings
#' for some complex functions may not work.
#'
#' @param .x,.y Name of other variables in data.
#' @param .f A function, formula, or excel formula.
#' @param ... Arguments to be passed into `.f`.
#' @param .l A vector of variable names.
#' @return A special derivation class.
calculate <- function(.x, .f, ..., .group_by = NULL) {
  if(inherits(.f, "xlformula")) {
    xlf <- .f
  } else if(is.function(.f)) {
    xlf <- map_to_xlf(.f, ...) # TODO
  } else {
    xlf <- map_to_xlf(rlang::as_function(.f))
  }
  return(structure(list(vars = as_label(enexpr(.x)),
                        xlf = xlf,
                        group_by = .group_by),
                   class = "derivative"))
}

calculate2 <- function(.x, .y, .f, ..., .group_by = NULL) {
  if(inherits(.f, "xlformula")) {
    xlf <- .f
  } else if(is.function(.f)) {
    xlf <- map_to_xlf(.f, ...) # TODO
  } else {
    xlf <- map_to_xlf(rlang::as_function(.f))
  }
  return(structure(list(vars = c(as_label(enexpr(.x)),
                                 as_label(enexpr(.y))),
                        xlf = xlf,
                        group_by = .group_by),
                   class = "derivative"))
}

pcalculate <- function(.l, .f, ..., .group_by = NULL) {
  if(inherits(.f, "xlformula")) {
    xlf <- .f
  } else if(is.function(.f)) {
    xlf <- map_to_xlf(.f, ...) # TODO
  } else {
    xlf <- map_to_xlf(rlang::as_function(.f))
  }
  return(structure(list(vars = all.vars(enexprs(l)),
                        xlf = xlf,
                        group_by = .group_by),
                   class = "derivative"))
}

xlf <- function(.f = NULL) {
  structure(.f, class = "xlformula")
}
