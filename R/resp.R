
#' Measure response of given units
#'
#' @description
#' This function creates new nodes to edibble graph with the name
#' corresponding to the intended response that will be measured.
#'
#' @param .design An `EdibbleDesign` object
#' @param ... Name-value pair. The name should correspond to the name of the
#'  unit defined in `set_units`. The value should be a vector of new variables
#'  names.
#'
#' @export
record_vars <- function(.design, ...,
                         .name_repair = c("check_unique", "unique", "universal", "minimal")) {

  .name_repair <- match.arg(.name_repair)
  dots <- enquos(...)

  if(.design$active == "graph") {
    vnames_unit <- names(dots)
    vnames_now <- names_vars(.design)
    if(!all(ind <- vnames_unit %in% vnames_now)) {
      abort("The units", vnames_unit[!ind], "are not defined in the design.")
    }
    attr <- vertex_attr_opt("default")

    for(i in seq_along(dots)) {
      vnames_new <- all.vars(dots[[i]])
      .design$add_variable_node(vnames_new, vnames_unit[i], attr)
    }


  } else if(.design$active == "table") {

  }

  .design
}

#' Set the expected values for variables
#'
#' @param ... Name-value pairs with the name belonging to the variable
#'  that are plan to be recorded from `record_vars()` and the values are
#'  the expected types and values set by helper functions, see `?expect-vars`.
#' @export
expect_vars <- function(.design, ...) {
  .design$append_validation(list2(...))
  .design
}

#' Expected type of entry
#'
#' @param value A vector of possible values for entry.
#' @param range,length A named list with two elements: "operator" and "value" as
#'  provided by helper `as_value()` that gives the possible range of values
#'  that the expected type can take.
#' @name expect-vars
to_be_numeric <- function(range) {
  c(list(type = "decimal"), range)
}

#' @rdname expect-vars
to_be_integer <- function(range) {
  c(list(type = "whole"), range)
}

#' @rdname expect-vars
to_be_date <- function(range) {
  c(list(type = "date"), range)
}

#' @rdname expect-vars
to_be_time <- function(range) {
  c(list(type = "time"), range)
}

#' @rdname expect-vars
to_be_character <- function(length) {
  c(list(type = "textLength"), length)
}

#' @rdname expect-vars
to_be_list <- function(value) {
  list(type = "list", values = value)
}



#' Validation values
#' @param operator
#' @param value An optional value related to operator
#' @param between,not_between An optional numerical vector of size two where the
#'  first entry is the minimum value and the second entry is the maximum value.
#'  For `between`, the value is valid if within the range of minimum and maximum
#'  value inclusive. For `not_between`, the value must lie outside of these values.
#'  @return A list with two elements `operator` and `value`.
as_value <- function(operator = c("=", "==", ">=", "<=", "<", ">", "!="),
                     value, between, not_between) {
  operator <- match.arg(operator)
  if(!missing(between) & !missing(not_between)) {
    abort("You cannot define `between` and `not_between` simultaneously.")
  }
  if(!missing(between)) {
    return(list(operator = "between", value = between))
  }
  if(!missing(not_between)) {
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


fill_responses <- function() {

}

