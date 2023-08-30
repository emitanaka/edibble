
#' Setting the traits of factors
#'
#' This function is used to set characteristics of the factors.
#'
#' @param .levels An `edbl_lvls` object that should contain information about the levels
#'   in the factor.
#' @param ... A name-value pair of attributes. The value must be a scalar and
#' attributed to the whole factor (not individual levels).
#' The values are added as attributes to the output object.
#'
#' @seealso lvl_traits
#' @examples
#' fct_attrs(levels = c("A", "B"))
#' @return An `edbl_lvls` object.
#' @export
fct_attrs <- function(.levels = NULL,
                      ...) {
  dots <- dots_list(..., .named = TRUE, .homonyms = "keep", .ignore_empty = "all")
  attr(.levels, "attrs") <- dots
  .levels
}


#' Setting the traits of the levels
#'
#' Use this function to create a "vector" of levels. The vector is actually comprised of a
#' data frame with a column `labels` and other columns with corresponding level attribute (if any).
#'
#' @param levels A vector that either denotes the index number or short name of the levels.
#' @param data A list or data frame of the same size as the `levels`.
#' @param ... Name-value pair denoting other level attributes. The value should be the same
#'  length as `levels` or a single value.
#' @importFrom vctrs new_rcrd vec_data
#' @examples
#' lvl_attrs(c("A", "B"))
#' @return An edbl_lvls object.
#' @export
lvls <- function(value = NULL,
                      data = NULL, ...) {
  if(!is_null(data) && isTRUE(attr(value, "column"))) {
    pos <- eval_select(value[[1]], data)
    value <- data[[pos]]
    data <- data[-pos]
  }

  new_rcrd(c(list2(..value.. = value, ...), data), class = "edbl_lvls")
}

#' @export
column <- function(x) {
  structure(list(enexpr(x)), column = TRUE)
}


#' @export
format.edbl_lvls <- function(x, ...) {
  levels(x)
}

#' @export
levels.edbl_lvls <- function(x, ...) {
  vec_data(x)[["..value.."]]
}


