
#' Setting the traits of factors
#'
#' This function is used to set characteristics of the factors.
#'
#' @param levels An `edbl_lvls` object that should contain information about the levels
#'   in the factor.
#' @param label A string that denotes the long name of the factor.
#' @param description The text description of the factor.
#' @param class An optional subclass.
#' @param ... A name-value pair of attributes. The value must be a scalar and
#' attributed to the whole factor (not individual levels).
#' The values are added as attributes to the output object.
#'
#' @seealso lvl_traits
#' @examples
#' fct_attrs(levels = c("A", "B"))
#' @return An `edbl_lvls` object.
#' @export
fct_attrs <- function(levels = NULL,
                      label = NULL,
                      description = NULL,
                      n = NULL,
                      class = NULL,
                      ...) {

  class(levels) <- c(class, class(levels))
  attr(levels, "label") <- label
  attr(levels, "description") <- description
  dots <- dots_list(..., .named = TRUE, .homonyms = "keep", .ignore_empty = "all")
  dots_names <- names(dots)
  for(i in seq_along(dots)) {
    attr(levels, dots_names[i]) <- dots[[i]]
  }
  levels
}


#' Setting the traits of the levels
#'
#' Use this function to create a "vector" of levels. The vector is actually comprised of a
#' data frame with a column `labels` and other columns with corresponding level attribute (if any).
#'
#' @param levels A vector that either denotes the index number or short name of the levels.
#' @param labels An optional character vector that is the long name format of `levels`.
#'  be a leading zero added to level indexes. This is ignored if `levels` is not numeric.
#' @param data A list or data frame of the same size as the `levels`.
#' @param ... Name-value pair denoting other level attributes. The value should be the same
#'  length as `levels` or a single value.
#' @importFrom vctrs new_rcrd vec_data
#' @examples
#' lvl_attrs(c("A", "B"))
#' @return An edbl_lvls object.
#' @export
lvl_attrs <- function(levels = NULL,
                      data = NULL, ...) {

  new_rcrd(c(list2(value = levels, ...), data), class = "edbl_lvls")
}

#' @export
lvls <- function(.x, ...) {
  new_rcrd(list2(.value = .x, ...), class = "edbl_lvls")
}

#' @export
lvls_data <- function(data, value = NULL) {
  value <- enexpr(value)
  if(is_null(value)) {
    pos <- 1L
  } else {
    pos <- eval_select(value, data)
  }
  new_rcrd(c(list2(.value = data[[pos]]), data[-pos]), class = "edbl_lvls")
}

#' @export
format.edbl_lvls <- function(x, ...) {
  levels(x)
}

#' @export
levels.edbl_lvls <- function(x, ...) {
  vec_data(x)$.value
}


