
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
#' fct_attrs(c("A", "B"))
#' @return An `edbl_lvls` object.
#' @export
fct_attrs <- function(.levels, ...) {
  dots <- dots_list(..., .named = TRUE, .homonyms = "keep", .ignore_empty = "all")
  if(length(dots)) {
    for(x in dots) {
      if(!is_null(x)) {
        vctrs::vec_assert(x, size = 1)
      } else {
        dots <- NULL
      }
    }
  }

  attr(.levels, "attrs") <- dots
  .levels
}


#' Setting the traits of the levels
#'
#'
#' @param value A vector of the level values.
#' @param data A list or data frame of the same size as the `levels`.
#' @param ... Name-value pair denoting other level attributes. The value should be the same
#'  length as `levels` or a single value.
#' @importFrom vctrs new_rcrd vec_data
#' @examples
#' lvls(c("A", "B"))
#' @return An edbl_lvls object.
#' @export
lvls <- function(value = NULL, data = NULL, ...) {
  if(!is_null(data) && isTRUE(attr(value, "column"))) {
    pos <- eval_select(value[[1]], data)
    value <- data[[pos]]
    data <- data[-pos]
  }
  if(length(unique(value)) != length(value)) {
    dups <- value[duplicated(value)]
    abort(paste0("The level values should be distinct.",
                 " The values ", .combine_words(dups), " are duplicated."))
  }

  new_rcrd(c(list2(..value.. = value, ...), data), class = "edbl_lvls")
}

#' Select a column.
#'
#' This is a helper function to select a column when data is supplied
#' for `lvls`.
#'
#' @param x The column to select. Can be unquoted name or the column index.
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


