
#' Setting the traits of factors
#'
#' This function is used to set characteristics of the factors.
#'
#' @param .levels Either a short hand given as either as a single integer (number of levels),
#'  a vector or levels created from `lvls()`.
#' @param ... A name-value pair of attributes. The value must be a scalar and
#' attributed to the whole factor (not individual levels).
#' The values are added as attributes to the output object.
#'
#' @seealso lvls
#' @examples
#' fct(c("A", "B"))
#' @export
fct <- function(.levels = character(), ...) {
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
  class(.levels) <- c("edbl_fcts", class(.levels))
  .levels
}

#' @rdname fct
#' @export
fct_attrs <- fct

#' Setting the traits of the levels
#'
#'
#' @param value A vector of the level values.
#' @param n The number of replicate (if applicable).
#' @param data A list or data frame of the same size as the `levels`.
#' @param ... Name-value pair denoting other level attributes. The value should be the same
#'  length as `levels` or a single value.
#' @importFrom vctrs new_rcrd vec_data
#' @examples
#' lvls(c("A", "B"))
#' @return An edbl_lvls object.
#' @export
lvls <- function(value = NULL, n = NA_integer_, data = NULL, ...) {
  if(!is_null(value)) {
    if(!is_null(data) && isTRUE(attr(value, "column"))) {
      pos <- eval_select(value[[1]], data)
      value <- data[[pos]]
      data <- data[-pos]
    }
    if(!is_null(data) && isTRUE(attr(n, "column"))) {
      pos <- eval_select(n[[1]], data)
      n <- data[[pos]]
      data <- data[-pos]
    }
    if(length(unique(value)) != length(value)) {
      dups <- value[duplicated(value)]
      abort(paste0("The level values should be distinct.",
                   " The values ", .combine_words(dups), " are duplicated."))
    }
    n <- vctrs::vec_recycle(n, length(value))

    new_rcrd(c(list2(..value.. = value, ..n.. = n, ...), data), class = "edbl_lvls")
  } else {
    structure(c(list2(..value.. = value, ..n.. = n, ...), data), class = "edbl_lvls")
  }
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
format.edbl_fct <- function(x, ...) {
  if(inherits(x, "factor")) {
    return(levels(x)[x])
  }
  x
}

#' @export
format.edbl_lvls <- function(x, ...) {
  levels(x)
}

#' @export
levels.edbl_lvls <- function(x, ...) {
  vec_data(x)[["..value.."]]
}


#' Set the experimental context as metadata
#'
#' These are structured information that can be encoded in into the design
#' object. By encoding this information, you can make it interoperable.
#' If you use [export_design()], the information is exported to the title sheet
#' of the excel output.
#'
#' @param .edibble An edibble table or design.
#' @param ... A series of name-value pairs where the name corresponds to the
#'  name of the metadata nad the value corresponds to the actual metadata value.
#'  If the name is omitted, then no name to the metadata is assigned for the
#'  corresponding value.
#' @examples
#' des <- set_attrs(design(aim = "Testing for new flu vaccine.",
#'                         contact = "emi.tanaka (at) anu.edu",
#'                        "Funded by Better Experiments Institute.") )
#'
#' des$context
#'
#' @export
set_attrs <- function(.edibble = design(), ...) {
  not_edibble(.edibble)
  dots <- dots_list(..., .ignore_empty = "trailing")
  dotsnm <- names(dots)
  des <- edbl_design(.edibble)
  prov <- activate_provenance(des)
  for(idot in seq_along(dots)) {
    dot <- dots[[idot]]
    dotnm <- dotsnm[idot]
    if(is_edibble_design(dot)) {
      des$context <- c(des$context, dot$context)
    } else if(inherits(dot, "edbl_fcts")) {
      context_fcts <- attr(dot, "attrs")
      # TODO

    } else if(inherits(dot, "edbl_lvls")) {
      n <- dot[["..n.."]]
      attrs <- dot[setdiff(names(dot), c("..value..", "..n.."))]
      # TO DO
    }
  }


  if(is_edibble_table(.edibble)) {
    attr(.edibble, "design") <- des
    .edibble
  } else {
    des
  }
}
