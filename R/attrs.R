
#' Setting the traits of factors
#'
#' This function is used to set characteristics of the factors.
#'
#' @param levels An `edbl_lvls` object that should contain information about the levels
#'   in the factor.
#' @param label A string that denotes the long name of the factor.
#' @param description The text description of the factor.
#' @param unit_of_measure A string denoting the unit of measurement if applicable.
#' @param class An optional subclass.
#' @param ... A name-value pair of attributes. The value must be a scalar and
#' attributed to the whole factor (not individual levels).
#' The values are added as attributes to the output object.
#'
#' @seealso lvl_traits
#' @return An `edbl_lvls` object.
#' @export
fct_attrs <- function(levels = NULL,
                       label = NULL,
                       description = NULL,
                       unit_of_measure = NULL,
                       class = NULL,
                       ...) {

  class(levels) <- c(class, class(levels))
  attr(levels, "label") <- label
  attr(levels, "description") <- description
  attr(levels, "unit_of_measure") <- unit_of_measure
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
#' This data frame can be accessed with `lvl_data()`.
#'
#' @param levels A vector that either denotes the index number or short name of the levels.
#' @param labels An optional character vector that is the long name format of `levels`.
#' @param prefix The prefix of the labels.
#' @param suffix The suffix of the labels.
#' @param sep A string to add between `prefix` and `levels`.
#' @param include_leading_zero A logical value to indicate whether there should
#'  be a leading zero added to level indexes. This is ignored if `levels` is not numeric.
#' @param data A list or data frame of the same size as the `levels`.
#' @param ... Name-value pair denoting other level attributes. The value should be the same
#'  length as `levels` or a single value.
#' @importFrom vctrs new_rcrd vec_data
#' @export
lvl_attrs <- function(levels = NULL,
                       labels = NULL,
                       prefix = "",
                       suffix = "",
                       sep = edibble_labels_opt("sep"),
                       include_leading_zero = edibble_labels_opt("leading_zero"),
                       data = NULL, ...) {
  form <- ifelse(vec_is(levels, numeric(), 1),
                 ifelse(include_leading_zero,
                        paste0("%s%s%.", ndigits(max(levels)), "d%s"),
                        "%s%s%d%s"),
                 "%s%s%s%s")
  name <- sprintf(form, prefix, sep, levels, suffix)
  labels <- labels %||% name
  new_rcrd(c(list2(name = name, label = labels, ...), data), class = "edbl_lvls")
}

#' @export
format.edbl_lvls <- function(x, ...) {
  levels(x)
}

#' @export
levels.edbl_lvls <- function(x, ...) {
  lvl_data(x)$name
}


lvl_data <- function(x) {
  vec_data(x)
}
