
#' Setting the traits of factors
#'
#' This function is used to set characteristics of the factors.
#'
#' @param n The number of levels. This is ignored if `labels` is provided.
#' @param longname The long name of the variable or level.
#' @param desc The text description of the factor.
#' @param unit A string denoting the unit of measurement if applicable.
#' @param labels A string vector that denotes the short name of the levels.
#' @param prefix The prefix of the labels.
#' @param suffix The suffix of the labels.
#' @param include_leading_zero A logical value to indicate whether there should
#' be a leading zero added to level indexes. This is ignored if `labels` is provided.
#' @param ... A name-value pair of attributes. The value must be a scalar and
#' attributed to the whole variable (not individual levels).
#' The values are added as attributes to the output object.
#'
#' @export
traits <- function(n = NULL, longname = NULL,
                   desc = NULL, unit = NULL,
                   labels = NULL, prefix = "", suffix = "",
                   class = NULL, sep = edibble_labels_opt("sep"),
                   include_leading_zero = edibble_labels_opt("leading_zero"),
                   ...) {
  if(is_null(labels)) {
    res <- traits_levels(prefix = prefix, suffix = suffix, sep = sep,
                         size = n, include_leading_zero = include_leading_zero)
  } else {
    res <- labels
  }
  class(res) <- c(class, "edbl_trait", class(res))
  attr(res, "longname") <- longname
  attr(res, "desc") <- desc
  attr(res, "unit") <- unit
  dots <- dots_list(..., .named = TRUE, .homonyms = "keep", .ignore_empty = "all")
  dots_names <- names(dots)
  for(i in seq_along(dots)) {
    attr(res, dots_names[i]) <- dots[[i]]
  }
  res
}

#' Helper function to create level names
traits_levels <- function(prefix = "", suffix = "", sep = edibble_labels_opt("sep"),
                          size = NULL, from = NULL, to = NULL,
                          include_leading_zero = edibble_labels_opt("leading_zero")) {
  if(is_null(size)) {
    labels <- seq(from, to)
    n <- to
  } else {
    labels <- seq(1, size)
    n <- size
  }
  if(!include_leading_zero) {
    paste0(prefix, labels, suffix)
  } else {
    sprintf(paste0("%s%s%.", ndigits(n), "d%s"), prefix, sep, suffix, labels)
  }
}

