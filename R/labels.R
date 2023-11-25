#' Generate a sequence of labels with custom formatting options
#'
#' These can be handy for generating pseudo labels for the levels or
#' factor names using `fct_generator`
#'
#' @param from An integer specifying the starting value (inclusive) of the sequence.
#' @param to An integer specifying the ending value (inclusive) of the sequence.
#' @param by An integer specifying the increment between values in the sequence.
#' @param length An integer specifying the desired length of the sequence.
#' @param prefix A character string to be prepended to the labels.
#' @param suffix A character string to be appended to the labels.
#' @param sep_prefix A character string used to separate the prefix from the labels.
#' @param sep_suffix A character string used to separate the suffix from the labels.
#' @param leading_zero A logical value indicating whether to add leading zeros to the labels.
#'  If integer, then pad based on the number supplied.
#'
#' @return A character vector containing the labels generated from the sequence.
#'
#' @examples
#' label_seq_to_length(to = 10, length = 5, by = 2)
#' label_seq_from_to(from = 8, to = 10, leading_zero = 3)
#' label_seq_length(10, leading_zero = FALSE)
#' @name label_seq
NULL

#' @rdname label_seq
#' @export
label_seq_from_to <- function(from = 1L, to = 1L, by = 1L,
                              prefix = "", suffix = "",
                              sep_prefix = "", sep_suffix = "",
                              leading_zero = edibble_labels_opt("leading_zero")) {

  levels <- seq(from = from, to = to, by = by)
  label_form(levels, leading_zero,
             prefix, suffix,
             sep_prefix, sep_suffix)
}

#' @rdname label_seq
#' @export
label_seq_from_length <- function(from = 1L, length = 1L, by = 1L,
                                  prefix = "", suffix = "",
                                  sep_prefix = "", sep_suffix = "",
                                  leading_zero = edibble_labels_opt("leading_zero")) {

  levels <- seq(from = from, by = by, length.out = length)
  label_form(levels, leading_zero,
             prefix, suffix,
             sep_prefix, sep_suffix)
}

#' @rdname label_seq
#' @export
label_seq_to_length <- function(to = 1L, length = 1L, by = 1L,
                                  prefix = "", suffix = "",
                                  sep_prefix = "", sep_suffix = "",
                                  leading_zero = edibble_labels_opt("leading_zero")) {

  levels <- seq(to = to, by = by, length.out = length)
  label_form(levels, leading_zero,
             prefix, suffix,
             sep_prefix, sep_suffix)
}

#' @rdname label_seq
#' @export
label_seq_length <- function(length = 1L,
                             prefix = "", suffix = "",
                             sep_prefix = "", sep_suffix = "",
                             leading_zero = edibble_labels_opt("leading_zero")) {

  levels <- seq_len(length)
  label_form(levels, leading_zero,
             prefix, suffix,
             sep_prefix, sep_suffix)
}

#' Label with nested or distinct labels
#'
#' @param x A unit vector.
#' @export
label_nested <- function(x) {
  attr(x, "label-nested") %||% x
}

#' @rdname label_nested
#' @export
label_distinct <- function(x) {
  attr(x, "label-non-nested") %||% x
}

#' @rdname label_nested
#' @export
index_levels <- function(x) {
  as.integer(label_distinct(x))
}


label_form <- function(levels, leading_zero,
                       prefix, suffix,
                       sep_prefix, sep_suffix) {
  form <- ifelse(is.numeric(leading_zero),
                 paste0("%s%s%.", leading_zero, "d%s%s"),
                 ifelse(leading_zero,
                        paste0("%s%s%.", ndigits(max(levels)), "d%s%s"),
                        "%s%s%d%s%s"))

  sprintf(form, prefix, sep_prefix, levels, sep_suffix, suffix)
}


#' Factor name generator
#'
#' Generate a factor with custom levels and repetitions.
#'
#' This function creates a factor with custom labels and specified repetitions for each label.
#'
#' @param labels A character vector specifying the custom labels for the factor levels.
#' @param nlevels An integer or a vector of integers indicating the number of repetitions for each label.
#'               If a single integer is provided, it is recycled to match the length of \code{labels}.
#'               If a vector is provided, it should have the same length as \code{labels}.
#'
#' @return A factor with custom levels and repetitions.
#'
#' @examples
#' # Example usage of the function
#' fct_generator(labels = c("A", "B", "C"), nlevels = 3)
#'
#' @export
fct_generator <- function(labels, nlevels) {
  lvl_list <- as.list(vctrs::vec_recycle(nlevels, length(labels)))
  names(lvl_list) <- labels
  structure(lvl_list, class = "fct_names")
}
