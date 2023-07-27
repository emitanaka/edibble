#' Generate a sequence of labels with custom formatting options
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
#' # Example usage of the function
#' label_seq_to_length(to = 10, length = 5, by = 2)
#' @name label_seq
NULL

#' @rdname label_seq
#' @export
label_seq_from_to <- function(from = 1L, to = 1L, by = 1L,
                              prefix = "", suffix = "",
                              sep_prefix = "", sep_suffix = "",
                              leading_zero = TRUE) {

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
                                  leading_zero = TRUE) {

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
                                  leading_zero = TRUE) {

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
                             leading_zero = TRUE) {

  levels <- seq_len(length)
  label_form(levels, leading_zero,
             prefix, suffix,
             sep_prefix, sep_suffix)
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
