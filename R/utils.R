

decorate_vars <- function(x, decorate_units, decorate_trts, decorate_resp, classes) {
  edbl_classes <- c("edbl_unit", "edbl_trt", "edbl_resp")
  decorate_fns <- list(decorate_units, decorate_trts, decorate_resp)
  for(i in seq_along(edbl_classes)) {
    index <- which(classes==edbl_classes[i])
    if(length(index) > 0) {
      x[index] <- decorate_fns[[i]](x[index])
    }
  }
  x
}


remove_nulls <- function(.x) {
  .x[!vapply(.x, is.null, logical(1))]
}


compact <- function(.x) {
  .x[!vapply(.x, is_empty, logical(1))]
}

# igraph operations removes previous attributes so
reinstate_graph_attrs <- function(g1, g2) {
  attributes(g1) <- attributes(g2)
  g1
}

#' Find how many digits
ndigits <- function(x) {
  max(c(floor(log10(abs(x))) + 1, edibble_labels_opt("min_ndigits")))
}


#' Helper function to create level names
traits_levels <- function(prefix = "", size = NULL, from = NULL, to = NULL,
                          include_leading_zero = edibble_labels_opt("leading_zero")) {
  if(is_null(size)) {
    labels <- seq(from, to)
    n <- to
  } else {
    labels <- 1:size
    n <- size
  }
  if(!include_leading_zero) {
    paste0(prefix, labels)
  } else {
    sprintf(paste0("%s%.", ndigits(n), "d"), prefix, labels)
  }
}



# number SI prefix --------------------------------------------------------

#' Numbers with SI prefix
#'
#' It's called SI prefix but the letter notation is added as a suffix.
#' The largest prefix is yotta $10^{24}$.
#'
#' @param x A numeric vector to format with SI prefix
#' @source https://en.wikipedia.org/wiki/Metric_prefix
number_si_prefix <- function(x) {
  if(!all(x >= 1)) abort("The numeric vector should be a positive integer.")

  prefix <- c(1, 10^c(k = 3, M = 6, G = 9, T = 12, P = 15, E = 18, Z = 21, Y = 24))
  map_chr(x, function(n) {
    index <- max(which(n/prefix >= 1))
    scale <- 1 / prefix[index]
    symbol <- names(prefix)[index]
    paste0(floor(n * scale), symbol)
  })
}


# inspired by knitr::combine_words
.combine_words <- function(words, sep = ", ", and = " and ",
                           before = "", after = before, fun = function(x) x) {

  words <- fun(paste0(before, words, after))
  n <- length(words)
  if(n <= 1) {
    return(words)
  }

  words[n - 1] = paste0(words[n - 1], and, words[n])
  paste0(words[-n], collapse = sep)
}
