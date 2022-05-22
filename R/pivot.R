
#' Pivot treatments to a wider list or table format
#'
#' @param .data An edibble table.
#' @param trts A vector of character with the treatment names. By default it is
#'   NULL and includes all the treatments.
#' @export
pivot_trts_widelist <- function(.data, trts = NULL) {
  not_edibble(.data)
  des <- cook_design(.data)
  if(is.null(trts)) {
    trt_names <- des$trt_names
  } else {
    trt_names <- trts
  }
  split(.data[setdiff(names(.data), trt_names)], x[trt_names])
}

#' @describeIn pivot_trts_widelist
#' @export
pivot_trts_widetable <- function(.data, trts = NULL) {
  out <- pivot_trts_widelist(.data, trts)
  ll <- sapply(out, nrow)
  ml <- max(ll)
  for(i in which(ll < ml)) {
    append <- as.data.frame(matrix(NA, ml - ll[i], ncol(out[[i]])))
    colnames(append) <- colnames(out[[i]])
    out[[i]] <- rbind(out[[i]], append)
  }
  res <- as.data.frame(out)
  colnames(res) <- rep(names(out), each = ncol(out[[1]]))
  res
}
