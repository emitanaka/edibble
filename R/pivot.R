
#' Pivot treatments to a wider list or table format
#'
#' @param .data An edibble table.
#' @param trts A vector of treatment (tidyselect compatible). By default it is
#'   NULL and includes all the treatments.
#' @param fcts A vector of factors in the edibble table.
#' @param drop Whether the resulting list should drop to a vector within each list
#'   element if there is only one column. Default is FALSE.
#' @export
pivot_trts_widelist <- function(.data, trts = NULL, fcts = NULL, drop = FALSE) {
  not_edibble(.data)
  data <- as.data.frame(.data, levels_as = "character")
  des <- cook_design(.data)
  if(is.null(trts)) {
    trt_names <- des$trt_names
  } else {
    tloc <- eval_select(enexpr(trts), .data)
    trt_names <- names(tloc)
  }
  if(is.null(fcts)) {
    fct_names <- setdiff(names(data), trt_names)
  } else {
    floc <- eval_select(enexpr(fcts), .data)
    fct_names <- names(floc)
  }
  if(drop & length(fct_names)==1) {
    split(data[[fct_names]], data[trt_names])
  } else {
    split(data[fct_names], data[trt_names])
  }
}

#' @rdname pivot_trts_widelist
#' @export
pivot_trts_widetable <- function(.data, trts = NULL, fcts = NULL) {
  out <- pivot_trts_widelist(.data, trts, fcts)
  ll <- sapply(out, nrow)
  ml <- max(ll)
  for(i in which(ll < ml)) {
    append <- as.data.frame(matrix(NA, ml - ll[i], ncol(out[[i]])))
    colnames(append) <- colnames(out[[i]])
    out[[i]] <- rbind(out[[i]], append)
  }
  res <- as.data.frame(out)
  colnames(res) <- rep(names(out), each = ncol(out[[1]]))
  rownames(res) <- 1:nrow(res)
  res
}
