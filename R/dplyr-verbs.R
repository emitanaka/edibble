#' Extract a single variable
#'
#' This is similar to the `dplyr::pull` function but for the
#' `edbl_nexus` object which requires to be transformed to data frame first.
#'
#' @param x `edbl_nexus` object
#' @param var variable name or position (see \link[dplyr]{pull})
#' @param name Optional vector used as name of extracted variable.
#'
#' @importFrom dplyr pull
#'
#' @export
pull.edbl_nexus<- function(x, var = -1, ...) {
  var <- enexpr(var)
  df <- serve_table(x)
  dplyr::pull(df, var = var, ...)
}
