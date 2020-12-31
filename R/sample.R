
#' Sample a subset of the levels for the given variable
#'
#'
#' @param .data An edibble table or edibble nexus.
#' @param ... The edibble variable(s) in `.data` to subset levels from.
#' @param n,prop Supply either `n`, the number of levels to sample, or `prop`,
#'   the proportion of levels to sample.
#' @export
sample_levels <- function(.data, ..., n, prop, weight_by = NULL) {
  UseMethod("sample_level")
}


sample_levels.default <- function(.data, ..., n, prop, weight_by = NULL) {

}

#' @export
sample_levels.edbl_nexus <- function(.data, ..., n, prop, weight_by = NULL) {


}

#' @export
sample_levels.edbl_df <- function(.data, ..., n, prop, weight_by = NULL) {

}
