
#' Measure response of given units
#'
#' @description
#' This function creates new nodes to edibble nexus with the name
#' corresponding to the intended response that will be measured.
#'
#' @param .nexus An `edbl_nexus` object
#' @param ... Name-value pair.
#'
#' @export
measure_response <- function(.nexus, ...,
                             .name_repair = c("check_unique", "unique", "universal", "minimal")) {
  set_vars(.nexus, ..., .name_repair = .name_repair,
           .class = "edbl_resp")
}

fill_response <- function() {

}
