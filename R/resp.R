
#' Measure response of given units
#'
#' @description
#' This function creates new nodes to edibble graph with the name
#' corresponding to the intended response that will be measured.
#'
#' @param .data An `edbl_graph` object
#' @param ... Name-value pair.
#'
#' @export
measure_response <- function(.data, ...,
                             .name_repair = c("check_unique", "unique", "universal", "minimal")) {
  set_vars(.data, ..., .name_repair = .name_repair,
           .class = "edbl_resp")
}

fill_response <- function() {

}
