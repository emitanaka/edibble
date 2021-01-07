
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
measure_responses <- function(.design, ...,
                             .name_repair = c("check_unique", "unique", "universal", "minimal")) {

  set_vars(.design, ..., .name_repair = .name_repair,
           .class = "edbl_resp")
}



fill_responses <- function() {

}

