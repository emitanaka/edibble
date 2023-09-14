
#' Set the experimental context as metadata
#'
#' These are structured information that can be encoded in into the design
#' object. By encoding this information, you can make it interoperable.
#' If you use [export_design()], the information is exported to the title sheet
#' of the excel output.
#'
#' @param .edibble An edibble table or design.
#' @param ... A series of name-value pairs where the name corresponds to the
#'  name of the metadata nad the value corresponds to the actual metadata value.
#'  If the name is omitted, then no name to the metadata is assigned for the
#'  corresponding value.
#' @examples
#' crd <- design("Completely Randomised Design") %>%
#'   set_units(subject = 30) %>%
#'   set_trts(vaccine = c("A", "B")) %>%
#'   set_context(aim = "Testing for new flu vaccine.",
#'               contact = "emi.tanaka (at) anu.edu",
#'               "Funded by Better Experiments Institute.") %>%
#'   allot_trts(vaccine ~ subject) %>%
#'   assign_trts("random", seed = 222)
#'
#' crd$context
#'
#' @export
set_context <- function(.edibble, ...) {
  not_edibble(.edibble)
  dots <- dots_list(..., .ignore_empty = "trailing")
  des <- edbl_design(.edibble)
  des$context <- c(des$context, dots)
  if(is_edibble_table(.edibble)) {
    attr(.edibble, "design") <- des
    .edibble
  } else {
    des
  }
}
