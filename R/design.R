#' Start the edibble design
#'
#' @description
#' This function doesn't really do much besides create a new edibble design object.
#'
#' @param name Optional name used as title for printing the design.
#' @return An empty `edbl_design` object.
#' @examples
#' start_design("My design")
#' @seealso Add variables to this design with [set_units()], [set_trts()], and
#' [set_rcrds()].
#' @family user-facing functions
#' @export
start_design <- function(name = NULL) {
  structure(list(name = name %||% "An edibble design",
                 vgraph = empty_edibble_graph("edbl_vgraph"),
                 lgraph = empty_edibble_graph("edbl_lgraph")),
            class = c("edbl_design", "edbl"),
            active = "graph")
}


