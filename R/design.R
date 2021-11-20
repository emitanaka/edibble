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
                 graph = initialise_edibble_graph()),
            class = c("edbl_design", "edbl"))
}

# initialise graph structure -----------------------------------------------

initialise_edibble_graph <- function() {
  fnodes <- data.frame(id = integer(), label = character(), class = character(), stringsAsFactors = FALSE)
  lnodes <- data.frame(idvar = integer(), id = integer(), label = character(), stringsAsFactors = FALSE)
  edges <-  data.frame(from = integer(), to = integer(),
                       alloc = integer(),
                       stringsAsFactors = FALSE)
  structure(list(nodes = fnodes,
                 edges = edges,
                 levels = list(nodes = lnodes,
                               edges = edges)),
            class = "edbl_graph")
}

