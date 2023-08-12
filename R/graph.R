





# data or vector --------------------------------------------------

#' Activate the provenance in the edibble design object
#'
#' This is a developer function to create a new Kitchen class with
#' the existing design.
#'
#' @param x An edibble object.
#' @return A Kitchen object.
#' @examples
#' activate_provenance(takeout())
#' @export
activate_provenance <- function(.edibble,
                                overwrite = c("graph", "anatomy", "recipe")) {
  des <- edbl_design(.edibble)
  prov <- des$provenance
  if(!is_environment(prov)) {
    abort("The provenance is not included in the design.")
  }
  prov$reactivate(des, overwrite)
  return(prov)
}


#' Get the node or edge data from an edibble design
#'
#' @param edibble An edibble object.
#' @family design manipulators
#' @name design_data
NULL

#' @rdname design_data
#' @export
fct_nodes <- function(edibble) {
  prov <- activate_provenance(edibble)
  prov$fct_nodes
}

#' @rdname design_data
#' @export
fct_edges <- function(edibble) {
  prov <- activate_provenance(edibble)
  prov$fct_edges
}



#' @rdname design_data
#' @export
lvl_nodes <- function(edibble) {
  prov <- activate_provenance(edibble)
  prov$lvl_nodes
}

#' @rdname design_data
#' @export
lvl_edges <- function(edibble) {
  prov <- activate_provenance(edibble)
  prov$lvl_edges
}



