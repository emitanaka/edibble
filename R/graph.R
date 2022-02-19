





# data or vector --------------------------------------------------

#' Cook the design into Edibble object
#' @export
cook_design <- function(x) {
  des <- edbl_design(x)
  if(!is_environment(des$kitchen)) {
    abort("The kitchen is not included in the design.")
  }
  return(des$kitchen$new(des))
}



#' Get the node or edge data from an edibble design
#'
#' @inheritParams var_ids
#' @family design manipulators
#' @name design_data
NULL

#' @rdname design_data
#' @export
fct_nodes <- function(edibble) {
  prep <- cook_design(edibble)
  prep$fct_nodes
}

#' @rdname design_data
#' @export
fct_edges <- function(edibble) {
  prep <- cook_design(edibble)
  prep$fct_edges
}



#' @rdname design_data
#' @export
lvl_nodes <- function(edibble) {
  prep <- cook_design(edibble)
  prep$lvl_nodes
}

#' @rdname design_data
#' @export
lvl_edges <- function(edibble) {
  prep <- cook_design(edibble)
  prep$lvl_edges
}



