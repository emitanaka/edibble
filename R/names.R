
#' Names of variables
#'
#' Functions to get or set the names of variables (with particular class) from
#' edibble or edibble graph object.
#'
#' @param .data Either an edibble data frame or edibble graph.
#' @param class A string.
#' @return A character vector of the names of the variables with the given class.
#' @name names-ed-vars
#' @examples
#' names_units(nclassics$split)
#' names_trts(nclassics$split)
names_units <- function(.data) {
  names_vars(.data, class = "edbl_unit")
}

#' @rdname names-ed-vars
names_trts <- function(.data) {
  names_vars(.data, class = "edbl_trt")
}

#' @rdname names-ed-vars
names_resp <- function(.data) {
  names_vars(.data, class = "edbl_resp")
}

#' @rdname names-ed-vars
names_vars <- function(.data, ...) {
  UseMethod("names_vars")
}

# Get names by vertex class
# @description
# Get the name of variables by class.
# @param class The class name.
names_by_class <- function(.graph, class = NULL) {
  vgraph <- subset_vars(.graph)
  class <- class %||% V(vgraph)$class
  return(V(vgraph)$name[V(vgraph)$class %in% class])
}

names_vars.EdibbleDesign <- function(.design, class = NULL) {
  names_vars.edbl_graph(.design$graph, class = class)
}

#' @rdname names-ed-vars
#' @importFrom igraph V
names_vars.edbl_graph <- function(.graph, class = NULL) {
  vgraph <- subset_vars(.graph)
  class <- class %||% V(vgraph)$class
  return(V(vgraph)$name[V(vgraph)$class %in% class])
}

#' @rdname names-ed-vars
names_vars.edbl_table <- function(.data, class = NULL) {
  class <- class %||% map_chr(.data, class)
  ind <- map_lgl(.data, function(var) any(class %in% class(var)))
  return(names(.data)[ind])
}

#' @rdname names-ed-vars
#' @importFrom igraph V
names.edbl_graph <- function(.data, label = TRUE) {
  if(label) return(V(.data)$vname)
  V(.data)$name
}

#' @rdname names-ed-vars
#' @importFrom igraph set_vertex_attr
`names<-.edbl_graph` <- function(x, value) {
  abort("You should not change the name of the edibble graph outside EdibbleDesign.")
}


