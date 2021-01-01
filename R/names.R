
#' Names of variables
#'
#' Functions to get or set the names of variables (with particular class) from
#' edibble or edibble graph object.
#'
#' @param .object,.data,.data Either an edibble data frame or edibble graph.
#' @param class A string.
#' @return A character vector of the names of the variables with the given class.
#' @name names-ed-vars
#' @examples
#' names_units(nclassics$split)
#' names_trts(nclassics$split)
#' @export
names_units <- function(.object) {
  names_vars(.object, class = "edbl_unit")
}

#' @rdname names-ed-vars
#' @export
names_trts <- function(.object) {
  names_vars(.object, class = "edbl_trt")
}

#' @rdname names-ed-vars
#' @export
names_resp <- function(.object) {
  names_vars(.object, class = "edbl_resp")
}

#' @rdname names-ed-vars
#' @export
names_vars <- function(.object, ...) {
  UseMethod("names_vars")
}

#' @rdname names-ed-vars
#' @export
names_vars.edbl_graph <- function(.data, class = NULL) {
  vgraph <- subset_vars(.data)
  class <- class %||% V(vgraph)$class
  return(V(vgraph)$name[V(vgraph)$class %in% class])
}

#' @rdname names-ed-vars
#' @export
names_vars.edbl_df <- function(.data, class = NULL) {
  class <- class %||% map_chr(.data, class)
  ind <- map_lgl(.data, function(var) any(class %in% class(var)))
  return(names(.data)[ind])
}

#' @rdname names-ed-vars
#' @export
names.edbl_graph <- function(.data, label = TRUE) {
  if(label) return(V(.data)$vname)
  V(.data)$name
}

#' @rdname names-ed-vars
#' @export
`names<-.edbl_graph` <- function(x, value) {
  igraph::set_vertex_attr(x, "vname", value = value)
}

#' @export
names_eunit <- function(.data, trt = NULL) {
  trt <- trt %||% names_trt(.data)
  vunit <- igraph::neighbors(.data, var_index(.data, trt), mode = "out")
  var_names(.data, vunit)
}

#' @export
names_ounit <- function(.data, resp = NULL) {
  resp <- resp %||% names_resp(.data)
  vunit <- igraph::neighbors(.data, var_index(.data, resp), mode = "out")
  var_names(.data, vunit)
}
