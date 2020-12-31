
#' Names of variables
#'
#' Functions to get or set the names of variables (with particular class) from
#' edibble or edibble nexus object.
#'
#' @param x An edibble or edibble nexus.
#' @param class A string.
#' @return A character vector of the names of the variables with the given class.
#' @name names-ed-vars
#' @examples
#' names_units(nclassics$split)
#' names_trts(nclassics$split)
#' @export
names_units <- function(x) {
  names_vars(x, class = "edbl_unit")
}

#' @rdname names-ed-vars
#' @export
names_trts <- function(x) {
  names_vars(x, class = "edbl_trt")
}

#' @rdname names-ed-vars
#' @export
names_resp <- function(x) {
  names_vars(x, class = "edbl_resp")
}

#' @rdname names-ed-vars
#' @export
names_vars <- function(x, class = NULL) {
  if(is_edibble_nexus(x)) {
    vnexus <- subset_vars(x)
    class <- class %||% V(vnexus)$class
    return(V(vnexus)$name[V(vnexus)$class %in% class])
  }
  if(is_edibble(x)) {
    class <- class %||% map_chr(x, class)
    ind <- map_lgl(x, function(var) any(class %in% class(var)))
    return(names(x)[ind])
  }
}

#' @rdname names-ed-vars
#' @export
names.edbl_nexus <- function(x) {
  V(x)$vname
}

#' @rdname names-ed-vars
#' @export
`names<-.edbl_nexus` <- function(x, value) {
  igraph::set_vertex_attr(x, "vname", value = value)
}

#' @export
names_eunit <- function(.nexus, trt = NULL) {
  trt <- trt %||% names_trt(.nexus)
  vunit <- igraph::neighbors(.nexus, var_index(.nexus, trt), mode = "out")
  var_names(.nexus, vunit)
}

#' @export
names_ounit <- function(.nexus, resp = NULL) {
  resp <- resp %||% names_resp(.nexus)
  vunit <- igraph::neighbors(.nexus, var_index(.nexus, resp), mode = "out")
  var_names(.nexus, vunit)
}
