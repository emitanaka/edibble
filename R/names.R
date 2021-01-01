
#' Names of variables
#'
#' Functions to get or set the names of variables (with particular class) from
#' edibble or edibble nexus object.
#'
#' @param .object,.data,.nexus Either an edibble data frame or edibble nexus.
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
names_vars.edbl_nexus <- function(.nexus, class = NULL) {
  vnexus <- subset_vars(.nexus)
  class <- class %||% V(vnexus)$class
  return(V(vnexus)$name[V(vnexus)$class %in% class])
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
names.edbl_nexus <- function(.nexus, label = TRUE) {
  if(label) return(V(.nexus)$vname)
  V(.nexus)$name
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
