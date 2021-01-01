#' Set units used in experiment
#'
#' @description
#' This function creates new edibble variables of class `edbl_unit`.
#'
#' @inheritParams set_vars
#' @section Definition of _unit_:
#' A _unit_, much like _factor_, is an over-used word but due to lack of a
#' better word, edibble uses the word "unit" to refer to any entity, physical
#' or otherwise, that pertain to the experiment. This function doen't
#' explicitly distinguish between experimental or observational units,
#' nor is a unit limited to these type of units.
#' A unit in edibble can be a blocking factor or discrete time unit.
#'
#' @section Limitations:
#' Currently a unit should only have a discrete set of levels.
#'
#' @examples
#' initiate_design() %>%
#'   set_units(block = 3)
#' @export
set_units <- function(.data, ...) {
  UseMethod("set_units")
}

#' @export
set_units.edbl_df <- function(.data, ...) {

}

#' @export
set_units.edbl_graph <- function(.data, ...,
                      .name_repair = c("check_unique", "unique", "universal", "minimal")) {
  set_vars(.data, ..., .name_repair = .name_repair, .class = "edbl_unit")
}



#' @export
vec_ptype_abbr.edbl_unit <- function(x)  {
  paste0("unit(", number_si_prefix(nlevels(x)), ")")
}

#' @export
vec_ptype_full.edbl_unit <- function(x) paste0("unit(", nlevels(x), ")")

#' @export
vec_cast.edbl_unit.edbl_unit <- function(x, to, ...) {
  x
}

#' Number of units associated with the given variable
#' @param .data An edibble graph or an edibble data frame.
#' @param var The name of the edibble variable. If this is not supplied then
#'  the number of experimental unit is returned.
#' @export
n_units <- function(.data, ...) {
  UseMethod("n_units")
}


#' @rdname n_units
#' @export
n_units.edbl_graph <- function(.data, var = NULL) {
  egraph <- structure(igraph::subgraph.edges(.data, which(E(.data)$etype=="t2vmay")),
                      class = class(.data))
  subset(egraph, name=="t1:t11")
  var <- var %||% names_trts(.data)
  vindex <- igraph::neighbors(.data, var_index(.data, var), mode = "out")
  vname <- var_names(.data, vindex)
  length(var_levels(.data, vname))
}


#' @rdname n_units
#' @export
n_units.edbl_df <- function(.data, var) {
 # [TODO]
}




