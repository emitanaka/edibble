#' Set units used in experiment
#'
#' @description
#' This function creates new edibble variables of class `edbl_unit`.
#'
#' @inheritParams design-context
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]><[`tidy-select`][dplyr::dplyr_tidy_select]>
#' Name-value pair.
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
#' @family user-facing functions
#' @export
set_units <- function(.edibble, ...,
                      .name_repair = c("check_unique", "unique", "universal", "minimal")) {

  set_vars(.edibble, ..., .name_repair = .name_repair, .class = "edbl_unit")
}


#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.edbl_unit <- function(x)  {
  paste0("unit(", number_si_prefix(nlevels(x)), ")")
}
#' @importFrom vctrs vec_ptype_full
#' @export
vec_ptype_full.edbl_unit <- function(x) paste0("unit(", nlevels(x), ")")
#' @importFrom vctrs vec_cast
#' @export
vec_cast.edbl_unit.edbl_unit <- function(x, to, ...) {
  x
}

#' Number of units associated with the given variable
#' @export
n_units <- function(.design) {
  nrow(.design$table)
}






