#' Set units used in experiment
#'
#' @description
#' This function sets new edibble variables of class `edbl_unit`. More
#' specifically, this means that new nodes are added to the `edbl_graph`.
#'
#' @inheritParams design-context
#' @param ... Either a name-value pair or a series of the names.
#' @param .name_repair Same as the argument in `tibble::tibble()`.
#' @section Definition of _unit_:
#' A _unit_, much like _factor_, is an over-used word but due to lack of a
#' better word, edibble uses the word "unit" to refer to any entity, physical
#' or otherwise, that pertain to the experiment. This function doen't
#' explicitly distinguish between experimental or observational units,
#' nor is a unit limited to these type of units.
#' A unit in edibble can be a blocking factor or even a discrete time unit.
#'
#' @section Limitations:
#' Currently a unit should only have a discrete set of levels and
#' you need to know the number of levels prior to setting the units.
#'
#' @examples
#' # 30 rats
#' start_design() %>%
#'   set_units(rat = 30) %>%
#'   serve_table()
#'
#' # 4 girls named "Anna", "Betty", "Carol", "Diana"
#' start_design() %>%
#'   set_units(girl = c("Anna", "Betty", "Carol", "Diana")) %>%
#'   serve_table()
#'
#' # 3 companies, with 10 boxes each
#' start_design() %>%
#'   set_units(company = c("A", "B", "C"),
#'                 box = nested_in(company, 10))
#'
#' # 2 classes, one with 10 students, the other with 20 students
#' start_design() %>%
#'   set_units(class = 2,
#'             student = nested_in(class,
#'                                 1 ~ 10,
#'                                 2 ~ 20))
#'
#' # 4 countries with 10 people from Australia & New Zealand and 20 from the rest
#' start_design() %>%
#'   set_units(country = c("AU", "NZ", "USA", "JPN"),
#'             person = nested_in(country,
#'                                c("AU", "NZ") ~ 10,
#'                                            . ~ 20)) %>%
#'   serve_table()
#'
#' # if using existing data then it specifies which variables are units
#' lady_tasting_tea %>%
#'   set_units(cup)
#'
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






