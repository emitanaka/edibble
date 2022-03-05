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
#'
#' @family user-facing functions
#' @export
set_units <- function(.design, ...,
                      .name_repair = c("check_unique", "unique", "universal", "minimal"),
                      .record = TRUE) {
  if(.record) record_step()
  set_vars(.design, ..., .name_repair = .name_repair, .class = "edbl_unit")
}

#' @importFrom tidyselect eval_select
#' @export
select_units <- function(prep, ...) {
  vlevs <- prep$fct_levels()
  loc <- eval_select(tidyselect::all_of(expr(c(...))), vlevs)
  keep_units <- names(vlevs)[loc]
  keep_uids <- prep$fct_id(keep_units)
  keep_uids_ancestors <- prep$fct_ancestor(keep_uids, class = "edbl_unit")
  sprep <- prep$clone()
  fnodes <- prep$fct_nodes
  fedges <- prep$fct_edges
  lnodes <- prep$lvl_nodes
  ledges <- prep$lvl_edges
  sprep$fct_nodes <- fnodes[fnodes$id %in% keep_uids_ancestors, ]
  sprep$fct_edges <- fedges[fedges$to %in% keep_uids_ancestors & fedges$from %in% keep_uids_ancestors,]
  sprep$lvl_nodes <- lnodes[lnodes$idvar %in% keep_uids_ancestors, ]
  keep_lids_ancestors <- sprep$lvl_id()
  sprep$lvl_edges <- ledges[ledges$to %in% keep_lids_ancestors & ledges$from %in% keep_lids_ancestors,]
  sprep
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

#' @importFrom pillar pillar_shaft new_pillar_shaft_simple
#' @export
pillar_shaft.edbl_unit <- function(x, ...) {
  out <- format(x)
  new_pillar_shaft_simple(out, align = "right", min_width = 11)
}

