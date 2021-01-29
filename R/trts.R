#' Set the treatment variables
#'
#' @description
#' This function add a special class, called `edbl_trt`, of edibble variables.
#'
#' @section Definition of _treatment_:
#' The word _treatment_ is sometimes used to refer to one of these variables.
#' When there are more than one treatment variables then this unfortunately
#' confuses whether treatment refers to the variable or the combination of
#' all treatment variables.
#'
#' Treatment is the whole description of what is applied in an experiment.
#'
#' @inheritParams set_units
#' @family user-facing functions
#' @examples
#'
#' start_design() %>%
#'   set_trts(pesticide = c("A", "B", "C"),
#'            dosage = c(0, 10, 20, 30, 40))
#'
#' # you can set treatments to existing edibble
#' lady_tasting_tea %>%
#'   edibble() %>%
#'   set_trts(first)
#' @export
set_trts <- function(.design, ...,
                      .name_repair = c("check_unique", "unique", "universal", "minimal")) {
  set_vars(.design, ..., .name_repair = .name_repair, .class = "edbl_trt")
}

#' Define the possible allocation of treatments to units
#'
#' @description
#' This function adds the edges between level nodes in
#' an edibble graph that outlines the possible ways that
#' the treatment may be allocated to the recipient units.
#'
#' @param .data An `edbl_graph` object.
#' @param ... One-sided or two-sided formula. If the input is a one-sided formula
#' then the whole treatment is applied to the specified unit.
#' @param class A sub-class. This is meant so that it can invoke the method
#' `randomise_trts.class`.
#' @importFrom rlang enexprs f_lhs f_rhs
#' @importFrom igraph add_edges
#' @family user-facing functions
#' @examples
#' start_design() %>%
#'   set_units(block = 10,
#'             plot = nested_in(block, 3)) %>%
#'   set_trts(treat = c("A", "B", "C"),
#'            pest = c("a", "b")) %>%
#'   allocate_trts(treat ~ plot,
#'                 pest ~ block)
#'
#' # allocation works on edibble table too
#' lady_tasting_tea %>%
#'   edibble() %>%
#'   set_units(cup) %>%
#'   set_trts(first) %>%
#'   allocate_trts(first ~ cup)
#'
#' @export
allocate_trts <- function(.edibble, ...) {

  not_edibble(.edibble)

  .design <- get_edibble_design(.edibble)

  dots <- enexprs(...)
  for(i in seq_along(dots)) {
      trts <- all.vars(f_lhs(dots[[i]]))
      # there should be only one unit
      unit <- all.vars(f_rhs(dots[[i]]))
      .design$add_allocation(trts, unit)
  }

  update_design(.edibble, .design)
}


get_trt_vars <- function(x) {
  if(is_edibble_graph(x)) {
    return(V(x)$vname[V(x)$class=="edbl_trt"])
  }
  if(is_edibble_df(x)) {
    ind <- unlist(lapply(x, function(var) "edbl_trt" %in% class(var)))
    return(names(x)[ind])
  }
}

get_trt_levels <- function(.data) {
  out <- list()
  vars <- get_trt_vars(.data)
  for(avar in vars) {
    out[[avar]] <- var_levels(.data, avar)
  }
  out
}




n_trts <- function(.data, ...) {
  UseMethod("n_trts")
}

n_trts.edbl_graph <- function(.data) {
  return(prod(lengths(get_trt_levels(.data))))
}

n_trts.edbl_table <- function(.data) {
  ind <- unlist(lapply(.data, function(var) "edbl_trt" %in% class(var)))
  return(prod(lengths(lapply(.data, levels)[ind])))
}

n_vars <- function(.data, class = NULL) {
  UseMethod("n_vars")
}

n_vars.edbl_table <- function(.data, class = NULL) {
  if(is_null(class)) return(ncol(.data))
  sum(map_lgl(.data, function(x) any(class(x) %in% class)))
}


n_vars.edbl_graph <- function(.data, class = NULL) {
  vgraph <- subset(.data, class %in% class, .vtype = "var")
  if(is_null(class)) return(sum(V(.data)$vtype=="var"))
  sum(V(.data)$class %in% class)
}

#' @importFrom pillar pillar_shaft new_pillar_shaft_simple
#' @export
pillar_shaft.edbl_trt <- function(x, ...) {
  out <- format(x)
  new_pillar_shaft_simple(out, align = "right", min_width = 10)
}

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.edbl_trt <- function(x, ...)  {
  paste0("trt(", number_si_prefix(nlevels(x)), ")")
}

#' @importFrom vctrs vec_ptype_full
#' @export
vec_ptype_full.edbl_trt <- function(x, ...) paste0("trt(", nlevels(x), ")")

#' @importFrom vctrs vec_cast
#' @export
vec_cast.edbl_trt.edbl_trt <- function(x, to, ...) {
  x
}


