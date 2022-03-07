#' Define the possible allocation of treatments to units
#'
#' @description
#' This function adds the edges between variable nodes to
#' specify the mapping of units to treatment. This function
#' does not actually assign specific treatment levels onto actual units.
#'
#' @param ... One-sided or two-sided formula. If the input is a one-sided formula
#' then the whole treatment is applied to the specified unit.
#' @inheritParams design-context
#' @family user-facing functions
#' @examples
#' start_design() %>%
#'   set_units(block = 10,
#'             plot = nested_in(block, 3)) %>%
#'   set_trts(treat = c("A", "B", "C"),
#'            pest = c("a", "b")) %>%
#'   allot_trts(treat ~ plot,
#'                 pest ~ block)
#'
#'
#' @export
allot_trts <- function(.design, ..., .record = TRUE) {

  not_edibble(.design)
  if(.record) record_step()

  dots <- list2(...)
  if(!is_null(.design$allotment)) {
    .design$allotment$trts <- c(.design$allotment$trts, dots)
  } else {
    .design$allotment <- list(trts = dots, units = NULL)
  }
  prep <- cook_design(.design)

  for(ialloc in seq_along(dots)) {
    trts <- all.vars(f_lhs(dots[[ialloc]]))
    # there should be only one unit
    unit <- all.vars(f_rhs(dots[[ialloc]]))
    prep$fct_exists(name = unit, class = "edbl_unit")
    uid <- prep$fct_id(unit)
    if(length(trts)) {
      prep$fct_exists(name = trts, class = "edbl_trt")
      tids <- prep$fct_id(trts)
    } else {
      prep$trts_exists()
      classes <- prep$fct_class()
      tids <- prep$trt_ids
    }

    prep$append_fct_edges(data.frame(from = tids, to = uid, alloc = ialloc, type = "allot"))
  }
  prep$design
}


#' @export
allot_units <- function(.design, ..., .record = TRUE) {
  not_edibble(.design)
  if(.record) record_step()

  dots <- list2(...)
  if(!is_null(.design$allotment)) {
    .design$allotment$units <- c(.design$allotment$units, dots)
  } else {
    .design$allotment <- list(trts = NULL, units = dots)
  }
  prep <- cook_design(.design)

  for(ialloc in seq_along(dots)) {
    # there should be only one unit for `big`
    big <- all.vars(f_lhs(dots[[ialloc]]))
    small <- all.vars(f_rhs(dots[[ialloc]]))
    prep$fct_exists(name = small, class = "edbl_unit")
    big_id <- prep$fct_id(big)
    prep$fct_exists(name = big, class = "edbl_unit")
    small_id <- prep$fct_id(small)
    prep$append_fct_edges(data.frame(from = big_id,
                                     to = small_id[length(small_id)],
                                     type = "nest"))
    if(length(small) > 1) {
      prep$append_fct_edges(data.frame(from =  big_id,
                                       to = small_id[length(small_id) - 1],
                                       type = "depends"))
    }
  }
  prep$design

}


#' A shorthand for allot, assign and serve
#'
#' @inheritParams assign_trts
#' @inheritDotParams allot_trts
#'
#' @export
allot_table <- function(.design, ..., order = "random", seed = NULL, constrain = nesting_structure(.design)) {

  .design %>%
    allot_trts(...) %>%
    assign_trts(order = order, seed = seed, constrain = constrain) %>%
    serve_table()
}
