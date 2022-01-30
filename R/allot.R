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
  .design$allotment <- dots
  for(ialloc in seq_along(dots)) {
    trts <- all.vars(f_lhs(dots[[ialloc]]))
    # there should be only one unit
    unit <- all.vars(f_rhs(dots[[ialloc]]))
    check_var_exists(.design, unit, "edbl_unit")
    uid <- fct_id(.design, unit)
    if(length(trts)) {
      check_var_exists(.design, trts, "edbl_trt")
      tids <- fct_id(.design, trts)
    } else {
      check_trt_exists(.design)
      classes <- fct_class(.design)
      tids <- trt_ids(.design)
    }

    .design$graph$edges <- add_row(.design$graph$edges,
                                  from = tids, to = uid, alloc = ialloc)
  }
  .design
}

#' @export
allocate_trts <- function(design, ...) {
  warn("`allocate_trts` is deprecated. Please use `allot_trts` instead.")
  allot_trts(design, ...)
}

#' A shorthand for allot, assign and serve
#'
#' @inheritParams assign_trts
#' @inheritDotParams allot_trts
#'
#' @export
allot_table <- function(.design, ..., order = "random", seed = NULL, constrain = nesting(.design)) {

  .design %>%
    allot_trts(...) %>%
    assign_trts(order = order, seed = seed, constrain = constrain) %>%
    serve_table()
}
