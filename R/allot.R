#' Define allotment of treatments to units
#'
#' @description
#' This function adds the edges between factor nodes to describe the
#' high-level relationship between factors.
#' This function does not actually assign edges between level nodes.
#'
#' @param ... One-sided or two-sided formula. If the input is a one-sided formula
#' then the whole treatment is applied to the specified unit.
#' @inheritParams assign
#' @inheritParams set_units
#' @family user-facing functions
#' @examples
#' design() %>%
#'   set_units(block = 10,
#'             plot = nested_in(block, 3)) %>%
#'   set_trts(treat = c("A", "B", "C"),
#'            pest = c("a", "b")) %>%
#'   allot_trts(treat ~ plot,
#'               pest ~ block)
#'
#' @return Return an edibble design.
#' @seealso assign
#' @export
allot_trts <- function(.edibble, ..., .record = TRUE) {

  not_edibble(.edibble)
  if(.record) record_step()

  des <- edbl_design(.edibble)

  dots <- list2(...)
  if(!is_null(des$allotment)) {
    des$allotment$trts <- c(des$allotment$trts, dots)
  } else {
    des$allotment <- list(trts = dots, units = NULL)
  }
  prep <- cook_design(des)

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

  if(is_edibble_design(.edibble)) {
    prep$design
  } else if(is_edibble_table(.edibble)) {
    if(length(trts)==0) {
      trts <- prep$trt_names
    }
    for(atrt in trts) {
      prep$append_lvl_edges(data.frame(from = prep$lvl_id(as.character(.edibble[[atrt]])),
                                       to = prep$lvl_id(as.character(.edibble[[unit]]))))
    }
    attr(.edibble, "design") <- prep$design
    .edibble
  }
}



#' Define allotment of units to nested units
#'
#' @description
#' This function adds the edges between factor nodes to describe the
#' high-level relationship between factors.
#' This function does not actually assign edges between level nodes.
#'
#' @param ... A two-sided formula.
#' @inheritParams assign
#' @inheritParams set_units
#' @family user-facing functions
#' @examples
#' design() %>%
#'   set_units(block = 10,
#'             plot = 20) %>%
#'   allot_units(block ~ plot)
#'
#' @return Return an edibble design.
#' @seealso assign
#' @export
allot_units <- function(.edibble, ..., .record = TRUE) {
  not_edibble(.edibble)
  if(.record) record_step()
  des <- edbl_design(.edibble)

  dots <- list2(...)
  if(!is_null(des$allotment)) {
    des$allotment$units <- c(des$allotment$units, dots)
  } else {
    des$allotment <- list(trts = NULL, units = dots)
  }
  prep <- cook_design(des)

  for(ialloc in seq_along(dots)) {
    # there should be only one unit for `big`
    big <- all.vars(f_lhs(dots[[ialloc]]))
    small <- all.vars(f_rhs(dots[[ialloc]]))
    op <- as.character(as.list(f_rhs(dots[[ialloc]]))[[1]])
    prep$fct_exists(name = small, class = "edbl_unit")
    big_id <- prep$fct_id(big)
    prep$fct_exists(name = big, class = "edbl_unit")
    small_id <- prep$fct_id(small)

    if(!op %in% c("crossed_by", "nested_in")) {
      prep$append_fct_edges(data.frame(from = big_id,
                                       to = small_id[length(small_id)],
                                       type = "nest"))
      if(length(small) > 1) {
        prep$append_fct_edges(data.frame(from =  big_id,
                                         to = small_id[length(small_id) - 1],
                                         type = "depends"))
      }

    }
  }
  if(is_edibble_design(.edibble)) {
    prep$design
  } else if(is_edibble_table(.edibble)) {
    # Note: for crossed and nested, it's the opposite -> small = big, not big = small.
    if(op %in% c("crossed_by", "nested_in")) {
      for(ismall in seq_along(small_id)) {
        prep$append_fct_edges(data.frame(from = small_id[ismall],
                                         to = big_id,
                                         type = "nest"))
        if(op == "crossed_by") {
          cross_df <- expand.grid(from = small_id, to = small_id)
          cross_df <- cross_df[cross_df$from!=cross_df$to,]
          cross_df$type <- "cross"
          prep$append_fct_edges(cross_df)
        }
        prep$append_lvl_edges(data.frame(from = prep$lvl_id(as.character(.edibble[[small[ismall]]])),
                                         to = prep$lvl_id(as.character(.edibble[[big]]))))

      }

    } else {
      for(asmall in small) {
        prep$append_lvl_edges(data.frame(from = prep$lvl_id(as.character(.edibble[[big]])),
                                         to = prep$lvl_id(as.character(.edibble[[asmall]]))))
      }
    }
    attr(.edibble, "design") <- prep$design
    .edibble
  }

}


#' Allot treatments to units and serve table
#'
#' This function is a short hand that combines `allot_trts()`, `assign_trts()`
#'  and `serve_table()`.
#'
#' @export
allot_table <- function(.edibble, ..., order = "random", seed = NULL, constrain = nesting_structure(.edibble)) {

  .edibble %>%
    allot_trts(...) %>%
    assign_trts(order = order, seed = seed, constrain = constrain) %>%
    serve_table()
}
