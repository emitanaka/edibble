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
  des <- edbl_design(.edibble)
  prov <- activate_provenance(des)
  if(.record) prov$record_step()

  dots <- list2(...)
  if(!is_null(des$allotment)) {
    des$allotment$trts <- c(des$allotment$trts, dots)
  } else {
    des$allotment <- list(trts = dots, units = NULL)
  }


  for(ialloc in seq_along(dots)) {
    trts <- all.vars(f_lhs(dots[[ialloc]]))
    # there should be only one unit
    unit <- all.vars(f_rhs(dots[[ialloc]]))
    prov$fct_exists(name = unit, role = "edbl_unit")
    uid <- prov$fct_id(name = unit)
    if(length(trts)) {
      prov$trt_exists(name = trts)
      tids <- prov$fct_id(name = trts)
    } else {
      prov$trt_exists()
      tids <- prov$trt_ids
    }

    prov$append_fct_edges(from = tids, to = uid, group = ialloc, type = "allot")
  }

  des$graph <- prov$get_graph()

  if(is_edibble_table(.edibble)) {
    if(length(trts)==0) {
      trts <- prov$trt_names()
    }
    for(atrt in trts) {
      prov$append_lvl_edges(from = prov$lvl_id(name = as.character(.edibble[[atrt]])),
                            to = prov$lvl_id(name = as.character(.edibble[[unit]])))
    }
    attr(.edibble, "design") <- des
    .edibble
  } else {
    des
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
  prov <- activate_provenance(des)

  for(ialloc in seq_along(dots)) {
    # there should be only one unit for `big`
    big <- all.vars(f_lhs(dots[[ialloc]]))
    small <- all.vars(f_rhs(dots[[ialloc]]))
    op <- as.character(as.list(f_rhs(dots[[ialloc]]))[[1]])
    prov$fct_exists(name = small, role = "edbl_unit")
    big_id <- prov$fct_id(big)
    prov$fct_exists(name = big, role = "edbl_unit")
    small_id <- prov$fct_id(small)

    if(!op %in% c("crossed_by", "nested_in")) {
      prov$append_fct_edges(data.frame(from = big_id,
                                       to = small_id[length(small_id)],
                                       type = "nest"))
      if(length(small) > 1) {
        prov$append_fct_edges(data.frame(from =  big_id,
                                         to = small_id[length(small_id) - 1],
                                         type = "depends"))
      }

    }
  }
  if(is_edibble_design(.edibble)) {
    prov$design
  } else if(is_edibble_table(.edibble)) {
    # Note: for crossed and nested, it's the opposite -> small = big, not big = small.
    if(op %in% c("crossed_by", "nested_in")) {
      for(ismall in seq_along(small_id)) {
        prov$append_fct_edges(data.frame(from = small_id[ismall],
                                         to = big_id,
                                         type = "nest"))
        if(op == "crossed_by") {
          cross_df <- expand.grid(from = small_id, to = small_id)
          cross_df <- cross_df[cross_df$from!=cross_df$to,]
          cross_df$type <- "cross"
          prov$append_fct_edges(cross_df)
        }
        prov$append_lvl_edges(data.frame(from = prov$lvl_id(as.character(.edibble[[small[ismall]]])),
                                         to = prov$lvl_id(as.character(.edibble[[big]]))))

      }

    } else {
      for(asmall in small) {
        prov$append_lvl_edges(data.frame(from = prov$lvl_id(as.character(.edibble[[big]])),
                                         to = prov$lvl_id(as.character(.edibble[[asmall]]))))
      }
    }
    attr(.edibble, "design") <- prov$design
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
