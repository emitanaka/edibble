#' Assign treatments to units
#'
#' This function assigns specific treatment levels to actual units.
#'
#' @param .design An edibble design which should have units, treatments and allotment defined.
#' @param order A character vector signifying the apportion of treatments to units.
#' The value should be either "random", "systematic" or "systematic-random".
#' "random" allocates the treatment randomly to units based on specified allotment with restrictions
#' implied by unit structure.
#' "systematic" allocates the treatment in a systematic order to units.
#' "systematic-random" allocates the treatment in a systematic order to units but
#' where it is not possible to divide treatments equally (as the number of units are not divisible
#' by the number of levels of the treatment factor), then the extras are chosen randomly.
#' @param seed A scalar value used to set the seed so that the result is reproducible.
#' @param constrain The nesting structure for units.
#' @param ... Arguments parsed into `order_trts` functions.
#' @param .record Whether to record the step.
#'
#'
#' @export
assign_trts <- function(.design, order = "random", seed = NULL, constrain = nesting_structure(.design), ..., .record = TRUE) {
  not_edibble(.design)

  if(.record) record_step()

  save_seed(seed)
  prep <- cook_design(.design)

  for(ialloc in seq_along(.design$allotment$trts)) {
    trts <- all.vars(f_lhs(.design$allotment$trts[[ialloc]]))
    # there should be only one unit
    unit <- all.vars(f_rhs(.design$allotment$trts[[ialloc]]))
    uid <- prep$fct_id(unit)
    if(length(trts)) {
      tids <- prep$fct_id(trts)
    } else {
      classes <- prep$fct_class()
      tids <- prep$trt_ids
    }

    lnodes <- prep$lvl_nodes
    luids <- lnodes[lnodes$idvar == uid, "id"]
    tdf <- lnodes[lnodes$idvar %in% tids, ]
    tidf <- expand.grid(split(tdf$name, prep$fct_names(tdf$idvar)), stringsAsFactors = FALSE)
    ntrts <- nrow(tidf)
    permutation <- switch(order,
                          "systematic" = rep(1:nrow(tidf), length.out = length(luids)),
                          "systematic-random" = rep(sample(nrow(tidf)), length.out = length(luids)),
                          "random" = {
                            if(is_empty(constrain[[unit]])) {
                              out <- as.vector(replicate(ceiling(length(luids) / nrow(tidf)),
                                                          sample(nrow(tidf))))
                              out[1:length(luids)]
                            } else {

                              # FIXME the ancestor should be found
                              # based on `constrain`

                              # find the grandest ancestor
                              vanc <- prep$fct_ancestor(id = uid)
                              vanc <- vanc[vanc %in% prep$unit_ids]
                              udf <- as.data.frame(serve_units(select_units(prep, !!prep$fct_names(vanc))))

                              vparents <- prep$fct_parent(id = uid)
                              vparents <- vparents[vparents %in% prep$unit_ids]
                              vparents <- setdiff(vparents, uid)

                              if(length(vparents)==1L) {
                                permute_parent_one_alg(prep, vparents, udf, ntrts)
                              } else {
                                permute_parent_more_than_one(prep, vparents, udf, ntrts)
                              }
                            }
                          },
                          order_trts(structure(order, class = order), prep, constrain, tidf, ...))

    tout <- tidf[permutation, , drop = FALSE]


    for(itvar in seq_along(tout)) {
      prep$lvl_edges <- prep$append_lvl_edges(data.frame(from = prep$lvl_id(tout[[itvar]]),
                                                         to = luids,
                                                         alloc = ialloc))
    }
  }

  prep$design$assignment <- order

  prep$design
}


#' @export
assign_units <- function(.design, order = "random", seed = NULL, constrain = nesting_structure(.design), ..., .record = TRUE) {
  not_edibble(.design)

  if(.record) record_step()

  save_seed(seed)
  prep <- cook_design(.design)

  for(ialloc in seq_along(.design$allotment$units)) {
    lhs <- all.vars(f_lhs(.design$allotment$units[[ialloc]]))
    rhs <- all.vars(f_rhs(.design$allotment$units[[ialloc]]))

    lnodes <- prep$lvl_nodes
    lhs_id <- lnodes[lnodes$idvar == prep$fct_id(lhs), "id"]
    udf <- as.data.frame(serve_units(select_units(prep, !!rhs)))
    udf <- udf[rhs]
    small_df <- data.frame(lhs = lhs_id)
    permutation <- switch(order,
                          "systematic" = rep(1:nrow(small_df), length.out = nrow(udf)),
                          "systematic-random" = rep(sample(nrow(small_df)), length.out = nrow(udf)),
                          "random" = {

                              # FIXME the ancestor should be found
                              # based on `constrain`??
                              vparents <- prep$fct_id(rhs[-length(rhs)])

                              if(length(rhs)==1L) {
                                out <- as.vector(replicate(ceiling(nrow(udf)/nrow(small_df)),
                                                        sample(nrow(small_df))))
                                out[1:nrow(udf)]
                              } else if(length(rhs)==2L) {
                                permute_parent_one_alg(prep, vparents, udf, nrow(small_df))
                              } else {
                                permute_parent_more_than_one(prep, vparents, udf, nrow(small_df))
                              }
                          }, abort("not implemented yet"))

    tout <- small_df[permutation, , drop = FALSE]

    for(itvar in seq_along(tout)) {
      prep$append_lvl_edges(data.frame(from = tout[[itvar]],
                                       to = prep$lvl_id(udf[[rhs[length(rhs)]]]),
                                       alloc = ialloc))
    }

  }

  prep$design
}
