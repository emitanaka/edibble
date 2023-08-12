#' Assign treatments or units to units
#'
#' This function assigns specific treatment or unit levels to actual units.
#'
#' @param .design An edibble design which should have units, treatments and allotment defined.
#' @param order A character vector signifying the apportion of treatments to units.
#' The value should be either "random", "systematic", "systematic-random" or a class name corresponding to the algorithm for order_trts().
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
#' @name assign
#' @examples
#' # 10 subject, 2 vaccine treatments
#' design() %>%
#'   set_units(subject = 10) %>%
#'   set_trts(vaccine = 2) %>%
#'   allot_trts(vaccine ~ subject) %>%
#'   assign_trts() %>%
#'   serve_table()
#'
#' # 20 subjects, 2 blocks, assign subjects to blocks
#' design() %>%
#'   set_units(subject = 20,
#'             block = 2) %>%
#'   allot_units(block ~ subject) %>%
#'   assign_units() %>%
#'   serve_table()
#' @return An edibble design.
#' @export
assign_trts <- function(.design, order = "random", seed = NULL, constrain = nesting_structure(.design), ..., .record = TRUE) {
  not_edibble(.design)

  prov <- activate_provenance(.design)
  if(.record) prov$record_step()

  prov$save_seed(seed)

  fedges <- prov$fct_edges
  allotments <- fedges[fedges$type == "allot", ]
  alloc_groups <- unique(allotments$group)
  order <- rep(order, length.out = length(alloc_groups))
  for(igroup in alloc_groups) {
    trts_id <- allotments[allotments$group == igroup, ]$from
    # there should be only one unit
    unit_id <- unique(allotments[allotments$group == igroup, ]$to)
    unit_nm <- prov$fct_names(id = unit_id)
    lnodes <- prov$lvl_nodes
    unit_level_ids <- lnodes[[unit_id]]$id
    nunits <- length(unit_level_ids)

    trts_list <- prov$fct_levels(id = trts_id, return = "id")
    # only allows for factorial treatment structure for now
    trts_df <- expand.grid(trts_list, stringsAsFactors = FALSE)
    ntrts <- nrow(trts_df)
    permutation <- switch(order[igroup],
                          "systematic" = rep(1:ntrts, length.out = nunits),
                          "systematic-random" = rep(sample(ntrts), length.out = nunits),
                          "random" = {
                            if(is_empty(constrain[[unit_nm]])) {
                              out <- as.vector(replicate(ceiling(nunits / ntrts),
                                                         sample(ntrts)))
                              sample(out[1:nunits])
                            } else {

                              # FIXME the ancestor should be found
                              # based on `constrain`

                              # find the grandest ancestor
                              vanc <- prov$fct_id_ancestor(id = unit_id, role = "edbl_unit")
                              units_df <- tibble::as_tibble(prov$serve_units(id = vanc))
                              vparents <- prov$fct_id_parent(id = unit_id, role = "edbl_unit")
                              if(length(vparents)==1L) {
                                permute_parent_one_alg(prov, vparents, units_df, ntrts)
                              } else {
                                permute_parent_more_than_one(prov, vparents, units_df, ntrts)
                              }
                            }
                          },
                          order_trts(structure(order, class = order), prov, constrain, trts_df, ...))

    trts_full_df <- trts_df[permutation, , drop = FALSE]

    for(itvar in seq_along(trts_full_df)) {
      prov$append_lvl_edges(from = trts_full_df[[itvar]],
                            to = unit_level_ids)
    }
  }

  .design$graph <- prov$get_graph()
  .design$assignment <- order
  .design
}


#' @rdname assign
#' @export
assign_units <- function(.design, order = "random", seed = NULL, constrain = nesting_structure(.design), ..., .record = TRUE) {
  not_edibble(.design)

  if(.record) record_step()

  save_seed(seed)
  prov <- activate_provenance(.design)

  for(ialloc in seq_along(.design$allotment$units)) {
    lhs <- all.vars(f_lhs(.design$allotment$units[[ialloc]]))
    rhs <- all.vars(f_rhs(.design$allotment$units[[ialloc]]))

    lnodes <- prov$lvl_nodes

    lhs_id <- lnodes[[prov$fct_id(name = lhs)]]$id
    udf <- as.data.frame(serve_units(select_units(prov, rhs)))
    udf <- udf[rhs]
    small_df <- data.frame(lhs = lhs_id)
    permutation <- switch(order,
                          "systematic" = rep(1:nrow(small_df), length.out = nrow(udf)),
                          "systematic-random" = rep(sample(nrow(small_df)), length.out = nrow(udf)),
                          "random" = {

                              # FIXME the ancestor should be found
                              # based on `constrain`??
                              vparents <- prov$fct_id_by_name(rhs[-length(rhs)])

                              if(length(rhs)==1L) {
                                out <- as.vector(replicate(ceiling(nrow(udf)/nrow(small_df)),
                                                        sample(nrow(small_df))))
                                out[1:nrow(udf)]
                              } else if(length(rhs)==2L) {
                                permute_parent_one_alg(prov, vparents, udf, nrow(small_df))
                              } else {
                                permute_parent_more_than_one(prov, vparents, udf, nrow(small_df))
                              }
                          }, abort("not implemented yet"))

    tout <- small_df[permutation, , drop = FALSE]

    browser()
    for(itvar in seq_along(tout)) {
      prov$append_lvl_edges(data.frame(from = tout[[itvar]],
                                       to = prov$lvl_id(udf[[rhs[length(rhs)]]]),
                                       alloc = ialloc))
    }

  }

  prov$design
}
