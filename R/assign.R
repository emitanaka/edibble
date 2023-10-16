#' Assign treatments or units to units
#'
#' This function assigns specific treatment or unit levels to actual units.
#'
#' @param .edibble An edibble design which should have units, treatments and allotment defined.
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
#' @name assign_fcts
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
assign_trts <- function(.edibble, order = "random", seed = NULL, constrain = nesting_structure(.edibble), ..., .record = TRUE) {
  not_edibble(.edibble)
  force(constrain) # evaluate this now rather than later

  prov <- activate_provenance(.edibble)
  if(.record) prov$record_step()
  prov$save_seed(seed, type = "assign_trts")
  fedges <- prov$fct_edges
  allotments <- fedges[fedges$type == "allot", ]
  alloc_groups <- unique(allotments$group)
  order <- rep(order, length.out = length(alloc_groups))

  for(igroup in seq_along(alloc_groups)) {
    trts_id <- allotments[allotments$group == alloc_groups[igroup], ]$from
    # there should be only one unit
    unit_id <- unique(allotments[allotments$group == alloc_groups[igroup], ]$to)
    unit_nm <- prov$fct_names(id = unit_id)
    lnodes <- prov$lvl_nodes
    unit_level_ids <- lnodes[[as.character(unit_id)]]$id
    nunits <- length(unit_level_ids)

    parent_trts <- prov$fct_id_parent(id = trts_id, role = "edbl_trt")
    parent_trts_not_in_this_allotment <- setdiff(parent_trts, trts_id)
    trts_df <- prov$make_trts_table(id = c(parent_trts, trts_id), return = "id")
    if(length(parent_trts_not_in_this_allotment)) {
      trts_df_with_id <- trts_df
      trts_df_with_id$..id.. <- 1:nrow(trts_df)
      vanc <- prov$fct_id_ancestor(id = unit_id, role = "edbl_unit")
      units_df_with_id <- units_df <- tibble::as_tibble(prov$serve_units(id = vanc))
      units_df_with_id$..id.. <- 1:nrow(units_df_with_id)
      ptrts_df <- na.omit(tibble::as_tibble(prov$serve_trts(id = parent_trts)))

      if(nrow(ptrts_df) == 0L) abort(paste0("The treatment factor, ",
                                            .combine_words(prov$fct_names(id = trts_id), fun = cli::col_blue),
                                            " is not assigned yet."))
      ptrts_df$..group_id.. <- as.numeric(factor(do.call(paste0, ptrts_df)))
      permutation <- integer(length = nrow(units_df_with_id))
      for(aptrt in unique(ptrts_df$..group_id..)) {
        locs <- which(ptrts_df$..group_id.. == aptrt)
        sub_units_df <- units_df[locs, ]
        sub_trts_df_with_id <- tibble::as_tibble(merge(trts_df_with_id, ptrts_df[locs, ][1,]))
        sub_trts_df <- sub_trts_df_with_id[, setdiff(names(sub_trts_df_with_id), c("..id..", "..group_id.."))]
        sub_ntrts <- nrow(sub_trts_df)
        sub_nunits <- nrow(sub_units_df)
        permute <- switch(order[igroup],
                                   "systematic" = ,
                                   "systematic-fastest" = rep(1:sub_ntrts, length.out = sub_nunits),
                                   "systematic-slowest" = sort(rep(1:sub_ntrts, length.out = sub_nunits)),
                                   "systematic-random" = ,
                                   "systematic-random-fastest" = rep(sample(sub_ntrts), length.out = sub_nunits),
                                   "systematic-random-slowest" = sort(rep(sample(sub_ntrts), length.out = sub_nunits)),
                                   "random" = {
                                     if(is_empty(constrain[[unit_nm]])) {
                                       sample(rep(sample(sub_ntrts), length.out = sub_nunits))
                                     } else {
                                       vparents <- prov$fct_id(name = constrain[[unit_nm]])
                                       if(length(vparents)==1L) {
                                         permute_parent_one_alg(vparents, sub_units_df, sub_ntrts)
                                       } else {
                                         vnparents <- prov$fct_id_parent(id = unit_id, role = "edbl_unit", type = "nest")
                                         permute_parent_more_than_one(setdiff(vparents, vnparents), sub_units_df, sub_ntrts, nparents = vnparents)
                                       }
                                     }
                                   },
                                   {
                                     vparents <- prov$fct_id(name = constrain[[unit_nm]])
                                     order_trts(structure(order[igroup], class = order[igroup]), sub_trts_df, sub_units_df, setNames(unit_id, unit_nm), vparents, prov, ...)
                                   })
        permutation[units_df_with_id$..id..[locs]] <- sub_trts_df_with_id[permute, "..id..", drop = TRUE]
      }
    } else {
      ntrts <- nrow(trts_df)
      permutation <- switch(order[igroup],
                            "systematic" = ,
                            "systematic-fastest" = rep(1:ntrts, length.out = nunits),
                            "systematic-slowest" = sort(rep(1:ntrts, length.out = nunits)),
                            "systematic-random" = ,
                            "systematic-random-fastest" = rep(sample(ntrts), length.out = nunits),
                            "systematic-random-slowest" = sort(rep(sample(ntrts), length.out = nunits)),
                            "random" = {
                              if(is_empty(constrain[[unit_nm]])) {
                                sample(rep(sample(ntrts), length.out = nunits))
                              } else {
                                # find the grandest ancestor
                                vanc <- prov$fct_id_ancestor(id = unit_id, role = "edbl_unit")
                                units_df <- tibble::as_tibble(prov$serve_units(id = vanc))
                                vparents <- prov$fct_id(name = constrain[[unit_nm]])
                                if(length(vparents)==1L) {
                                  permute_parent_one_alg(vparents, units_df, ntrts)
                                } else {
                                  vnparents <- prov$fct_id_parent(id = unit_id, role = "edbl_unit", type = "nest")
                                  permute_parent_more_than_one(setdiff(vparents, vnparents), units_df, ntrts, nparents = vnparents)
                                }
                              }
                            },
                            {
                              vanc <- prov$fct_id_ancestor(id = unit_id, role = "edbl_unit")
                              units_df <- tibble::as_tibble(prov$serve_units(id = vanc))
                              vparents <- prov$fct_id(name = constrain[[unit_nm]])
                              order_trts(structure(order[igroup], class = order[igroup]), trts_df, units_df, setNames(unit_id, unit_nm), vparents, prov, ...)
                            })

    }
    trts_full_df <- trts_df[permutation, , drop = FALSE]

    for(itvar in seq_along(trts_full_df)) {
      prov$append_lvl_edges(from = trts_full_df[[itvar]],
                            to = unit_level_ids)
    }
  }

  return_edibble_with_graph(.edibble, prov)

  # .edibble$assignment <- order
}


#' @rdname assign_fcts
#' @export
assign_units <- function(.edibble, order = "random", seed = NULL, constrain = nesting_structure(.edibble), ..., .record = TRUE) {
  not_edibble(.edibble)
  prov <- activate_provenance(.edibble)
  if(.record) prov$record_step()
  prov$save_seed(seed, "assign_units")
  # FIXME: check

  fedges <- prov$fct_edges

  allotments <- fedges[fedges$from %in% prov$unit_ids & fedges$to %in% prov$unit_ids & !is.na(fedges$group), ]
  alloc_groups <- unique(allotments$group)
  order <- rep(order, length.out = length(alloc_groups))

  for(igroup in seq_along(alloc_groups)) {
    parent_id <- allotments[allotments$group == alloc_groups[igroup], ]$from
    # there should be only one unit
    unit_id <- unique(allotments[allotments$group == alloc_groups[igroup], ]$to)
    unit_nm <- prov$fct_names(id = unit_id)
    lnodes <- prov$lvl_nodes
    unit_level_ids <- lnodes[[as.character(unit_id)]]$id
    nunits <- length(unit_level_ids)

    parent_level_ids <- lnodes[[as.character(parent_id)]]$id
    udf <- data.frame(unit = unit_level_ids)
    small_df <- data.frame(lhs = parent_level_ids)
    permutation <- switch(order,
                          "systematic" = ,
                          "systematic-fastest" = rep(1:nrow(small_df), length.out = nrow(udf)),
                          "systematic-slowest" = sort(rep(1:nrow(small_df), length.out = nrow(udf))),
                          "systematic-random" = ,
                          "systematic-random-fastest" = rep(sample(nrow(small_df)), length.out = nrow(udf)),
                          "systematic-random-slowest" = sort(rep(sample(nrow(small_df)), length.out = nrow(udf))),
                          "random" = {
                            vparents <- prov$fct_id(name = constrain[[unit_nm]])
                            if(length(vparents)==1L) {
                              out <- as.vector(replicate(ceiling(nrow(udf)/nrow(small_df)),
                                                         sample(nrow(small_df))))
                              out[1:nrow(udf)]
                            } else if(length(vparents)==2L) {
                              permute_parent_one_alg(vparents, udf, nrow(small_df))
                            } else {
                              permute_parent_more_than_one(vparents, udf, nrow(small_df))
                            }
                          }, abort("not implemented yet"))

    tout <- small_df[permutation, , drop = FALSE]

    for(itvar in seq_along(tout)) {
      prov$append_lvl_edges(from = tout[[itvar]],
                            to = udf$unit)
    }
  }
  return_edibble_with_graph(.edibble, prov)
}
