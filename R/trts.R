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
#' @export
set_trts <- function(.design, ...,
                     .name_repair = c("check_unique", "unique", "universal", "minimal"),
                     .record = TRUE) {
  if(.record) record_step()
  set_vars(.design, ..., .name_repair = .name_repair, .class = "edbl_trt")
}



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
                              sample(rep(sample(nrow(tidf)), length.out = length(luids)))
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
      prep$lvl_edges <- add_row(prep$lvl_edges,
                                     from = prep$lvl_id(tout[[itvar]]),
                                     to = luids, alloc = ialloc)
    }
  }

  prep$design$assignment <- order

  prep$design
}

order_trts <- function(x, ...) {
  UseMethod("order_trts")
}

order_trts.default <- function(order, prep, constrain, ...) {
  abort(paste("The", order, "`order` is not implemented."))
}

order_trts.dae <- function(order, prep, constrain, trts, ...) {
  dat <- assign_trts(prep$design, order = "systematic", constrain = constrain, .record = FALSE) %>%
    serve_table(use_labels = TRUE) %>%
    lapply(as.factor) %>%
    as.data.frame()
  out <- dae::designRandomize(allocated = dat[prep$trt_names],
                       recipient = dat[prep$unit_names],
                       nested.recipients = constrain)
  trtsv <- setNames(1:nrow(trts), do.call(paste0, trts[prep$trt_names]))
  otrtsv <- do.call(paste0, dat[prep$trt_names])
  unname(trtsv[otrtsv])
}

permute_parent_more_than_one <- function(prep, vids, udf, ntrts) {
  gparents <- prep$fct_names(vids)
  vlevs <- prep$fct_levels()

  lvls <- lengths(vlevs[gparents])
  oa <- latin_array(dim = lvls, ntrts)

  index <- lapply(gparents, function(gparent) match(udf[[gparent]], vlevs[[gparent]]))

  out <- vector("integer", length = nrow(udf))
  for(i in seq(nrow(udf))) {
    out[i] <- do.call("[", c(list(oa), lapply(index, function(x) x[i])))
  }

  out

}



permute_parent_one_alg <- function(prep, vid, udf, ntrts) {
  gparent <- prep$fct_names(vid)
  blocksizes <- table(udf[[gparent]])
  # if(min(blocksizes) > ntrts) {
  #   permute_parent_one(.design, vid, udf, ntrts)
  # } else {
  # AlgDesign::optBlock takes really long when at least one block size > ntrts
  # in this case subtract total trts
  blocksizes_adj <- blocksizes
  blocksizes_adj[blocksizes > ntrts] <- blocksizes_adj[blocksizes > ntrts] %% ntrts
  capture.output({
      # prevent "No improvement over initial random design" print out from AlgDesign
      # where the design is balanced
    res <- tryCatch({
        AlgDesign::optBlock(~.,
                                   withinData = data.frame(tindex = factor(1:ntrts)),
                                   blocksizes = blocksizes_adj)
      }, error = function(x) {
        return(permute_parent_one(prep, vid, udf, ntrts))
      })
  })
  if(is.integer(res)) return(res)
  reps <- ceiling(blocksizes / ntrts) - 1L
  unname(unlist(lapply(seq_along(blocksizes), function(i) {
    blocksizes
    sample(c(as.integer(res$Blocks[[i]]$tindex), rep(1:ntrts, reps[i])))
  })))
  # }
}


permute_parent_one <- function(prep, vid, udf, ntrts) {
  gparent <- prep$fct_names(vid)
  blocksizes <- as.data.frame(table(table(udf[[gparent]])))
  blocksizes$size <- as.numeric(as.character(blocksizes$Var1))
  for(isize in seq(nrow(blocksizes))) {
    if(blocksizes$size[isize] <= ntrts) {
      comb <- combn(ntrts, blocksizes$size[isize])
      blocksizes$rows[isize] <- list(comb)
    } else {
      nrep <- floor(blocksizes$size[isize] / ntrts)
      nremain <- blocksizes$size[isize] %% ntrts
      comb <- combn(ntrts, nremain)
      blocksizes$rows[isize] <- list(rbind(comb,
                                           matrix(rep(1:ntrts, nrep * ncol(comb)), ncol = ncol(comb))))
    }
    blocksizes$select[isize] <- list(sample(rep(sample(ncol(comb)), length.out = blocksizes$Freq[isize])))
  }
  blocksizes$wselect <- blocksizes$select
  gpar_tab <- as.data.frame(table(udf[[gparent]]))
  out <- vector("integer", length = nrow(udf))
  for(ianc in seq(nrow(gpar_tab))) {
    imatch <- which(blocksizes$size == gpar_tab$Freq[ianc])
    iselect <- blocksizes$wselect[imatch][[1]][1]
    blocksizes$wselect[imatch] <- list(blocksizes$wselect[imatch][[1]][-1])
    out[as.character(udf[[gparent]])==as.character(gpar_tab$Var1[ianc])] <- sample(blocksizes$rows[imatch][[1]][,iselect])
  }
  out
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


# TODO
# add_trts()
