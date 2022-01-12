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
                     .name_repair = c("check_unique", "unique", "universal", "minimal")) {
  set_vars(.design, ..., .name_repair = .name_repair, .class = "edbl_trt")
}


#' Define the possible allocation of treatments to units
#'
#' @description
#' This function adds the edges between variable nodes to
#' specify the mapping of units to treatment. This function
#' does not actually assign specific treatment levels onto actual units.
#'
#' @param .data An `edbl_graph` object.
#' @param ... One-sided or two-sided formula. If the input is a one-sided formula
#' then the whole treatment is applied to the specified unit.
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
allot_trts <- function(design, ...) {

  not_edibble(design)

  dots <- list2(...)
  design$allotment <- dots
  for(ialloc in seq_along(dots)) {
    trts <- all.vars(f_lhs(dots[[ialloc]]))
    # there should be only one unit
    unit <- all.vars(f_rhs(dots[[ialloc]]))
    check_var_exists(design, unit, "edbl_unit")
    uid <- fct_id(design, unit)
    if(length(trts)) {
      check_var_exists(design, trts, "edbl_trt")
      tids <- fct_id(design, trts)
    } else {
      check_trt_exists(design)
      classes <- fct_class(design)
      tids <- trt_ids(design)
    }

    design$graph$edges <- add_row(design$graph$edges,
                                  from = tids, to = uid, alloc = ialloc)
  }
  design
}

#' @export
allocate_trts <- function(design, ...) {
  warn("`allocate_trts` is deprecated. Please use `allot_trts` instead.")
  allot_trts(design, ...)
}

#' Assign treatments to units
#'
#' This function assigns specific treatment levels to actual units.
#'
#' @param design An edibble design which should have units, treatments and allotment defined.
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
#'
#' @export
assign_trts <- function(design, order = "random", seed = NULL, constrain = nesting(design)) {

  not_edibble(design)

  set.seed(seed)
  design %@% "seed" <- seed
  design %@% "seeds" <- .Random.seed

  for(ialloc in seq_along(design$allotment)) {
    trts <- all.vars(f_lhs(design$allotment[[ialloc]]))
    # there should be only one unit
    unit <- all.vars(f_rhs(design$allotment[[ialloc]]))
    uid <- fct_id(design, unit)
    if(length(trts)) {
      tids <- fct_id(design, trts)
    } else {
      classes <-fct_class(design)
      tids <- trt_ids(design)
    }

    luids <- lvl_nodes_filter(design, idvar == uid)$id
    tdf <- lvl_nodes_filter(design, idvar %in% tids)
    tidf <- expand.grid(split(tdf$id, fct_label(design, tdf$idvar)))
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
                              vanc <- fct_ancestor(design, id = uid)
                              vanc <- vanc[vanc %in% unit_ids(design)]
                              udf <- as.data.frame(serve_units(select_units(design, !!fct_label(design, vanc))))
                              gparent <- fct_label(design, vanc[2])
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
                          },
                          abort(paste("The", order, "`order` is not implemented.")))

    tout <- tidf[permutation, , drop = FALSE]


    for(itvar in seq_along(tout)) {
      design$graph$levels$edges <- add_row(design$graph$levels$edges,
                                     from = tout[[itvar]],
                                     to = luids, alloc = ialloc)
    }
  }

  design$assignment <- order

  design
}

#' @export
randomise_trts <- function(design, ...) {
  warn("`randomise_trts` is deprecated. Please use `assign_trts(\"random\")` instead.")
  assign_trts(design, order = "random", ...)
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
