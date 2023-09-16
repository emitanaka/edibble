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
#' design() %>%
#'   set_trts(pesticide = c("A", "B", "C"),
#'            dosage = c(0, 10, 20, 30, 40))
#'
#' @return An edibble design.
#' @export
set_trts <- function(.edibble = design(), ...,
                     .name_repair = c("check_unique", "unique", "universal", "minimal"),
                     .record = TRUE) {
  prov <- activate_provenance(.edibble)
  if(.record) prov$record_step()
  set_fcts(.edibble, ..., .name_repair = .name_repair, .class = "edbl_trt")
}



order_trts <- function(x, ...) {
  UseMethod("order_trts")
}

order_trts.default <- function(x, prov, constrain, ...) {
  abort(paste("The", order, "`order` is not implemented."))
}

order_trts.dae <- function(x, prov, constrain, trts, ...) {
  # FIXME
  dat <- assign_trts(prov$design, order = "systematic", constrain = constrain, .record = FALSE) %>%
    serve_table(use_labels = TRUE) %>%
    lapply(as.factor) %>%
    as.data.frame()
  out <- dae::designRandomize(allocated = dat[prov$trt_names],
                       recipient = dat[prov$unit_names],
                       nested.recipients = constrain)
  trtsv <- stats::setNames(1:nrow(trts), do.call(paste0, trts[prov$trt_names]))
  otrtsv <- do.call(paste0, dat[prov$trt_names])
  unname(trtsv[otrtsv])
}

permute_parent_more_than_one <- function(prov, vids, udf, ntrts) {
  vlevs <- prov$fct_levels(return = "id")
  lvls <- lengths(vlevs[as.character(vids)])
  oa <- latin_array(dim = lvls, ntrts)
  index <- lapply(vids, function(id) match(udf[[as.character(id)]], vlevs[[as.character(id)]]))

  out <- vector("integer", length = nrow(udf))
  for(i in seq(nrow(udf))) {
    out[i] <- do.call("[", c(list(oa), lapply(index, function(x) x[i])))
  }
  out
}



permute_parent_one_alg <- function(prov, vid, udf, ntrts) {
  udf$.id <- 1:nrow(udf)
  udf <- udf[order(udf[[as.character(vid)]]),]
  blocksizes <- table(udf[[as.character(vid)]])
  # if(min(blocksizes) > ntrts) {
  #   permute_parent_one(.edibble, vid, udf, ntrts)
  # } else {
  # AlgDesign::optBlock takes really long when at least one block size > ntrts
  # in this case subtract total trts
  blocksizes_adj <- blocksizes
  blocksizes_adj[blocksizes > ntrts] <- blocksizes_adj[blocksizes > ntrts] %% ntrts
  utils::capture.output({
      # prevent "No improvement over initial random design" print out from AlgDesign
      # where the design is balanced
    res <- tryCatch({
        AlgDesign::optBlock(~.,
                                   withinData = data.frame(tindex = factor(1:ntrts)),
                                   blocksizes = blocksizes_adj)
      }, error = function(x) {
        return(permute_parent_one(prov, vid, udf, ntrts))
      })
  })
  if(is.integer(res)) return(res)
  reps <- ceiling(blocksizes / ntrts) - 1L
  out <- unname(unlist(lapply(seq_along(blocksizes), function(i) {
    blocksizes
    xv <- c(as.integer(res$Blocks[[i]]$tindex), rep(1:ntrts, reps[i]))
    if(length(xv)==1) xv else sample(xv)
  })))
  udf$.res <- out
  udf[order(udf$.id), ".res", drop = TRUE]
}


permute_parent_one <- function(prov, vid, udf, ntrts) {
  blocksizes <- as.data.frame(table(table(udf[[as.character(vid)]])))
  blocksizes$size <- as.numeric(as.character(blocksizes$Var1))

  for(isize in seq(nrow(blocksizes))) {
    if(blocksizes$size[isize] <= ntrts) {
      comb <- utils::combn(ntrts, blocksizes$size[isize])
      blocksizes$rows[isize] <- list(comb)
    } else {
      nrep <- floor(blocksizes$size[isize] / ntrts)
      nremain <- blocksizes$size[isize] %% ntrts
      comb <- utils::combn(ntrts, nremain)
      blocksizes$rows[isize] <- list(rbind(comb,
                                           matrix(rep(1:ntrts, nrep * ncol(comb)), ncol = ncol(comb))))
    }
    blocksizes$select[isize] <- list(sample(rep(sample(ncol(comb)), length.out = blocksizes$Freq[isize])))
  }
  blocksizes$wselect <- blocksizes$select
  gpar_tab <- as.data.frame(table(udf[[as.character(vid)]]))
  out <- vector("integer", length = nrow(udf))
  for(ianc in seq(nrow(gpar_tab))) {
    imatch <- which(blocksizes$size == gpar_tab$Freq[ianc])
    iselect <- blocksizes$wselect[imatch][[1]][1]
    blocksizes$wselect[imatch] <- list(blocksizes$wselect[imatch][[1]][-1])
    out[as.character(udf[[as.character(vid)]])==as.character(gpar_tab$Var1[ianc])] <- sample(blocksizes$rows[imatch][[1]][,iselect])
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


#' Treatments table
#'
#' @param .edibble An edibble table
#' @export
trts_table <- function(.edibble) {
  prov <- activate_provenance(.edibble)
  tids <- prov$trt_ids
  prov$make_trts_table(id = tids, return = "value")
}

# TODO
# add_trts()
