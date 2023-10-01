
order_trts.blockdesign <- function(x, trts_df, units_df, unit, vparents, prov, ...) {
  if(!requireNamespace("blocksdesign")) abort("You need to install `blocksdesign` package.")
  udf <- units_df[, setdiff(names(units_df), as.character(unit)), drop = FALSE]
  names(trts_df) <- paste0("T", 1:ncol(trts_df))
  tlist <- lapply(trts_df, as.factor)
  udf <- data.frame(lapply(udf, as.factor))
  res <- blocksdesign::design(treatments = tlist, blocks = udf, treatments_model = model,
                              weighting = 0)
  tdf$..trtid.. <- 1:nrow(tdf)
  out <- res$Design
  out$..id.. <- 1:nrow(out)
  out <- merge(out, tdf)
  out[order(out$..id..), "..trtid..", drop = TRUE]
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


permute_parent_more_than_one <- function(xparents, udf, ntrts, nparents = NULL) {
  if(is_null(nparents) | length(nparents) == 0) {
    # no nested parents
    vlevs <- lapply(udf[as.character(xparents)], unique)
    lvls <- lengths(vlevs[as.character(xparents)])
    oa <- latin_array(dim = lvls, ntrts)
    index <- lapply(xparents, function(id) match(udf[[as.character(id)]], vlevs[[as.character(id)]]))
    map_int(seq(nrow(udf)), function(i) do.call("[", c(list(oa), lapply(index, function(x) x[i]))))
  } else {
    split_by_nested <- split(udf[as.character(c(xparents, nparents))], udf[as.character(nparents)])
    results_by_nested <- list()
    for(isplit in seq_along(split_by_nested)) {
      audf <- split_by_nested[[isplit]]
      audf$alloc <- permute_parent_more_than_one(xparents, audf[as.character(xparents)], ntrts)
      results_by_nested[[isplit]] <- audf
    }
    alloc_table <- do.call(rbind, results_by_nested)
    udf[["...order..."]] <- 1:nrow(udf)
    merged_alloc <- tibble::as_tibble(merge(udf, alloc_table))
    merged_alloc[merged_alloc[["...order..."]], ]$alloc
  }
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

