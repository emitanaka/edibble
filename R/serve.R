#' Serve edibble table
#' @description
#' This converts an edibble graph object to a data frame called edibble.
#' This function should be used when the design is in the final form
#' (or close to the final form). The table can only be formed when the
#' variables can be reconciled, otherwise it will be a data frame with
#' zero rows.
#'
#' @param .design An edibble design.
#' @param ... Ignored.
#' @return An `edbl` data frame with columns defined by vertices and
#' rows displayed only if the vertices are connected and reconcile for output.
#' @family user-facing functions
#' @export
serve_table <- function(.design, ...) {
  if(!is_connected(.design)) {
    lout <- serve_vars_not_reconciled(.design)
  } else {
    classes <- .design$vgraph$nodes$class
    lunit <- ltrt <- lvar <- list()
    if("edbl_unit" %in% classes) lunit <- serve_units(.design)
    if("edbl_trt" %in% classes) ltrt <- serve_trts(.design, lunit)
    if("edbl_rcrd" %in% classes) lvar <- serve_rcrds(.design, lunit)
    lout <- c(lunit, ltrt, lvar)
  }

  namesv <- .design$vgraph$nodes$label
  new_edibble(lout[namesv], design = .design)
}

serve_trts <- function(design, lunits) {
  tids <- subset(design$vgraph$nodes, class=="edbl_trt")$id
  vnames <- vlabel(design$vgraph, id = tids)
  lvs <- lapply(tids, function(i) {
    serve_trt(design, i, lunits)
  })
  names(lvs) <- vnames
  lvs
}

rcrd_to_unit_dict <- function(design, rids) {
  tdf <- subset(design$vgraph$edges, to %in% rids)
  set_names(vlabel(design$vgraph, tdf$from),
            vlabel(design$vgraph, tdf$to))
}

serve_rcrds <- function(design, lunits) {
  rids <- subset(design$vgraph$nodes, class=="edbl_rcrd")$id
  rcrd2unit <- rcrd_to_unit_dict(design, rids)
  rnames <- vlabel(design$vgraph, id = rids)
  N <- max(lengths(lunits))
  lvs <- lapply(rnames, function(avar) {
    new_edibble_rcrd(N, lunits[[rcrd2unit[avar]]])
  })
  names(lvs) <- rnames
  lvs
}

is_connected <- function(design) {
  nvar <- nrow(design$vgraph$nodes)
  if(nvar==0) return(FALSE)
  if(nvar==1) return(TRUE)
  all(design$lgraph$nodes$id %in% c(design$lgraph$edges$to, design$lgraph$edges$from))
}

# Returns list of edibble variables
serve_vars_not_reconciled <- function(.design) {
  namesv <- .design$vgraph$nodes$label
  res <- lapply(namesv,
                function(avar) {
                  new_edibble_var(levels = vlevels(.design, label = avar)[[avar]],
                                  name = avar,
                                  class = vclass(.design$vgraph,
                                                 id = vid(.design$vgraph, avar)))
                })
  names(res) <- namesv
  res
}

# Return edibble unit
serve_unit_with_child <- function(parent_levels, parent_vname, parent_class,
                                  child_labels, child_vname, design) {
  pids <- vid(design$lgraph, label = parent_levels)
  cids <- vid(design$lgraph, label = unique(child_labels))
  ledges <- subset(design$lgraph$edges, to %in% cids & from %in% pids)
  dict <- set_names(vlabel(design$lgraph, ledges$from), vlabel(design$lgraph, ledges$to))
  new_edibble_var(levels = parent_levels,
                  labels = unname(dict[child_labels]),
                  name = parent_vname,
                  class = parent_class)

}

serve_unit_with_no_child <- function(vlevs, vname, classv) {
  new_edibble_var(levels = vlevs,
                  labels = vlevs,
                  name = vname,
                  class = classv)
}


serve_units <- function(design) {
  uid <- subset(design$vgraph$nodes, class=="edbl_unit")$id
  rid <- subset(design$vgraph$nodes, class=="edbl_rcrd")$id
  lid <- uid[!uid %in% subset(design$vgraph$edges, !to %in% rid)$from]
  wid <- uid
  vlev <- vlevels(design)
  res <- list()
  while(!is_empty(lid)) {
    lvs <- lapply(lid, function(i) {
      vname <- vlabel(design$vgraph, id = i)
      classv <- vclass(design$vgraph, id = i)
      vlevs <- vlev[[vname]]
      cid <- setdiff(vchild(design$vgraph, id = i), rid)
      if(!is_empty(cid) & length(cid)==1) {
        # currently assumes one child only for now
        cname <- vlabel(design$vgraph, id = cid)
        serve_unit_with_child(vlevs, vname, classv,
                              as.character(res[[cname]]), cname, design)
      } else {
        serve_unit_with_no_child(vlevs, vname, classv)
      }
    })
    names(lvs) <- vlabel(design$vgraph, id = lid)
    res <- c(res, lvs)
    wid <- setdiff(wid, lid)
    lid <- wid[!wid %in% subset(design$vgraph$edges, !to %in% c(lid, rid))$from]
  }
  res
}

serve_trts <- function(design, lunits) {
  tids <- subset(design$vgraph$nodes, class=="edbl_trt")$id
  vnames <- vlabel(design$vgraph, id = tids)
  lvs <- lapply(tids, function(i) {
    serve_trt(design, i, lunits)
  })
  names(lvs) <- vnames
  lvs
}

serve_trt <- function(design, tid, lunits) {
  tdf <- subset(design$lgraph$nodes, idvar == tid)
  ltids <- tdf$id
  luids <- vchild(design$lgraph, id = ltids)
  ledges <- subset(design$lgraph$edges, to %in% luids & from %in% ltids)
  aunit <- vlabel(design$vgraph, id = vchild(design$vgraph, id = tid))
  if(!is_empty(aunit)) {
    dict <- set_names(vlabel(design$lgraph, ledges$from), vlabel(design$lgraph, ledges$to))
    labels <- unname(dict[lunits[[aunit]]])
  } else {
    labels <- tdf$label
  }
  new_edibble_var(levels = tdf$label,
                  labels = labels,
                  name = vlabel(design$vgraph, id = tid),
                  class = vclass(design$vgraph, id = tid))
}
