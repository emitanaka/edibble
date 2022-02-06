#' Serve edibble table
#' @description
#' This converts an edibble graph object to a data frame called edibble.
#' This function should be used when the design is in the final form
#' (or close to the final form). The table can only be formed when the
#' variables can be reconciled, otherwise it will be a data frame with
#' zero rows.
#'
#' @inheritParams design-context
#' @return An `edbl` data frame with columns defined by vertices and
#' rows displayed only if the vertices are connected and reconcile for output.
#' @family user-facing functions
#' @export
serve_table <- function(.design, ..., .record = TRUE) {
  if(.record) record_step()

  if(!is_connected(.design)) {
    lout <- serve_vars_not_reconciled(.design)
  } else {
    classes <- fct_class(.design)
    lunit <- ltrt <- lrcrd <- list()
    if("edbl_unit" %in% classes) lunit <- serve_units(.design)
    if("edbl_trt" %in% classes) ltrt <- serve_trts(.design, lunit)
    if(length(lunit) | length(ltrt)) {
      if("edbl_rcrd" %in% classes) lrcrd <- serve_rcrds(.design, lunit)
      lout <- c(lunit, ltrt, lrcrd)
    } else {
      lout <- serve_vars_not_reconciled(.design)
    }
  }

  namesv <- fct_names(.design)
  new_edibble(lout[namesv], design = .design)
}

serve_trts <- function(design, lunits) {
  tids <- trt_ids(design)
  vnames <- fct_names(design, id = tids)
  lvs <- lapply(tids, function(i) {
    serve_trt(design, i, lunits)
  })
  names(lvs) <- vnames
  lvs
}

rcrd_to_unit_dict <- function(design, rids) {
  tdf <- fct_edges_filter(design, to %in% rids)
  set_names(fct_names(design, tdf$from),
            fct_names(design, tdf$to))
}

serve_rcrds <- function(design, lunits) {
  rids <- rcrd_ids(design)
  rcrd2unit <- rcrd_to_unit_dict(design, rids)
  rnames <- fct_names(design, id = rids)
  N <- max(lengths(lunits))
  lvs <- lapply(rnames, function(avar) {
    unit <- rcrd2unit[avar]
    unit_values <- lunits[[unit]]
    new_edibble_rcrd(rep(NA_real_, N), unit, unit_values)
  })
  names(lvs) <- rnames
  lvs
}

is_connected <- function(design) {
  nvar <- fct_n(design) - length(rcrd_ids(design))
  if(nvar==0) return(FALSE)
  if(nvar==1) return(TRUE)
  all(lvl_nodes_pull(design, id) %in% c(lvl_edges_pull(design, to), lvl_edges_pull(design, from)))
}

# Returns list of edibble variables
serve_vars_not_reconciled <- function(.design) {
  namesv <- fct_names(.design)
  res <- lapply(namesv,
                function(avar) {
                  new_edibble_var(levels = fct_levels(.design, name = avar)[[avar]],
                                  name = avar,
                                  class = fct_class(.design,
                                                    id = fct_id(.design, avar)))
                })
  names(res) <- namesv
  res
}

# Return edibble unit
serve_unit_with_child <- function(parent_levels, parent_vname, parent_class,
                                  child_labels, child_vname, design) {
  pids <- lvl_id(design, name = parent_levels)
  cids <- lvl_id(design, name = unique(child_labels))
  ledges <- lvl_edges_filter(design, to %in% cids & from %in% pids)
  dict <- set_names(lvl_names(design, ledges$from), lvl_names(design, ledges$to))
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
  uid <- unit_ids(design)
  lid <- fct_leaves(design)
  if(length(lid) != 1) {
    return(list())
  }
  wid <- uid
  vlev <- fct_levels(design)
  res <- list()
  while(!is_empty(lid)) {
    lvs <- lapply(lid, function(i) {
      vname <- fct_names(design, id = i)
      classv <- fct_class(design, id = i)
      vlevs <- vlev[[vname]]
      cid <- intersect(fct_child(design, id = i), uid)
      if(!is_empty(cid)) {
        # currently uses the first child only
        cname <- fct_names(design, id = cid)
        serve_unit_with_child(vlevs, vname, classv,
                              as.character(res[[cname[1]]]), cname[1], design)
      } else {
        serve_unit_with_no_child(vlevs, vname, classv)
      }
    })
    names(lvs) <- fct_names(design, id = lid)
    res <- c(res, lvs)
    wid <- setdiff(wid, lid)
    wdes <- select_units(design, fct_names(design, wid))
    lid <- fct_leaves(wdes)
  }
  res
}

serve_trts <- function(design, lunits) {
  tids <- trt_ids(design)
  vnames <- fct_names(design, id = tids)
  lvs <- lapply(tids, function(i) {
    serve_trt(design, i, lunits)
  })
  names(lvs) <- vnames
  lvs
}

serve_trt <- function(design, tid, lunits) {
  tdf <- lvl_nodes_filter(design, idvar == tid)
  ltids <- tdf$id
  luids <- lvl_child(design, id = ltids)
  ledges <- lvl_edges_filter(design, to %in% luids & from %in% ltids)
  aunit <- fct_names(design, id = fct_child(design, id = tid))
  if(!is_empty(aunit)) {
    dict <- set_names(lvl_names(design, ledges$from), lvl_names(design, ledges$to))
    labels <- unname(dict[lunits[[aunit]]])
  } else {
    labels <- tdf$label
  }
  new_edibble_var(levels = tdf$label,
                  labels = labels,
                  name = fct_names(design, id = tid),
                  class = fct_class(design, id = tid))
}
