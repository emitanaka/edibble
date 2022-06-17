#' Serve edibble table
#' @description
#' This converts an edibble graph object to a data frame called edibble.
#' This function should be used when the design is in the final form
#' (or close to the final form). The table can only be formed when the
#' variables can be reconciled, otherwise it will be a data frame with
#' zero rows.
#'
#' @inheritParams set_units
#' @param use_labels To show the labels instead of names.
#' @return An `edbl` data frame with columns defined by vertices and
#' rows displayed only if the vertices are connected and reconcile for output.
#' @family user-facing functions
#' @export
serve_table <- function(.edibble, use_labels = FALSE, ..., .record = TRUE) {
  if(.record) record_step()

  prep <- cook_design(.edibble)

  if(!prep$is_connected) {
    lout <- serve_vars_not_reconciled(prep)
  } else {
    classes <- prep$fct_class()
    lunit <- ltrt <- lrcrd <- list()
    if("edbl_unit" %in% classes) lunit <- serve_units(prep)
    if("edbl_trt" %in% classes) ltrt <- serve_trts(prep, lunit)
    if(length(lunit) | length(ltrt)) {
      if("edbl_rcrd" %in% classes) lrcrd <- serve_rcrds(prep, lunit)
      lout <- c(lunit, ltrt, lrcrd)
    } else {
      lout <- serve_vars_not_reconciled(prep)
    }
  }

  namesv <- prep$fct_names()
  if(use_labels) {
    translate <- stats::setNames(prep$lvl_nodes$label, prep$lvl_nodes$name)
    # FIXME: it lsoes the classes when this is done
    lout <- lapply(lout, function(.x) translate[.x])
  }

  new_edibble(lout[namesv], design = .edibble)
}

serve_trts <- function(prep, lunits) {
  tids <- prep$unit_ids()
  vnames <- prep$fct_names(id = tids)
  lvs <- lapply(tids, function(i) {
    serve_trt(prep, i, lunits)
  })

  names(lvs) <- vnames
  lvs
}

rcrd_to_unit_dict <- function(prep, rids) {
  fedges <- prep$fct_edges
  tdf <- fedges[fedges$to %in% rids, ]
  set_names(prep$fct_names(tdf$from),
            prep$fct_names(tdf$to))
}


serve_rcrds <- function(prep, lunits) {
  rids <- prep$rcrd_ids
  rcrd2unit <- rcrd_to_unit_dict(prep, rids)
  rnames <- prep$rcrd_names
  N <- max(lengths(lunits))
  lvs <- lapply(rnames, function(avar) {
    unit <- rcrd2unit[avar]
    unit_values <- lunits[[unit]]
    new_edibble_rcrd(rep(NA_real_, N), unit, unit_values)
  })
  names(lvs) <- rnames
  lvs
}



# Returns list of edibble variables
serve_vars_not_reconciled <- function(prep) {
  namesv <- prep$fct_names()
  res <- lapply(namesv,
                function(avar) {
                  new_edibble_fct(levels = prep$fct_levels(name = avar)[[avar]],
                                  name = avar,
                                  class = prep$fct_class(id = prep$fct_id(avar)))
                })
  names(res) <- namesv
  res
}

# Return edibble unit
serve_unit_with_child <- function(parent_levels, parent_vname, parent_class,
                                  child_labels, child_vname, prep) {
  pids <- prep$lvl_id(name = parent_levels)
  cids <- prep$lvl_id(name = unique(child_labels))
  ledges <- prep$lvl_edges
  ledges <- ledges[ledges$to %in% cids & ledges$from %in% pids, ]
  dict <- set_names(prep$lvl_names(ledges$from), prep$lvl_names(ledges$to))
  new_edibble_fct(levels = parent_levels,
                  labels = unname(dict[child_labels]),
                  name = parent_vname,
                  class = parent_class)

}

serve_unit_with_no_child <- function(vlevs, vname, classv) {
  new_edibble_fct(levels = vlevs,
                  labels = vlevs,
                  name = vname,
                  class = classv)
}


serve_units <- function(prep) {
  uid <- prep$unit_ids
  lid <- prep$fct_leaves
  if(length(lid) != 1) {
    return(list())
  }
  wid <- uid
  vlev <- prep$fct_levels()
  res <- list()
  while(!is_empty(lid)) {
    lvs <- lapply(lid, function(i) {
      vname <- prep$fct_names(id = i)
      classv <- prep$fct_class(id = i)
      vlevs <- vlev[[vname]]
      cid <- intersect(prep$fct_child(id = i), uid)
      if(!is_empty(cid)) {
        # currently uses the first child only
        cname <- prep$fct_names(id = cid)
        serve_unit_with_child(vlevs, vname, classv,
                              as.character(res[[cname[1]]]), cname[1], prep)
      } else {
        serve_unit_with_no_child(vlevs, vname, classv)
      }
    })
    names(lvs) <- prep$fct_names(id = lid)
    res <- c(res, lvs)
    wid <- setdiff(wid, lid)
    wprep <- select_units(prep, prep$fct_names(wid))
    lid <- wprep$fct_leaves
  }
  res
}

serve_trts <- function(prep, lunits) {
  tids <- prep$trt_ids
  tnames <- prep$trt_names
  lvs <- lapply(tids, function(i) {
    serve_trt(prep, i, lunits)
  })
  names(lvs) <- tnames
  lvs
}

serve_trt <- function(prep, tid, lunits) {
  lnodes <- prep$lvl_nodes
  ledges <- prep$lvl_edges
  tdf <- lnodes[lnodes$idvar == tid, ]
  ltids <- tdf$id
  luids <- prep$lvl_child(id = ltids)
  ledges <- ledges[ledges$to %in% luids & ledges$from %in% ltids,]
  aunit <- prep$fct_names(id = prep$fct_child(id = tid))
  if(!is_empty(aunit)) {
    dict <- set_names(prep$lvl_names(ledges$from), prep$lvl_names(ledges$to))
    labels <- unname(dict[lunits[[aunit]]])
  } else {
    labels <- tdf$label
  }
  new_edibble_fct(levels = tdf$label,
                  labels = labels,
                  name = prep$fct_names(id = tid),
                  class = prep$fct_class(id = tid))
}
