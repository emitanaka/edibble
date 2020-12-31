#' Serve edibble table
#' @export
serve_table <- function(.nexus, ...) {
  UseMethod("serve_table")
}

#' Serve edibble table
#' @description
#' This converts an edibble nexus object to a data frame called edibble.
#'
#' @param .nexus An `edbl_nexus` object.
#' @return An `edbl` data frame with columns defined by vertices and
#' rows displayed only if the vertices are connected and reconcile for output.
#' @export
serve_table.edbl_nexus <- function(.nexus, ...) {

  if(!igraph::is_connected(.nexus)) {
    lout <- serve_vars_not_reconciled(.nexus)
  } else {
    classes <- V(.nexus)$class
    lunit <- ltrt <- lresp <- list()
    if("edbl_unit" %in% classes) lunit <- serve_units(.nexus)
    if("edbl_trt" %in% classes) ltrt <- serve_trts(.nexus, lunit)
    if("edbl_resp" %in% classes) lresp <- serve_resps(.nexus, lunit)
    lout <- c(lunit, ltrt, lresp)
  }

  vnames <- names(subset_vars(.nexus))
  new_edibble(lout[vnames], nexus = .nexus)
}




# Returns list of edibble variables
serve_vars_not_reconciled <- function(.nexus) {
  vnames <- V(.nexus)$vname
  res <- lapply(vnames,
                function(avar) {
                  new_edibble_var(levels = var_levels(.nexus, avar),
                                  name = avar,
                                  class = var_class(.nexus, avar))
                })
  names(res) <- vnames
  res
}

child_to_parent_dict <- function(.nexus, parent_vname, child_vname, etype = "l2l") {
  lnexus <- subset(.nexus,
                   vname %in% c(parent_vname, child_vname),
                   .vtype = "level")
  df <- endpoints(lnexus, etype, "level")
  set_names(df$from, df$to)
}


# Return edibble unit
serve_unit_with_child <- function(.nexus, parent_vname, child_vname, child_labels) {
  dict <- child_to_parent_dict(.nexus, parent_vname, child_vname)
  child_vnames <- vertex_level_names(child_vname, child_labels)
  parent_vnames <- unname(dict[child_vnames])

  new_edibble_var(levels = unname(var_levels(.nexus, parent_vname, label = TRUE)),
                  labels = names_to_lnames(.nexus, parent_vnames),
                  name = parent_vname,
                  class = var_class(.nexus, parent_vname))

}

serve_unit_with_no_child <- function(.nexus, vname) {
  new_edibble_var(levels = unname(var_levels(.nexus, vname, label = TRUE)),
                  labels = unname(var_levels(.nexus, vname, label = TRUE)),
                  name = vname,
                  class = var_class(.nexus, vname))
}

get_vertex_child <- function(.nexus, vname) {
  vidx <- which(V(.nexus)$name==vname)
  res <- setdiff(igraph::neighbors(.nexus, vidx, mode = "out"), vidx)
  if(length(res)) return(res)
  NULL
}

serve_resps <- function(.nexus, lunits) {

}

serve_units <- function(.nexus) {
  unexus <- subset(.nexus, class=="edbl_unit", .vtype = "var")
  leaves <- V(unexus)$vname[igraph::degree(unexus, mode = "out")==0]
  wnexus <- unexus
  res <- list()
  while(length(leaves) > 0) {
    lvs <- lapply(leaves, function(aleaf) {
            child_vertex <- get_vertex_child(unexus, aleaf)
            if(length(child_vertex) > 0) {
              child_name <- var_names(unexus, child_vertex)
              serve_unit_with_child(.nexus, aleaf, child_name, res[[child_name]])
            } else {
              serve_unit_with_no_child(.nexus, aleaf)
            }
      })
    names(lvs) <- leaves
    res <- c(res, lvs)
    wnexus <- igraph::delete_vertices(wnexus, V(wnexus)$name %in% leaves)
    leaves <- V(wnexus)$name[igraph::degree(wnexus, mode = "out")==0]
  }
  res
}

serve_trts <- function(.nexus, lunits) {
  unexus <- subset(.nexus, class=="edbl_trt", .vtype = "var")
  vnames <- V(unexus)$vname
  lvs <- lapply(vnames, function(aname) {
    serve_trt(.nexus, aname, lunits)
  })
  names(lvs) <- vnames
  lvs
}

serve_trt <- function(.nexus, vname, lunits) {
  unexus <- subset_vars(.nexus)
  child_vertex <- get_vertex_child(unexus, vname)
  child_vname <- var_names(unexus, child_vertex)
  dict <- child_to_parent_dict(.nexus, vname, child_vname, etype = "t2v")
  child_vnames <- vertex_level_names(child_vname, lunits[[child_vname]])
  parent_vnames <- unname(dict[child_vnames])

  new_edibble_var(levels = unname(var_levels(.nexus, vname, label = TRUE)),
                  labels = names_to_lnames(.nexus, parent_vnames),
                  name = vname,
                  class = var_class(.nexus, vname))
}
