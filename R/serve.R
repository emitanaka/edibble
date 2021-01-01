#' Serve edibble table
#' @export
serve_table <- function(.data, ...) {
  UseMethod("serve_table")
}

#' Serve edibble table
#' @description
#' This converts an edibble graph object to a data frame called edibble.
#'
#' @param .data An `edbl_graph` object.
#' @return An `edbl` data frame with columns defined by vertices and
#' rows displayed only if the vertices are connected and reconcile for output.
#' @export
serve_table.edbl_graph <- function(.data, ...) {

  if(!igraph::is_connected(.data)) {
    lout <- serve_vars_not_reconciled(.data)
  } else {
    classes <- V(.data)$class
    lunit <- ltrt <- lresp <- list()
    if("edbl_unit" %in% classes) lunit <- serve_units(.data)
    if("edbl_trt" %in% classes) ltrt <- serve_trts(.data, lunit)
    if("edbl_resp" %in% classes) lresp <- serve_resps(.data, lunit)
    lout <- c(lunit, ltrt, lresp)
  }

  vnames <- names(subset_vars(.data))
  new_edibble(lout[vnames], graph = .data)
}




# Returns list of edibble variables
serve_vars_not_reconciled <- function(.data) {
  ugraph <- subset_vars(.data)
  vnames <- V(ugraph)$name
  res <- lapply(vnames,
                function(avar) {
                  new_edibble_var(levels = var_levels(.data, avar),
                                  name = avar,
                                  class = var_class(.data, avar))
                })
  names(res) <- vnames
  res
}

child_to_parent_dict <- function(.data, parent_vname, child_vname, etype = "l2l") {
  lgraph <- subset(.data,
                   vname %in% c(parent_vname, child_vname),
                   .vtype = "level")
  df <- endpoints(lgraph, etype, "level")
  set_names(df$from, df$to)
}


# Return edibble unit
serve_unit_with_child <- function(.data, parent_vname, child_vname, child_labels) {
  dict <- child_to_parent_dict(.data, parent_vname, child_vname)
  child_vnames <- vertex_level_names(child_vname, child_labels)
  parent_vnames <- unname(dict[child_vnames])

  new_edibble_var(levels = unname(var_levels(.data, parent_vname, label = TRUE)),
                  labels = names_to_lnames(.data, parent_vnames),
                  name = parent_vname,
                  class = var_class(.data, parent_vname))

}

serve_unit_with_no_child <- function(.data, vname) {
  new_edibble_var(levels = unname(var_levels(.data, vname, label = TRUE)),
                  labels = unname(var_levels(.data, vname, label = TRUE)),
                  name = vname,
                  class = var_class(.data, vname))
}

get_vertex_child <- function(.data, vname) {
  vidx <- which(V(.data)$name==vname)
  res <- setdiff(igraph::neighbors(.data, vidx, mode = "out"), vidx)
  if(length(res)) return(res)
  NULL
}

serve_resps <- function(.data, lunits) {

}

serve_units <- function(.data) {
  ugraph <- subset(.data, class=="edbl_unit", .vtype = "var")
  leaves <- V(ugraph)$vname[igraph::degree(ugraph, mode = "out")==0]
  wgraph <- ugraph
  res <- list()
  while(length(leaves) > 0) {
    lvs <- lapply(leaves, function(aleaf) {
            child_vertex <- get_vertex_child(ugraph, aleaf)
            if(length(child_vertex) > 0) {
              child_name <- var_names(ugraph, child_vertex)
              serve_unit_with_child(.data, aleaf, child_name, res[[child_name]])
            } else {
              serve_unit_with_no_child(.data, aleaf)
            }
      })
    names(lvs) <- leaves
    res <- c(res, lvs)
    wgraph <- igraph::delete_vertices(wgraph, V(wgraph)$name %in% leaves)
    leaves <- V(wgraph)$name[igraph::degree(wgraph, mode = "out")==0]
  }
  res
}

serve_trts <- function(.data, lunits) {
  ugraph <- subset(.data, class=="edbl_trt", .vtype = "var")
  vnames <- V(ugraph)$vname
  lvs <- lapply(vnames, function(aname) {
    serve_trt(.data, aname, lunits)
  })
  names(lvs) <- vnames
  lvs
}

serve_trt <- function(.data, vname, lunits) {
  ugraph <- subset_vars(.data)
  child_vertex <- get_vertex_child(ugraph, vname)
  child_vname <- var_names(ugraph, child_vertex)
  dict <- child_to_parent_dict(.data, vname, child_vname, etype = "t2v")
  child_vnames <- vertex_level_names(child_vname, lunits[[child_vname]])
  parent_vnames <- unname(dict[child_vnames])

  new_edibble_var(levels = unname(var_levels(.data, vname, label = TRUE)),
                  labels = names_to_lnames(.data, parent_vnames),
                  name = vname,
                  class = var_class(.data, vname))
}