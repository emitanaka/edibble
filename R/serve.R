#' Serve edibble table
#' @description
#' This converts an edibble graph object to a data frame called edibble.
#'
#' @param .design An `edbl_graph` object.
#' @return An `edbl` data frame with columns defined by vertices and
#' rows displayed only if the vertices are connected and reconcile for output.
#' @importFrom igraph is_connected
#' @importFrom rlang set_names
#' @export
serve_table <- function(.design, ...) {
  graph <- .design$graph
  lgraph <- .design$subset_graph(type = "level")
  if(!is_connected(lgraph)) {
    lout <- serve_vars_not_reconciled(graph)
  } else {
    classes <- .design$var_class()
    lunit <- ltrt <- lresp <- list()
    if("edbl_unit" %in% classes) lunit <- serve_units(graph)
    if("edbl_trt" %in% classes) ltrt <- serve_trts(graph, lunit)
    if("edbl_resp" %in% classes) lresp <- serve_resps(graph, lunit)
    lout <- c(lunit, ltrt, lresp)
  }

  vnames <- .design$var_names()
  new_edibble(lout[vnames], design = .design)
}


# Returns list of edibble variables
serve_vars_not_reconciled <- function(.design) {
  ugraph <- subset_vars(.design)
  vnames <- V(ugraph)$name
  res <- lapply(vnames,
                function(avar) {
                  new_edibble_var(levels = var_levels(.design, avar),
                                  name = avar,
                                  class = var_class(.design, avar))
                })
  names(res) <- vnames
  res
}

child_to_parent_dict <- function(.design, parent_vname, child_vname, etype = "l2l") {
  lgraph <- subset(.design,
                   vname %in% c(parent_vname, child_vname),
                   .vtype = "level")
  df <- endpoints(lgraph, etype, "level")
  set_names(df$from, df$to)
}


# Return edibble unit
serve_unit_with_child <- function(.design, parent_vname, child_vname, child_labels) {
  dict <- child_to_parent_dict(.design, parent_vname, child_vname)
  child_vnames <- vertex_level_names(child_vname, child_labels)
  parent_vnames <- unname(dict[child_vnames])

  new_edibble_var(levels = unname(var_levels(.design, parent_vname, label = TRUE)),
                  labels = names_to_lnames(.design, parent_vnames),
                  name = parent_vname,
                  class = var_class(.design, parent_vname))

}

serve_unit_with_no_child <- function(.design, vname) {
  new_edibble_var(levels = unname(var_levels(.design, vname, label = TRUE)),
                  labels = unname(var_levels(.design, vname, label = TRUE)),
                  name = vname,
                  class = var_class(.design, vname))
}

#' @importFrom igraph neighbors V
get_vertex_child <- function(.design, vname) {
  vidx <- which(V(.design)$name==vname)
  res <- setdiff(neighbors(.design, vidx, mode = "out"), vidx)
  if(length(res)) return(res)
  NULL
}

serve_resps <- function(.design, lunits) {

}

#' @importFrom igraph degree V delete_vertices
serve_units <- function(.design) {
  ugraph <- subset(.design, class=="edbl_unit", .vtype = "var")
  leaves <- V(ugraph)$vname[degree(ugraph, mode = "out")==0]
  wgraph <- ugraph
  res <- list()
  while(length(leaves) > 0) {
    lvs <- lapply(leaves, function(aleaf) {
            child_vertex <- get_vertex_child(ugraph, aleaf)
            if(length(child_vertex) > 0) {
              child_name <- var_names(ugraph, child_vertex)
              serve_unit_with_child(.design, aleaf, child_name, res[[child_name]])
            } else {
              serve_unit_with_no_child(.design, aleaf)
            }
      })
    names(lvs) <- leaves
    res <- c(res, lvs)
    wgraph <- delete_vertices(wgraph, V(wgraph)$name %in% leaves)
    leaves <- V(wgraph)$name[degree(wgraph, mode = "out")==0]
  }
  res
}

#' @importFrom igraph V
serve_trts <- function(.design, lunits) {
  ugraph <- subset(.design, class=="edbl_trt", .vtype = "var")
  vnames <- V(ugraph)$vname
  lvs <- lapply(vnames, function(aname) {
    serve_trt(.design, aname, lunits)
  })
  names(lvs) <- vnames
  lvs
}

serve_trt <- function(.design, vname, lunits) {
  ugraph <- subset_vars(.design)
  child_vertex <- get_vertex_child(ugraph, vname)
  child_vname <- var_names(ugraph, child_vertex)
  dict <- child_to_parent_dict(.design, vname, child_vname, etype = "t2v")
  child_vnames <- vertex_level_names(child_vname, lunits[[child_vname]])
  parent_vnames <- unname(dict[child_vnames])

  new_edibble_var(levels = unname(var_levels(.design, vname, label = TRUE)),
                  labels = names_to_lnames(.design, parent_vnames),
                  name = vname,
                  class = var_class(.design, vname))
}
