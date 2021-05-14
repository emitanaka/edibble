#' Serve edibble table
#' @description
#' This converts an edibble graph object to a data frame called edibble.
#' This function should be used when the design is in the final form
#' (or close to the final form). The table can only be formed when the
#' variables can be reconciled, otherwise it will be a data frame with
#' zero rows.
#'
#' @param .design An `edbl_graph` object.
#' @param ... Ignored.
#' @return An `edbl` data frame with columns defined by vertices and
#' rows displayed only if the vertices are connected and reconcile for output.
#' @importFrom igraph is_connected
#' @importFrom rlang set_names
#' @family user-facing functions
#' @export
serve_table <- function(.design, ...) {
  # this causes C stack error
  #.design <- get_edibble_design(.design)
  lgraph <- subset_levels(.design$graph)
  if(!is_connected(lgraph)) {
    lout <- serve_vars_not_reconciled(.design)
  } else {
    classes <- var_class(.design$graph)
    lunit <- ltrt <- lvar <- list()
    if("edbl_unit" %in% classes) lunit <- serve_units(.design$graph)
    if("edbl_trt" %in% classes) {
      if(is.null(.design$allocation)) {
         .design$assign_allocation()
      }
      ltrt <- serve_trts(.design$graph, lunit)
    }
    if("edbl_rcrd" %in% classes) lvar <- serve_rcrds(.design$graph, lunit)
    lout <- c(lunit, ltrt, lvar)
  }

  vnames <- names_vars(.design)
  new_edibble(lout[vnames], design = .design)
}

serve_rcrds <- function(.graph, lunits) {
  vgraph <- subset_vars(.graph)
  N <- max(lengths(lunits))
  rcrd2unit <- rcrd_to_unit_dict(.graph)
  rnames <- names(rcrd2unit)
  res <- lapply(rnames,
                function(avar) {
                  new_edibble_rcrd(N, lunits[[rcrd2unit[avar]]])
                })
  names(res) <- rnames
  res
}

rcrd_to_unit_dict <- function(.graph) {
  rcrd2unit <- endpoints(.graph, "r2v", "var")
  setNames(rcrd2unit$to, rcrd2unit$from)
}


# Returns list of edibble variables
serve_vars_not_reconciled <- function(.design) {
  graph <- .design$graph
  vnames <- names_vars(.design)
  res <- lapply(vnames,
                function(avar) {
                  new_edibble_var(levels = var_levels(graph, avar),
                                  name = avar,
                                  class = var_class(graph, avar))
                })
  names(res) <- vnames
  res
}

child_to_parent_dict <- function(.graph, parent_vname, child_vname, etype = "l2l") {
  lgraph <- subset(.graph,
                   vname %in% c(parent_vname, child_vname),
                   .vtype = "level")
  df <- endpoints(lgraph, etype, "level")
  set_names(df$from, df$to)
}


# Return edibble unit
serve_unit_with_child <- function(.graph, parent_vname, child_vname, child_labels) {
  dict <- child_to_parent_dict(.graph, parent_vname, child_vname)
  child_vnames <- vertex_level_names(child_vname, child_labels)
  parent_vnames <- unname(dict[child_vnames])

  new_edibble_var(levels = unname(var_levels(.graph, parent_vname, label = TRUE)),
                  labels = names_to_lnames(.graph, parent_vnames),
                  name = parent_vname,
                  class = var_class(.graph, parent_vname))

}

serve_unit_with_no_child <- function(.graph, vname) {
  new_edibble_var(levels = unname(var_levels(.graph, vname, label = TRUE)),
                  labels = unname(var_levels(.graph, vname, label = TRUE)),
                  name = vname,
                  class = var_class(.graph, vname))
}

#' @importFrom igraph neighbors V
get_vertex_child <- function(graph, vname) {
  vidx <- which(V(graph)$name==vname)
  res <- setdiff(neighbors(graph, vidx, mode = "out"), vidx)
  if(length(res)) return(res)
  NULL
}



#' @importFrom igraph degree V delete_vertices
serve_units <- function(graph) {
  ugraph <- subset(graph, class=="edbl_unit", .vtype = "var")
  leaves <- V(ugraph)$vname[degree(ugraph, mode = "out")==0]
  wgraph <- ugraph
  res <- list()
  while(length(leaves) > 0) {
    lvs <- lapply(leaves, function(aleaf) {
            child_vertex <- get_vertex_child(ugraph, aleaf)
            if(length(child_vertex) > 0) {
              child_name <- V(ugraph)$name[child_vertex]
              serve_unit_with_child(graph, aleaf, child_name, res[[child_name]])
            } else {
              serve_unit_with_no_child(graph, aleaf)
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
serve_trts <- function(graph, lunits) {
  ugraph <- subset(graph, class=="edbl_trt", .vtype = "var")
  vnames <- V(ugraph)$vname
  lvs <- lapply(vnames, function(aname) {
    serve_trt(graph, aname, lunits)
  })
  names(lvs) <- vnames
  lvs
}

serve_trt <- function(graph, vname, lunits) {
  ugraph <- subset_vars(graph)
  child_vertex <- get_vertex_child(ugraph, vname)
  child_vname <- V(ugraph)$name[child_vertex]
  dict <- child_to_parent_dict(graph, vname, child_vname, etype = "t2v")
  child_vnames <- vertex_level_names(child_vname, lunits[[child_vname]])
  parent_vnames <- unname(dict[child_vnames])

  new_edibble_var(levels = unname(var_levels(graph, vname, label = TRUE)),
                  labels = names_to_lnames(graph, parent_vnames),
                  name = vname,
                  class = var_class(graph, vname))
}
