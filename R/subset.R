#' Subset an edibble graph
#'
#' @description
#' This function subsets vertices of edibble graph based on vertex attributes.
#'
#' @param .graph An edibble graph.
#' @param ... Expressions that return a logical value.
#' @param .vtype The vertex type to subset. By default, all vertices are returned.
#'   For `"var"`, only vertices that have attribute `vtype` as "var" is returned.
#' @name subset-edbl-graph
#' @importFrom igraph induced_subgraph vertex_attr
#' @export
subset.edbl_graph <- function(.graph, ..., .vtype = c("all", "var", "level")) {
  .vtype <- match.arg(.vtype)
  dots <- enquos(...)
  out <- switch(.vtype,
                "all" = .graph,
                "var" = subset_vars(.graph),
                "level" = subset_levels(.graph))
  for(i in seq_along(dots)) {
    ind <- which(eval_tidy(dots[[i]], vertex_attr(out)))
    out <- induced_subgraph(out, ind)
  }
  reinstate_graph_attrs(out, .graph)
}

#' @rdname subset-edbl-graph
#' @export
subset_vars <- function(.graph) {
  reinstate_graph_attrs(induced_subgraph(.graph, V(.graph)$vtype=="var"),
                        .graph)
}

#' @rdname subset-edbl-graph
#' @export
subset_levels <- function(.graph) {
  reinstate_graph_attrs(induced_subgraph(.graph, V(.graph)$vtype=="level"),
                        .graph)
}

