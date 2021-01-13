#' Subset an edibble graph
#'
#' @description
#' The `subset` method subsets vertices of edibble graph based on vertex attributes.
#' The `subset_vars` and `subset_levels` subsets the edibble graph to
#' variable node or level nodes, respectively. This is essentially what we
#' refer to as high-level view or low-level view of the edibble graph respectively.
#'
#' @param .graph An edibble graph.
#' @param ... Expressions that return a logical value.
#' @param .vtype The vertex type to subset. By default, all vertices are returned.
#'   For `"var"`, only vertices that have attribute `vtype` as "var" is returned.
#' @name subset-edbl-graph
#' @importFrom igraph induced_subgraph vertex_attr
#' @examples
#' graph <- get_edibble_graph(rye_grass)
#' subset(graph, vname %in% c("strip", "plot"))
#' subset_vars(graph)
#' susbet_levels(graph)
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

