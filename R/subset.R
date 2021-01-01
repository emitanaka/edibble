#' Subset an edibble graph
#'
#' @description
#' This function subsets vertices of edibble graph based on vertex attributes.
#'
#' @param .data An edibble graph.
#' @param ... Expressions that return a logical value.
#' @param .vtype The vertex type to subset. By default, all vertices are returned.
#'   For `"var"`, only vertices that have attribute `vtype` as "var" is returned.
#' @name subset-edbl-graph
#'
#' @export
subset.edbl_graph <- function(.data, ..., .vtype = c("all", "var", "level")) {
  .vtype <- match.arg(.vtype)
  dots <- enquos(...)
  out <- switch(.vtype,
                "all" = .data,
                "var" = subset_vars(.data),
                "level" = subset_levels(.data))
  for(i in seq_along(dots)) {
    ind <- which(eval_tidy(dots[[i]], igraph::vertex_attr(out)))
    out <- igraph::induced_subgraph(out, ind)
  }
  class(out) <- class(.data)
  out
}

#' @rdname subset-edbl-graph
#' @export
subset_vars <- function(.data) {
  structure(igraph::induced_subgraph(.data, V(.data)$vtype=="var"),
            class = class(.data))
}

#' @rdname subset-edbl-graph
#' @export
subset_levels <- function(.data) {
  structure(igraph::induced_subgraph(.data, V(.data)$vtype=="level"),
            class = class(.data))
}
