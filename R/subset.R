#' Subset an edibble nexus
#'
#' @description
#' This function subsets vertices of edibble nexus based on vertex attributes.
#'
#' @param .nexus An edibble nexus.
#' @param ... Expressions that return a logical value.
#' @param .vtype The vertex type to subset. By default, all vertices are returned.
#'   For `"var"`, only vertices that have attribute `vtype` as "var" is returned.
#' @name subset-edbl-nexus
#'
#' @export
subset.edbl_nexus <- function(.nexus, ..., .vtype = c("all", "var", "level")) {
  .vtype <- match.arg(.vtype)
  dots <- enquos(...)
  out <- switch(.vtype,
                "all" = .nexus,
                "var" = subset_vars(.nexus),
                "level" = subset_levels(.nexus))
  for(i in seq_along(dots)) {
    ind <- which(eval_tidy(dots[[i]], igraph::vertex_attr(out)))
    out <- igraph::induced_subgraph(out, ind)
  }
  class(out) <- class(.nexus)
  out
}

#' @rdname subset-edbl-nexus
#' @export
subset_vars <- function(.nexus) {
  structure(igraph::induced_subgraph(.nexus, V(.nexus)$vtype=="var"),
            class = class(.nexus))
}

#' @rdname subset-edbl-nexus
#' @export
subset_levels <- function(.nexus) {
  structure(igraph::induced_subgraph(.nexus, V(.nexus)$vtype=="level"),
            class = class(.nexus))
}
