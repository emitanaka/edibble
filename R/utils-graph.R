# Simple graph structure

empty_edibble_graph <- function(class) {
  nodes <- data.frame(id = integer(), label = character(), stringsAsFactors = FALSE)
  if(class == "edbl_lgraph") nodes <- tibble::add_column(nodes, idvar = integer(), .before = "id")
  if(class == "edbl_vgraph") nodes <- tibble::add_column(nodes, class = character(), .after = "label")

  structure(list(nodes = nodes,
                 edges = data.frame(from = integer(), to = integer(),
                                    alloc = integer(),
                                    stringsAsFactors = FALSE)),
            class = c(class, "edbl_graph"))
}

# vectorised
vid <- function(.graph, label = NULL) {
  idv <- .graph$nodes$id
  namesv <- .graph$nodes$label
  label <- label %||% namesv
  map_int(label, function(vname) idv[namesv==vname])
}

vlabel <- function(.graph, id = NULL) {
  id <- id %||% .graph$nodes$id
  map_chr(id, function(x) .graph$nodes$label[.graph$nodes$id==x])
}

vclass <- function(.graph, id = NULL) {
  id <- id %||% .graph$nodes$id
  map_chr(id, function(x) .graph$nodes$class[.graph$nodes$id==x])
}

vchild <- function(.graph, id = NULL) {
  childv <- .graph$edges$to
  childv[.graph$edges$from %in% id]
}

vparent <- function(.graph, id = NULL) {
  parentv <- .graph$edges$from
  parentv[.graph$edges$to %in% id]
}

# includes self
vancestor <- function(.graph, id = NULL) {
  out <- id
  if(!is_empty(vparent(.graph, id))) {
    out <- c(out, vancestor(.graph, vparent(.graph, id)))
  }
  out
}

vlevels <- function(.design, id = NULL, label = NULL) {
  idv <- .design$vgraph$nodes$id
  namesv <- .design$vgraph$nodes$label
  qid <- id %||% vid(.design$vgraph, label)
  df <- .design$lgraph$nodes[.design$lgraph$nodes$idvar %in% qid,]
  df$var <- vlabel(.design$vgraph, df$idvar)
  split(df$label, df$var)
}

#' Get the variable and level graph
#'
#' This function computes extra attributes such as
#' the the size and label names in the edges.
#'
#' @param design An edibble design
#' @param node_var The name of the column to select from the nodes
#' @param edge_var The name of the column to select from the edges
#' Ignored if `node_var` is defined instead.
#' @export
vgraph <- function(design, node_var = NULL, edge_var = NULL) {
  edges <- design$vgraph$edges
  edges$var_from <- vlabel(design$vgraph, id = edges$from)
  edges$var_to <- vlabel(design$vgraph, id = edges$to)
  nodes <- design$vgraph$nodes
  nodes$n <- lengths(vlevels(design)[nodes$label])
  if(is_null(node_var) & is_null(edge_var)) {
    structure(list(nodes = nodes,
                   edges = edges),
              class = c("edbl_vgraph", "edbl_graph"))
  } else if(is_null(node_var)) {
    edges[[edge_var]]
  } else {
    nodes[[node_var]]
  }
}

#' @export
lgraph <- function(design, node_var = NULL, edge_var = NULL) {
  edges <- design$lgraph$edges
  edges$var_from <- vlabel(design$lgraph, id = edges$from)
  edges$var_to <- vlabel(design$lgraph, id = edges$to)
  nodes <- design$lgraph$nodes
  nodes$var <- vlabel(design$vgraph, id = nodes$idvar)

  if(is_null(node_var) & is_null(edge_var)) {
    structure(list(nodes = nodes,
                   edges = edges),
              class = c("edbl_lgraph", "edbl_graph"))
  } else if(is_null(node_var)) {
    edges[[edge_var]]
  } else {
    nodes[[node_var]]
  }

}
