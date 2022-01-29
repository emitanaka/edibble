
#' Interactive plot of the edibble design
#' @param .design An edibble design.
#' @param view A string of either "factors" or "levels".
#' @param width,height The width and height of the plot.
#' @param seed A seed number so same plot is always generated.
#' @param show_buttons A logical value indicating whether to hide or show the buttons.
#' @export
plot.edbl_design <- function(.design, view = c("factors", "levels"), width = "100%", height = NULL, seed = 1, show_buttons = TRUE, ...) {
  if(!require("visNetwork")) abort("You need to install `visNetwork` package.")
  view <- match.arg(view)
  nodes <- switch(view,
                  "factors" = fct_nodes(.design),
                  "levels" = lvl_nodes(.design))
  nodes$group <- switch(view,
                        "factors" = gsub("edbl_", "", nodes$class),
                        "levels" = nodes$var)
  nodes$label <- nodes$name
  class2shape <- c("edbl_unit" = "circle",
                   "edbl_trt" = "diamond",
                   "edbl_rcrd" = "database")
  nodes$shape <- class2shape[fct_class(.design, nodes$idvar)]
  edges <- switch(view,
                  "factors" = fct_edges(.design),
                  "levels" = lvl_edges(.design))
  out <- visNetwork::visNetwork(nodes = nodes,
                               edges = edges,
                               width = width,
                               height = height,
                               main = .design$name) %>%
    visNetwork::visLayout(randomSeed = seed) %>%
    visNetwork::visNodes(id = "id") %>%
    visNetwork::visEdges(arrows = "to", id = "id")
    #visNetwork::visLegend() %>%
  if(show_buttons) {
    out %>%
      visNetwork::visOptions(highlightNearest = TRUE,
                             nodesIdSelection = TRUE,
                             collapse = TRUE,
                             manipulation = TRUE,
                             selectedBy = "group") %>%
      visNetwork::visInteraction(navigationButtons = TRUE)
  } else {
    out
  }
}

#' @rdname plot.edbl_design
#' @export
plot.edbl_table <- function(.table, ...) {
  des <- edbl_design(.table)
  plot(des, ...)
}
