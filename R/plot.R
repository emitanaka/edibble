#' @export
plot.edbl_design <- function(.design, view = c("factors", "levels"), width = "100%", height = NULL, seed = 1, ...) {
  if(!require("visNetwork")) abort("You need to install `visNetwork` package.")
  view <- match.arg(view)
  nodes <- switch(view,
                  "factors" = fct_nodes(.design),
                  "levels" = lvl_nodes(.design))
  nodes$group <- switch(view,
                        "factors" = gsub("edbl_", "", nodes$class),
                        "levels" = nodes$var)
  class2shape <- c("edbl_unit" = "circle",
                   "edbl_trt" = "diamond",
                   "edbl_rcrd" = "database")
  nodes$shape <- class2shape[fct_class(.design, nodes$idvar)]
  edges <- switch(view,
                  "factors" = fct_edges(.design),
                  "levels" = lvl_edges(.design))
  visNetwork::visNetwork(nodes = nodes,
                         edges = edges,
                         width = width,
                         height = height,
                         main = .design$name) %>%
    visNetwork::visLayout(randomSeed = seed) %>%
    visNetwork::visNodes(id = "id") %>%
    visNetwork::visEdges(arrows = "to", id = "id") %>%
    #visNetwork::visLegend() %>%
    visNetwork::visOptions(highlightNearest = TRUE,
                           nodesIdSelection = TRUE,
                           collapse = TRUE,
                           manipulation = TRUE,
                           selectedBy = "group") %>%
    visNetwork::visInteraction(navigationButtons = TRUE)
}

#' @export
plot.edbl_table <- function(.table, ...) {
  des <- edbl_design(.table)
  plot(des, ...)
}
