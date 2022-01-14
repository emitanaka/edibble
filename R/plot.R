#' @export
plot.edbl_design <- function(.design, view = c("factors", "levels"), width = "100%", height = NULL, ...) {
  if(!require("visNetwork")) abort("You need to install `visNetwork` package.")
  view <- match.arg(view)
  nodes <- switch(view,
                  "factors" = fct_nodes(.design),
                  "levels" = lvl_nodes(.design))
  nodes$group <- switch(view,
                        "factors" = gsub("edbl_", "", nodes$class),
                        "levels" = nodes$var)
  edges <- switch(view,
                  "factors" = fct_edges(.design),
                  "levels" = lvl_edges(.design))
  visNetwork::visNetwork(nodes = nodes,
                         edges = edges,
                         width = width,
                         height = height,
                         main = .design$name) %>%
    visNetwork::visEdges(arrows = "to", id = "id") %>%
    visNetwork::visLegend() %>%
    visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE,
                           collapse = TRUE) %>%
    visNetwork::visInteraction(navigationButtons = TRUE)
}

#' @export
plot.edbl_table <- function(.table, ...) {
  des <- edbl_design(.table)
  plot(des, ...)
}
