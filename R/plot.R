
#' Interactive plot of the edibble design
#' @param .design An edibble design.
#' @param which A string of either "factors" or "levels".
#' @param width,height The width and height of the plot.
#' @param seed A seed number so same plot is always generated.
#' @param title The title of the plot. By default it uses the name from the `.design` object.
#' @param view A string of either "show-buttons" (default), "hide-buttons", "static"
#' @export
plot.edbl_design <- function(.design, which = c("factors", "levels"),
                             width = "100%", height = NULL, seed = 1, title = NULL,
                             view = c("show-buttons", "hide-buttons", "static"), ...) {
  if(!require("visNetwork")) abort("You need to install `visNetwork` package.")
  which <- match.arg(which)
  view <- match.arg(view)

  nodes <- switch(which,
                  "factors" = fct_nodes(.design),
                  "levels" = lvl_nodes(.design))
  nodes$group <- switch(which,
                        "factors" = gsub("edbl_", "", nodes$class),
                        "levels" = nodes$var)
  nodes$label <- nodes$name
  class2shape <- c("edbl_unit" = "circle",
                   "edbl_trt" = "diamond",
                   "edbl_rcrd" = "database")
  nodes$shape <- class2shape[fct_class(.design, nodes$idvar)]
  edges <- switch(which,
                  "factors" = fct_edges(.design),
                  "levels" = lvl_edges(.design))
  out <- visNetwork::visNetwork(nodes = nodes,
                               edges = edges,
                               width = width,
                               height = height,
                               main = title %||% .design$name) %>%
    visNetwork::visLayout(randomSeed = seed) %>%
    visNetwork::visNodes(id = "id") %>%
    visNetwork::visEdges(arrows = "to", id = "id")
    #visNetwork::visLegend() %>%
  switch(view,
         "show-buttons" = out %>%
           visNetwork::visOptions(highlightNearest = TRUE,
                                  nodesIdSelection = TRUE,
                                  collapse = TRUE,
                                  manipulation = TRUE,
                                  selectedBy = "group") %>%
           visNetwork::visInteraction(navigationButtons = TRUE),
         "hide-buttons" = out,
         "static" = out %>%
           visNetwork::visInteraction(dragNodes = FALSE,
                                      dragView = FALSE,
                                      hover = FALSE,
                                      keyboard = FALSE,
                                      multiselect = FALSE,
                                      navigationButtons = FALSE,
                                      selectable = FALSE,
                                      zoomView = FALSE) )

}

#' @rdname plot.edbl_design
#' @export
plot.edbl_table <- function(.table, ...) {
  des <- edbl_design(.table)
  plot(des, ...)
}
