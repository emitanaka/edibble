#' Plot graph of edibble variables
#'
#' @param .data An edibble graph.
#' @param view A string specifying the level of the view to plot.
#' @param ... Additional parameters passed to `plot.igraph`.
#' @param layout An igraph layout.
#' @examples
#' plot(.data, sample_levels(n = 30))
#' @importFrom igraph plot.igraph
#' @export
plot.edbl_graph <- function(.data, view = c("high", "low"), ...,
                            main = NULL) {

  view <- match.arg(view)
  out <- switch(view,
                high = subset_vars(.data),
                low = subset_levels(.data))

  main <- main %||% "An edibble design"

  plot.igraph(out, ..., annotate.plot = TRUE,
              main = main)
}

#' @export
plot.EdibbleDesign <- function(.data, ...) {
  .data$plot(...)
}
