#' Plot graph of edibble variables
#'
#' @param .data An edibble desgin.
#' @param view A string specifying the level of the view to plot.
#' @param ... Additional parameters passed to `plot.igraph`.
#' @param main The title of the plot. By default it is`.data$.name`.
#' @export
plot.EdibbleDesign <- function(.data, view = "high", ..., main = NULL) {
  .data$plot(view = view, ..., main = main)
}
