#' Plot graph of edibble variables
#'
#' @param .data An edibble graph.
#' @param view A string specifying the level of the view to plot.
#' @param ... Additional parameters passed to `plot.igraph`.
#' @param layout An igraph layout.
#' @examples
#' plot(.data, sample_levels(n = 30))
#'
#' @export
plot.edbl_graph <- function(.data, view = c("high", "low"), ...,
                            layout = igraph::layout.auto,
                            main = NULL) {

  view <- match.arg(view)
  out <- switch(view,
                "high" = subset_vars(.data),
                "low"  = subset_levels(.data))

  if(is_null(main)) {
    main <- igraph::graph_attr(.data, "name") %||% "An edibble design"
  }

  igraph::plot.igraph(out, ..., annotate.plot = TRUE,
                      main = main,
                      layout = layout)
}


# questioning - remove?
select_units <- function(.edbl) {
  ind <- unlist(lapply(.edbl, function(x) "edibble_unit" %in% class(x)))
  .edbl[, ind]
}

# questioning - remove?
select_trts <- function(.edbl) {
  ind <- unlist(lapply(.edbl, function(x) "edibble_trt" %in% class(x)))
  .edbl[, ind]
}

#as.data.frame.edbl <- function(x) NextMethod()
