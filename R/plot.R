#' Plot graph of an edibble design
#'
#' @description
#'
#' There are two available view for the graph of edibble variables:
#'  * `"high"` level view where only nodes that are variables are shown, or
#'  * `"low"` level view where only nodes that are levels are shown.
#' The default view is "high" since the "low" view is usually too cluttered to
#' be of any use.
#'
#' The main use for this is to visualise the relationship between variables
#' and levels interactively. The plot output is created using `plot.igraph()`.
#' As typical of network visualisation, the layout may require adjusting for a
#' better look.
#'
#' This plot is not optimal for a polished publication. It is designed more for
#' a quick exploratory use.
#'
#'
#' @param .edibble An edibble desgin, an edibble table or an edibble graph.
#' @param view A string specifying the level of the view ("high" or "low") to plot.
#'  By default it is "high".
#' @param ... Additional parameters passed to `plot.igraph`.
#' @param main The title of the plot. By default it is the
#'  name of the edibble design.
#' @importFrom igraph plot.igraph
#' @name plot.edibble
#' @return The return object is the original input.
#' @seealso See [iplot()] for interactive plot of the graph of an edibble design
#' or \code{\link[deggust]{autoplot.edibble}} for ggplot version of the plot.
NULL

#' @rdname plot.edibble
#' @export
plot.EdibbleDesign <- function(.edibble, view = c("high", "low"), ..., main = NULL) {
  main <- main %||% .edibble$name
  .graph <- get_edibble_graph(.edibble)
  plot(.graph, view = view, ..., main = main)
  invisible(.edibble)
}

#' @rdname plot.edibble
#' @export
plot.edbl_graph <- function(.edibble, view = c("high", "low"), ..., main = NULL) {
  main <- main %||% "An edibble design" # note ebbl_graph doesn't have name
  view <- match.arg(view)
  out <- switch(view,
                high = subset_vars(.edibble),
                low = subset_levels(.edibble))
  plot.igraph(out, ...,
              annotate.ploggt = TRUE,
              main = main)
  invisible(.edibble)
}

#' @rdname plot.edibble
#' @export
plot.edbl_table <- function(.edibble, view = c("high", "low"), ..., main = NULL) {
  if(!is_edibble(.edibble)) {
    abort("Don't know how to plot an edibble table with no design.")
  }
  plot(get_edibble_design(.edibble))
  invisible(.edibble)
}


# interactive plot --------------------------------------------------------

#' Interactive plot of the graph of an edibble design
#'
#' @description
#'
#' This is similar to `plot.EdibbleDesign()` except it is an interactive version
#' using `igraph::tkplot()` behind the scenes. Unlike `plot.EdibbleDesign()`,
#' the default view is "low".
#'
#' @param .edibble An edibble desgin, an edibble table or an edibble graph.
#' @param ... Additional parameters that are passed into `igraph::tkplot`.
#' @param width The width of the canvas. Same as the `canvas.width` in `tkplot`.
#' @param height The height of the canvas. Same as the `canvas.height` in `tkplot`.
#' @importFrom igraph tkplot
#' @seealso See [plot.edbl_graph()] for static plot of the graph of an edibble design.
#' @export
iplot <- function(.edibble, ...) {
  UseMethod("iplot")
}

#' @rdname iplot
#' @export
iplot.EdibbleDesign <- function(.edibble, view = c("low", "high"), ...,
                                width = 450, height = 450) {
  iplot(get_edibble_graph(.edibble),
        view = view, ...,
        width = width, height = height)
  invisible(.edibble)
}

#' @rdname iplot
#' @export
iplot.edbl_graph <- function(.edibble, view = c("low", "high"), ...,
                             width = 450, height = 450) {
  view <- match.arg(view)
  out <- switch(view,
                high = subset_vars(.edibble),
                low = subset_levels(.edibble))
  tkplot(out, ...,
         canvas.width = width, canvas.height = height)
  invisible(.edibble)
}

#' @rdname iplot
#' @export
iplot.edbl_table <- function(.edibble, view = c("low", "high"), ...,
                             width = 450, height = 450) {
  if(!is_edibble(.edibble)) {
    abort("Don't know how to plot an edibble table with no design.")
  }
  iplot(get_edibble_design(.edibble),
        view = view, ...,
        width = width, height = height)
  invisible(.edibble)
}


