#' Plot graph of edibble variables
#'
#' @param .nexus An edibble nexus.
#' @param view A string specifying the level of the view to plot.
#' @param ... Additional parameters passed to `plot.igraph`.
#' @param layout An igraph layout.
#' @examples
#' plot(.nexus, sample_levels(n = 30))
#'
#' @export
plot.edbl_nexus <- function(.nexus, view = c("high", "low"), ...,
                            layout = igraph::layout.auto,
                            main = NULL) {

  view <- match.arg(view)
  out <- switch(view,
                "high" = subset_vars(.nexus),
                "low"  = subset_levels(.nexus))

  if(is_null(main)) {
    main <- igraph::graph_attr(.nexus, "name") %||% "An edibble design"
  }

  igraph::plot.igraph(out, ..., annotate.plot = TRUE,
                      main = main,
                      layout = layout)
}


#' @importFrom ggplot2 autoplot ggplot facet_wrap geom_tile scale_fill_viridis_d coord_equal ylab labs aes theme
#' @export
autoplot.edbl <- function(.edbl, ...) {
  if(n_trt_vars(.edbl)==1 & n_unit_vars(.edbl)==1) {
    unit <- sym(get_unit_vars(.edbl))
    trt <- sym(get_trt_vars(.edbl))
    gg <- as.data.frame(.edbl) %>%
      ggplot2::ggplot(ggplot2::aes(x = "", y = !!unit, fill = !!trt)) +
      ggplot2::geom_tile(color = "black", size = 1.1) +
      ggplot2::coord_equal() +
      ggplot2::scale_fill_viridis_d() +
      ggplot2::labs(x = "", title = igraph::graph_attr(attr(.edbl, "nexus"), "name")) +
      ggplot2::theme(axis.ticks.length.x = unit(0, "pt"))
  }
  if(n_trt_vars(.edbl)==1 & n_unit_vars(.edbl)==2) {
    units <- syms(get_unit_vars(.edbl))
    trt <- sym(get_trt_vars(.edbl))
    gg <- as.data.frame(.edbl) %>%
      ggplot(aes(x = "", y = !!units[[2]], fill = !!trt)) +
      geom_tile(color = "black", size = 1.1) +
      scale_fill_viridis_d() +
      labs(x = "", title = graph_attr(attr(.edbl, "nexus"), "name")) +
      facet_wrap(units[[1]], scale = "free") +
      theme(axis.ticks.length.x = unit(0, "pt"))

  }

  gg
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
