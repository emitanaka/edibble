
#' Initiate the edibble design
#'
#' @description
#' This function doesn't really do much besides create an empty igraph.
#'
#' @param name Optional name used as title for printing the design.
#' @return An `edbl_graph` object which is a special type of `igraph` class.
#' @examples
#' initiate_design("My design")
#' @seealso Add variables to this design with [set_units()], [set_trts()], and
#' [measure_units()].
#'
#' @export
start_design <- function(name = NULL) {
  EdibbleDesign$new(name = name)
  #name <- name %||% "An edibble design"
  #out <- igraph::set_graph_attr(igraph::make_empty_graph(), "name", name)
  #structure(out, class = c("edbl_graph", class(out)))
}

#' @export
initiate_design <- function(name = NULL) {
  start_design(name = name)
}

#' Print edibble graph to terminal
#'
#' @description
#' This function prints an `edbl_graph` object as a tree to terminal.
#' The variables are color coded (or decorated) with the given options.
#' Any ANSI coloring or styling are only visible in the console or terminal
#' outputs that support it. The print output is best used interactively since
#' any text styling are lost in text or R Markdown output. More details can
#' be found in `vignette("edbl-output")`.
#'
#' @param .data An edibble graph.
#' @param decorate_trts,decorate_units,decorate_resp,decorate_levels,decorate_title
#' A function applied to the name of treatment, unit, response factors or
#' design title. The function should return a string. Most often this wraps the name with
#' ANSI colored text. Run [edibble_opt()] to see the list of default
#' values for the options.
#'
#' @examples
#' # stylizing are only visible in terminal output that supports it
#' print(nclassics$split)
#' ## Split plot design
#' ## ├─mainplot (4 levels)
#' ## │ └─subplot (8 levels)
#' ## ├─subplot (8 levels)
#' ## ├─variety (2 levels)
#' ## └─irrigation (2 levels)
#' @importFrom rlang %||%
#'
#' @export
print.edbl_graph <- function(.data,
                             decorate_units  = edibble_decorate("units"),
                             decorate_trts   = edibble_decorate("trts"),
                             decorate_resp   = edibble_decorate("resp"),
                             decorate_levels = edibble_decorate("levels"),
                             decorate_main  = edibble_decorate("main"),
                             main = NULL) {

  main <- main %||% igraph::graph_attr(.data, "name") %||% "An edibble design"
  vgraph <- subset_vars(.data)
  gnames <- V(vgraph)$name

  if(is_empty(gnames)) {
    data <- data.frame(var = "root", child = NA,
                       label = as.character(decorate_main(main)))
  } else {

    classes <- V(vgraph)$class
    label_names <- decorate_vars(gnames,
                                 decorate_units,
                                 decorate_trts,
                                 decorate_resp,
                                 classes)


    var_nlevels <- lengths(vars_levels(.data, gnames))
    nvar <- length(gnames)
    ll <- lapply(V(vgraph),
              function(v) {
                  class <- igraph::vertex_attr(vgraph, "class", v)
                  children <- igraph::neighbors(vgraph, v, mode = "out")
                  if(class!="edbl_trt" & !is_empty(children)) {
                    gnames[children]
                  } else {
                    character()
                  }
              })

    data <- data.frame(var = c("root", gnames),
                       child = I(c(list(gnames), ll)),
                       label = c(decorate_main(main),
                                 paste(label_names, map_chr(var_nlevels, decorate_levels))))
  }
  cat(cli::tree(data, root = "root"), sep = "\n")
}




#' Test if the object is an edibble data frame
#'
#' A simple function that return `TRUE` if the object inherits
#' the `edbl_df` class, `FALSE` otherwise.
#'
#' @param x An object.
#' @return `TRUE` if the object inherits the `edbl_df` class.
#' @seealso See [is_edibble_graph()] for testing if the object is `edbl_graph`.
#' @export
is_edibble <- function(x) {
  inherits(x, "edbl_df")
}

#' Test if the object is an edibble graph
#'
#' A simple function that return `TRUE` if the object inherits
#' the `edbl_graph` class, `FALSE` otherwise.
#'
#' @inheritParams is_edibble
#' @return `TRUE` if the object inherits the `edbl_graph` class.
#' @seealso See [is_edibble()] for testing `edbl_df` data frame.
#'
#' @export
is_edibble_graph <- function(x) {
  inherits(x, "edbl_graph")
}


#' Meta information print out for edibble
#'
#' @param .data An edibble data frame.
#' @importFrom tibble tbl_sum
#' @export
tbl_sum.edbl_df <- function(.data) {
  head_meta <- c("An edibble" = dim_desc(.data))
  head_meta
}

#' Make input edibble
#'
#' @description
#' **(WIP)** If variables are already defined as external data then you can import this
#' data as edibble.
#'
#' @param .data A named list of edibble variables or data frame.
#' @param ... Passed to `new_tibble`.
#' @param units A character vector.
#' @param trts A character vector.
#' @seealso See [start_design()] for initiating design from scratch.
#'
#' @export
edibble <- function(.data, ..., units = NULL, trts = NULL) {
  new_edibble(.data, ...) #%>%
    #set_units(units) %>%
    #set_trts(trts)
}

#' An edibble constructor
#'
#' @description
#'
#' @param .data data frame or list of the same size.
#' @param ... Passed to `new_tibble`.
#' @param graph An edibble graph object.
#' @param class Subclasses for edibble. The default is NULL.
#'
#' @importFrom vctrs vec_size_common
#' @importFrom rlang !!!
#'
#' @export
new_edibble <- function(.data, ..., graph = NULL, class = NULL) {
  tibble::new_tibble(.data, ..., nrow = vec_size_common(!!!.data),
                     class = "edbl_df", graph = graph)
}


#' @rdname is_edibble
#' @export
not_edibble <- function(x) {
  if (!is_edibble(x)) {
    abort(sprintf("%s is not edibble.", deparse(substitute(x))))
  }
}
