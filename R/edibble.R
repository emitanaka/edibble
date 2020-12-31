
#' Initiate the edibble design
#'
#' @description
#' This function doesn't really do much besides create an empty igraph.
#'
#' @param name Optional name used as title for printing the design.
#' @return An `edbl_nexus` object which is a special type of `igraph` class.
#' @examples
#' initiate_design("My design")
#' @seealso Add variables to this design with [set_units()], [set_trts()], and
#' [measure_units()].
#'
#' @export
initiate_design <- function(name = NULL) {
  name <- name %||% "An edibble design"
  out <- igraph::set_graph_attr(igraph::make_empty_graph(), "name", name)
  class(out) <- c("edbl_nexus", class(out))
  out
}

#' Print edibble nexus to terminal
#'
#' @description
#' This function prints an `edbl_nexus` object as a tree to terminal.
#' The variables are color coded (or decorated) with the given options.
#' Any ANSI coloring or styling are only visible in the console or terminal
#' outputs that support it. The print output is best used interactively since
#' any text styling are lost in text or R Markdown output. More details can
#' be found in `vignette("edbl-output")`.
#'
#' @param .nexus An edibble nexus.
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
#'
#' @export
print.edbl_nexus <- function(.nexus,
                             decorate_units  = edibble_decorate("units"),
                             decorate_trts   = edibble_decorate("trts"),
                             decorate_resp   = edibble_decorate("resp"),
                             decorate_levels = edibble_decorate("levels"),
                             decorate_main  = edibble_decorate("main"),
                             main = NULL) {
  if(is_null(main)) {
    main <- igraph::graph_attr(.nexus, "name") %||% "An edibble design"
  }
  vnexus <- igraph::induced_subgraph(.nexus, V(.nexus)$vtype=="var")
  vnames <- V(vnexus)$name
  if(is_null(vnames)) {
    data <- data.frame(var = "root", child = NA,
                       label = as.character(decorate_main(main)))
  } else {

    classes <- V(vnexus)$class
    label_names <- decorate_vars(vnames,
                                 decorate_units,
                                 decorate_trts,
                                 decorate_resp,
                                 classes)


    var_nlevels <- lengths(lapply(vnames, function(x) var_levels(.nexus, x)))
    nvar <- length(vnames)
    ll <- lapply(V(vnexus),
              function(v) {
                  class <- igraph::vertex_attr(vnexus, "class", v)
                  children <- igraph::neighbors(vnexus, v, mode = "out")
                  if(class!="edbl_trt" & length(children) > 0) {
                    vnames[children]
                  } else {
                    character()
                  }
              })
    data <- data.frame(var = c("root", vnames),
                       child = I(c(list(vnames), ll)),
                       label = c(decorate_main(main),
                                 paste(label_names, map_chr(var_nlevels, decorate_levels))))
  }
  cat(cli::tree(data, root = "root"), sep = "\n")
}






#' Test if the object is edibble
#'
#' A simple function that return `TRUE` if the object inherits
#' the `edbl` class, `FALSE` otherwise.
#'
#' @param x An object.
#' @return `TRUE` if the object inherits the `edbl` class.
#' @seealso See [is_edibble_nexus()] for testing `edbl_nexus`.
#' @export
is_edibble <- function(x) {
  inherits(x, "edbl_df")
}

#' Test if the object is an edibble nexus
#'
#' A simple function that return `TRUE` if the object inherits
#' the `edbl_nexus` class, `FALSE` otherwise.
#'
#' @inheritParams is_edibble
#' @return `TRUE` if the object inherits the `edbl_nexus` class.
#' @seealso See [is_edibble()] for testing `edbl` data frame.
#'
#' @export
is_edibble_nexus <- function(x) {
  inherits(x, "edbl_nexus")
}


#' Meta information print out for edibble
#' @importFrom tibble tbl_sum
#' @export
tbl_sum.edbl_df <- function(x) {
  head_meta <- c("An edibble" = dim_desc(x))
  head_meta
}

#' Make input edibble
#'
#' @description
#' If variables are already defined as external data then you can import this
#' data as edibble. T
#'
#' @param x A named list of edibble variables or data frame.
#' @param ... Passed to `new_tibble`.
#' @param units A character vector.
#' @seealso See [initiate_design()] for initiating design from scratch.
#' @return A data frame of class `edbl`.
#'
#' @export
edibble <- function(x, ..., units = NULL, trts = NULL) {
  new_edibble(x, ...) #%>%
    #set_units(units) %>%
    #set_trts(trts)
}

#' @export
new_edibble <- function(x, ..., nexus = NULL, class = NULL) {
  tibble::new_tibble(x, ..., nrow = vec_size_common(!!!x),
                     class = "edbl_df", nexus = nexus)
}

#' @export
not_edibble <- function(x) {
  if (!is_edibble(x)) {
    abort(sprintf("%s is not edibble.", deparse(substitute(x))))
  }
}
