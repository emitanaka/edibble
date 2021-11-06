


decorate_vars <- function(x, decorate_units, decorate_trts, decorate_resp, classes) {
  edbl_classes <- c("edbl_unit", "edbl_trt", "edbl_resp")
  decorate_fns <- list(decorate_units, decorate_trts, decorate_resp)
  for(i in seq_along(edbl_classes)) {
    index <- which(classes==edbl_classes[i])
    if(length(index) > 0) {
      x[index] <- decorate_fns[[i]](x[index])
    }
  }
  x
}


#' Print intermediate experimental design to terminal
#'
#' @description
#' This function prints an `edbl_graph` object as a tree to terminal.
#' The variables are color coded (or decorated) with the given options.
#' Any ANSI coloring or styling are only visible in the console or terminal
#' outputs that support it. The print output is best used interactively since
#' any text styling are lost in text or R Markdown output. More details can
#' be found in `vignette("edbl-output", package = "edibble")`.
#'
#' @param x An edibble graph.
#' @param decorate_trts,decorate_units,decorate_resp,decorate_levels,decorate_title
#' A function applied to the name of treatment, unit, response factors or
#' design title. The function should return a string. Most often this wraps the name with
#' ANSI colored text. Run [edibble_opt()] to see the list of default
#' values for the options.
#' @param title The title of the design.
#' @param ... Unused.
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
#' @importFrom cli tree cli_li
#' @export
print.edbl_design <- function(x,
                              decorate_units  = edibble_decorate("units"),
                              decorate_trts   = edibble_decorate("trts"),
                              decorate_resp   = edibble_decorate("resp"),
                              decorate_levels = edibble_decorate("levels"),
                              decorate_title  = edibble_decorate("title"),
                              title = NULL, ...) {
  title <- title %||% x$name
  vnames <- x$vgraph$nodes$label

  if(is_empty(vnames)) {
    data <- data.frame(var = "root",
                       child = NA,
                       label = as.character(decorate_title(title)))
  } else {

    classes <- x$vgraph$nodes$class
    label_names <- decorate_vars(vnames,
                                 decorate_units,
                                 decorate_trts,
                                 decorate_resp,
                                 classes)

    var_nlevels <- lengths(vlevels(x)[vnames])
    nvar <- length(vnames)
    ll <- lapply(vnames,
                 function(v) {
                   id <- vid(x$vgraph, v)
                   class <- vclass(x$vgraph, id = id)
                   children <- vchild(x$vgraph, id = id)
                   if(class!="edbl_trt" & !is_empty(children)) {
                     vlabel(x$vgraph, id = children)
                   } else {
                     character()
                   }
                 })
    nodes_with_parents <- unname(unlist(ll))

    data <- data.frame(var = c("root", vnames),
                       child = I(c(list(setdiff(vnames, nodes_with_parents)), ll)),
                       label = c(decorate_title(title),
                                 paste(label_names, map_chr(var_nlevels, decorate_levels))))
  }
  cat(tree(data, root = "root"), sep = "\n")
  if(!is_null(x$allotment)) {
    cat(decorate_title("Allotment:\n"))
    s <- as.character(x$allotment)
    tilde_pos <- unlist(gregexpr("~", s))
    tilde_pos_max <- max(tilde_pos)
    pad <- map_chr(tilde_pos_max - tilde_pos, function(n) ifelse(n==0, "", paste0(rep(" ", n), collapse = "")))
    cli_li(items = paste0(" ", pad, s))
  }
  if(!is_null(x$assignment)) {
    cat(decorate_title("Assignment:"), x$assignment, "\n")
  }
  if(!is_null(x$validation)) {
    cat(decorate_title("Validation:\n"))
    rnames <- names(x$validation)
    items <- map_chr(seq_along(x$validation), function(i) {
      paste0(rnames[i], ": ", style_italic(x$validation[[i]]$record))
    })
    cli_li(items = items)
  }
}

#' @export
print.edbl_graph <- function(x, ...) {
  cat(cli::col_green("nodes\n"))
  print(x$nodes)
  cat(cli::col_green("edges\n"))
  print(x$edges)
}

names.edbl_graph <- function(.graph) {
  .graph$nodes$label
}

names.edbl_design <- function(.design) {
  names(.design$vgraph)
}


remove_nulls <- function(.x) {
  .x[!vapply(.x, is.null, logical(1))]
}


compact <- function(.x) {
  .x[!vapply(.x, is_empty, logical(1))]
}


#' Find how many digits
ndigits <- function(x) {
  max(c(floor(log10(abs(x))) + 1, edibble_labels_opt("min_ndigits")))
}

#' Count of child units per parent unit
#' @param .edibble An edibble design or graph
#' @param parent A string with the name of the parent.
#' @param child A string with the child name.
#' @export
table_units <- function(.edibble, parent, child) {
  graph <- get_edibble_graph(.edibble)
  vgraph <- delete_edges(graph, which(E(graph)$etype!="l2l"))
  # [FIXME] shouldn't match based on prefix to find parent and child level nodes
  parent_names <- V(graph)$name[grepl(paste0(parent, ":"), V(graph)$name)]
  child_names <- V(graph)$name[grepl(paste0(child, ":"), V(graph)$name)]
  count <- setNames(rep(0, length(parent_names)), parent_names)
  for(aparent in parent_names) {
    nv <- neighbors(vgraph, V(vgraph)$name==aparent, mode = "out")
    count[aparent] <- length(nv)
  }
  count
}


is_nested_unit <- function(design, uid) {
  unit_ids <- subset(vgraph(design)$nodes, class == "edbl_unit")$id
  out <- subset(vgraph(design)$edges, to %in% uid & from %in% unit_ids)
  nrow(out) > 0
}


# number SI prefix --------------------------------------------------------

#' Numbers with SI prefix
#'
#' It's called SI prefix but the letter notation is added as a suffix.
#' The largest prefix is yotta $10^{24}$.
#'
#' @param x A numeric vector to format with SI prefix
#' @source https://en.wikipedia.org/wiki/Metric_prefix
number_si_prefix <- function(x) {
  if(!all(x >= 1)) abort("The numeric vector should be a positive integer.")

  prefix <- c(1, 10^c(k = 3, M = 6, G = 9, T = 12, P = 15, E = 18, Z = 21, Y = 24))
  map_chr(x, function(n) {
    index <- max(which(n/prefix >= 1))
    scale <- 1 / prefix[index]
    symbol <- names(prefix)[index]
    paste0(floor(n * scale), symbol)
  })
}


# inspired by knitr::combine_words
.combine_words <- function(words, sep = ", ", and = " and ",
                           before = "", after = before, fun = function(x) x) {

  words <- fun(paste0(before, words, after))
  n <- length(words)
  if(n <= 1) {
    return(words)
  }

  words[n - 1] = paste0(words[n - 1], and, words[n])
  paste0(words[-n], collapse = sep)
}
