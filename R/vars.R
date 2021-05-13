#' Set edibble variables
#'
#' @description
#' Adds variable and their level nodes in an edibble graph.
#'
#' @inheritParams set_units
#' @param .name_repair Specify how to deal when there are duplicated name
#' entries. The repair follows the same argument in [tibble::tibble()].
#' @seealso [set_units()] and [set_trts()] for setting special types of nodes.
#' @importFrom vctrs vec_as_names
#' @importFrom rlang quo_get_expr have_name expr
#' @importFrom cli col_grey
#' @importFrom tidyselect eval_select
set_vars <- function(.edibble, ..., .class = NULL,
                     .name_repair = c("check_unique", "unique", "universal", "minimal")) {

  not_edibble(.edibble)

  .name_repair <- match.arg(.name_repair)
  .design <- get_edibble_design(.edibble)
  type <- switch(.class,
                 edbl_unit = "unit",
                 edbl_trt = "trt",
                 "default")
  attr <- vertex_attr_opt(type)

  if(is_edibble_design(.edibble)) {
    dots <- enquos(..., .named = TRUE, .homonyms = "error")
    vnames_new <- names(dots)
    vnames_old <- names_vars(.design)
    vnames <- vec_as_names(c(vnames_old, vnames_new), repair = .name_repair)
    for(i in seq_along(dots)) {
      vname <- vnames[i + length(vnames_old)]
      value <- eval_rhs_attr(dots[[i]], vname, .design)
      .design$add_variable(value, vname, attr)
    }

  } else if(is_edibble_table(.edibble)) {
    .table <- get_edibble_table(.edibble)
    dots <- enquos(..., .named = TRUE)

    loc <- eval_select(expr(!!names(dots)), .table)
    for(i in seq_along(loc)) {
      var <- .table[[loc[i]]]
      lvls <- unique(as.character(var))
      vname <- names(loc)[i]
      .table[[loc[i]]] <- new_edibble_var(labels = as.character(var),
                                         levels = lvls,
                                         name = vname,
                                         class = .class)
      .design$add_variable(lvls, vname, attr)
      # adding nesting

    }
    .edibble <- .table
  }

  update_design(.edibble, .design)
}

eval_rhs_attr <- function(.x, vname, .design = NULL) {
  q <- quo_get_expr(.x)
  # At the moment if it fails, this is because
  # it's `nested_in`.
  # it adds the extra bit needed to make sure nested works
  tryCatch(eval(q), error = function(.error) {
    vlevels <- vars_levels(.design$graph)
    q$.vname <- vname
    eval_tidy(q, data = vlevels)
  })
}

#' Add an edibble vertex
#'
#' @description
#' In edibble, a short hand is used to assign new edibble variables/nodes.
#' This function converts the short hand to make the minimal edibble node
#' and returns an edibble graph with that additional node.
#'
#' @name add_edibble_vertex
#' @return Returns an evaluated expression.
add_edibble_vertex <- function(.data, ...) {
  UseMethod("add_edibble_vertex")
}

#' @param value A value that may be a single number, unnamed vector,
#' a one sided formula or linkabble.
#' @param name Name of the edibble variable as string.
#' @param design An edibble graph with levels converted as list used
#' for evaluation in any expressions.
#' @rdname add_edibble_vertex
#' @export
add_edibble_vertex.default <- function(value, name, design = NULL, attr) {
  type <- get_value_type(value)
  levels <- switch(type,
                   "numeric" = traits_levels(prefix = name, size = value),
                   "unnamed_vector" = value,
                   "named_vector" = value,
                   "formula" = {
                     vlevels <- design$vars_levels()
                     tt <- terms(value)
                     vars <- rownames(attr(tt, "factor"))
                     n <- prod(lengths(vlevels[vars]))
                     traits_levels(prefix = name, size = n)},
                   "unimplemented" = character())

  out <- add_edibble_vertex_common(design$graph, name, levels, attr)
  # extra modifications
  # switch(type,
  #        formula = {
  #          tt <- terms(value)
  #          vars <- rownames(attr(tt, "factor"))
  #          out <- out %>%
  #            add_edges(cross_edge_seq(vars, name))
  #        })

  class(out) <- class(design$graph)
  return(out)
}

#' @rdname add_edibble_vertex
#' @importFrom igraph graph_attr delete_graph_attr add_edges delete_vertex_attr
#' @export
add_edibble_vertex.lkbl_graph <- function(.lgraph, name, design = NULL, attr) {
  levels <- gsub(paste0(name, ":"), "", V(.lgraph)$name[V(.lgraph)$ltype=="child"])
  #.data <- igraph::set_vertex_attr(.data, "name", value = V(.data)$name)

  agraph <- add_edibble_vertex_common(design$graph, name, levels, attr)

  child_name <- name
  parent_name <- graph_attr(.lgraph, "parent_name")
  lgraph <- delete_graph_attr(.lgraph, "parent_name")
  lgraph <- delete_vertex_attr(lgraph, "ltype")

  cgraph <- combine_graphs(agraph, lgraph)
  out <- add_edges(cgraph,
                   cross_edge_seq(cgraph, parent_name, child_name),
                   attr = edge_attr_opt("v2v"))

  return(reinstate_graph_attrs(out, design$graph))
}


#' Helper to set the traits for edibble variable
#'
#' @description
#' This explicitly defines `edbl_var`.
#'
#' @param labels A character vector. If `levels` is supplied then the all
#'   all elements should appear in `levels`.
#' @param levels A character vector with the name of the levels or
#'   a named vector where the names are the name of the levels and the entries
#'   are the number of replicate for the corresponding level.
#' @param n,nlevels The number of levels.
#' @param name The name of the object stored in the traits.
#' @param prefix The prefix to add the names of the levels.
#'   Ignored if `levels` is used.
#' @param rep The number of available replicate for each level. If undefined,
#'   the number of replicate is assumed to be infinite.
#'
#' @param ... Passed to `new_edibble_var`.
#' @param class The subclass name.
#' @examples
#' traits(n = 10, name = "plot")
#' traits(labels = c("vaccine", "control", "vaccine"),
#'        levels = c("vaccine", "control"))
#' @return
#' Returns an `edbl_var` object.
#' @export
traits <- function(labels = character(), levels = unique(labels), n = NULL,
                   name = NULL, prefix = name,
                   nlevels = n, rep = NULL, ..., class = NULL) {
  prefix <- prefix %||% ""
  if(is_null(n) & !is_null(nlevels)) n <- nlevels
  if(length(levels)==0) {
    levels <- traits_levels(prefix = prefix, size = n)
  } else {
    if(is_named(levels)) {
      rep <- unname(levels)
      levels <- names(levels)
    }
  }
  if(is_null(rep)) rep <- rep(Inf, length(levels))

  new_edibble_var(labels = labels, levels = levels,
                  name = name, rep = rep, ..., class = class)
}


#' Constructor for an edibble variable
#' @importFrom vctrs new_vctr
new_edibble_var <- function(labels = character(), levels = unique(labels),
                            name = character(), rep = NULL, ..., class = NULL) {
  x <- new_vctr(labels, levels = levels, name = name,
                rep = rep, ..., class = "edbl_var")
  class(x) <- c(class, class(x))
  x
}




var_class <- function(.graph, vname) {
  if(missing(vname)) {
    V(.graph)$class
  } else {
    V(.graph)$class[var_index(.graph, vname)]
  }
}

var_index <- function(.graph, vname, var = FALSE) {
  if(var) {
    which(V(.graph)$vname %in% vname)
  } else {
    which(V(.graph)$name %in% vname)
  }
}

#' @export
var_names <- function(.graph, vindex) {
  if(any(w <- V(.graph)$vtype[vindex]!="var")) {
    abort(paste0("The vertex index ", vindex[w], " is not an edibble variable node."))
  }
  V(.graph)$name[vindex]
}

var_levels <- function(.graph, vname, label = FALSE) {
  sgraph <- subset_levels(.graph)
  vindex <- var_index(sgraph, vname, var = TRUE)
  if(label) {
    set_names(V(sgraph)$label[vindex], V(sgraph)$name[vindex])
  } else {
    V(sgraph)$name[vindex]
  }
}

vars_levels <- function(.graph, vnames) {
  if(missing(vnames)) {
    vnames <- names_vars(.graph)
  }
  structure(lapply(vnames, function(vname) var_levels(.graph, vname)),
            names = vnames)
}

vertex_var_label <- function(name, nlevels) {
  paste0(name, "\n(", nlevels, " levels)")
}

vertex_level_names <- function(vname, lnames) {
  paste0(vname, ":", lnames)
}

#' @importFrom igraph add_vertices add_edges
add_edibble_vertex_common <- function(graph, name, levels, attr) {
  label <-  vertex_var_label(name, length(levels))
  lnames <- vertex_level_names(name, levels)
  var_node_added <- add_vertices(graph, 1,
                                         name = name,
                                         vtype = "var", vname = name,
                                         label = label,
                                         attr = attr)
  lvl_nodes_added <- add_vertices(var_node_added, length(levels),
                                          name = lnames,
                                          vtype = "level", vname = name,
                                          label = unname(levels),
                                          value = names(levels),
                                          attr = attr)
  var2lvl_edges_added <- add_edges(lvl_nodes_added,
                                           cross_edge_seq(lvl_nodes_added, name, lnames),
                                           attr = edge_attr_opt("v2l"))
  add_edges(var2lvl_edges_added,
                    path_seq(var2lvl_edges_added, lnames),
                    attr = edge_attr_opt("l2lseq"))

}

#' @importFrom igraph union
combine_graphs <- function(g1, g2) {
  cgraph <- union(g1, g2, byname = TRUE)
  # union has issues combining attributes
  for(comp in c("graph", "edge", "vertex")) {
    fnames <- getFromNamespace(paste0(comp, "_attr_names"), "igraph")
    fattr <- getFromNamespace(paste0(comp, "_attr"), "igraph")
    dattr <- getFromNamespace(paste0("delete_", comp, "_attr"), "igraph")
    sattr <- getFromNamespace(paste0("set_", comp, "_attr"), "igraph")
    nattrs <- union(fnames(g1), fnames(g2))
    cattrs <- fnames(cgraph)
    fix_names <- nattrs[!(nattrs %in% cattrs)]
    for(aname in fix_names) {
      cattr1 <- fattr(cgraph, paste0(aname, "_1"))
      cattr2 <- fattr(cgraph, paste0(aname, "_2"))
      # is the function vectorised?
      cgraph <- dattr(cgraph, paste0(aname, "_1"))
      cgraph <- dattr(cgraph, paste0(aname, "_2"))
      cattr1[is.na(cattr1)] <- cattr2[is.na(cattr1)]
      cgraph <- sattr(cgraph, aname, value = cattr1)
    }
  }
  cgraph
}


# Evaluate input value type
#' @importFrom rlang is_formula
get_value_type <- function(x) {
  if(is.numeric(x) && length(x)==1) return("numeric")
  if(is.vector(x) && !is_named(x)) return("unnamed_vector")
  if(is.vector(x) && is_named(x)) return("named_vector")
  if(is_formula(x, lhs = FALSE)) return("formula")
  return("unimplemented")
}



#' Utility functions for edibble variable
#'
#' @description
#' The S3 methods for `edbl_var` objects have
#' the same expected output that of a factor.
#'
#' Other functions are utility functions related to `edbl_var` object.
#'
#' @param x An `edbl_var` object.
#' @param ... Ignored.
#'
#' @name utility-edibble-var
#' @export
as.character.edbl_var <- function(x, ...) {
  #unname(levels(x)[x])
  out <- unclass(x)
  attributes(out) <- NULL
  out
}

#' @rdname utility-edibble-var
#' @export
as.integer.edbl_var <- function(x, ...) {
  out <- as.integer(as.factor(as.character(x)))
  attributes(out) <- NULL
  out
}

#' @export
levels.edbl_unit <- function(x) {
  attr(x, "levels")
}

#' @export
levels.edbl_trt <- function(x) {
  attr(x, "levels")
}

#' @rdname utility-edibble-var
#' @export
is_edibble_var <- function(x) {
  inherits(x, "edbl_var")
}
