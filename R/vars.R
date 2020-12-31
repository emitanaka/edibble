#' Set edibble variables
#'
#' @description
#' A node in an edibble nexus becomes a variable when served.
#'
#' @param .nexus,.data An `edbl_nexus` or `edbl_df` object.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]><[`tidy-select`][dplyr::dplyr_tidy_select]>
#' Name-value pair.
#' @param .name_repair Specify how to deal when there are duplicated name
#' entries. The repair follows the same argument in [tibble::tibble()].
#' @seealso [set_units()] and [set_trts()] for setting special types of nodes.
#' @name set_vars
NULL

#' @rdname set_vars
#' @param .attr Named list of node attributes.
#' This is mainly used to control the plotting parameters.
#' See list of plot parameters [here](https://igraph.org/r/doc/plot.common.html).
#' @export
set_vars <- function(.data, ...) {
  UseMethod("set_vars")
}


#' @export
set_vars.edbl_nexus <- function(.nexus, ..., .class = "edbl_var",
                                .name_repair = c("check_unique", "unique", "universal", "minimal")) {
  .name_repair <- match.arg(.name_repair)
  dots <- enquos(...)
  vnames_new <- names(dots)
  vnexus <- subset_vars(.nexus)
  vnames_old <- names(vnexus)
  vnames <- vec_as_names(c(vnames_old, vnames_new), repair = .name_repair)
  attr <- vertex_attr_opt(gsub("edbl_", "", .class))

  out <- .nexus
  for(i in seq_along(dots)) {
    vname <- vnames[i + length(vnames_old)]
    x <- eval_traits(dots[[i]], vname, out)
    out <- add_edibble_vertex(x, vname, out, attr)
  }

  # since igraph operations seem to wipe out class info
  structure(out, class = class(.nexus))
}

#' @export
set_vars.edbl_df <- function(.data, ..., .attr = NULL,
                             .name_repair = c("check_unique", "unique", "universal", "minimal")) {

  .name_repair <- match.arg(.name_repair)
  dots <- enquos(...)


  dots_names <- names(dots)
  vnames <- vec_as_names(c(names(.data), dots_names), repair = .name_repair)

  loc <- tidyselect::eval_select(expr(c(...)), .data)


  names(out) <- vnames
  out
}



var_class <- function(.nexus, vname) {
  V(.nexus)$class[var_index(.nexus, vname)]
}

var_index <- function(.nexus, vname, var = FALSE) {
  if(var) {
    which(V(.nexus)$vname %in% vname)
  } else {
    which(V(.nexus)$name %in% vname)
  }
}

#' @export
var_names <- function(.nexus, vindex) {
  if(any(w <- V(.nexus)$vtype[vindex]!="var")) {
    abort(glue::glue("The vertex index {vindex[w]} is not an edibble variable node."))
  }
  V(.nexus)$name[vindex]
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
new_edibble_var <- function(labels = character(), levels = unique(labels),
                            name = character(), rep = NULL, ..., class = NULL) {
  x <- new_vctr(labels, levels = levels, name = name,
                rep = rep, ..., class = "edbl_var")
  class(x) <- c(class, class(x))
  x
}


#' Evaluate traits
#'
#' @param x A value that may be a single number, unnamed vector,
#' a one sided formula or linkabble.
#' @param vname The name of the variable.
#' @param nexus An edibble nexus with levels converted as list used
#' for evaluation in any expressions.
#' @return Returns an evaluated expression.
eval_traits <- function(x, vname, nexus = NULL) {
  lvls <- ed_levels(nexus)
  q <- quo_get_expr(x)
  tryCatch(eval(q), error = function(.error) {
    q$.vname <- vname
    eval_tidy(q, data = lvls)
  })
}

#' Add an edibble vertex
#'
#' @description
#' In edibble, a short hand is used to assign new edibble variables/nodes.
#' This function converts the short hand to make the minimal edibble node
#' and returns an edibble nexus with that additional node.
#'
#' @name add_edibble_vertex
#' @return Returns an evaluated expression.
#' @export
add_edibble_vertex <- function(.nexus, ...) {
  UseMethod("add_edibble_vertex")
}

vertex_var_label <- function(name, nlevels) {
  paste0(name, "\n(", nlevels, " levels)")
}

vertex_level_names <- function(vname, lnames) {
  paste0(vname, ":", lnames)
}

add_edibble_vertex_common <- function(.nexus, name, levels, attr) {
  label <-  vertex_var_label(name, length(levels))
  lnames <- vertex_level_names(name, levels)
  var_node_added <- igraph::add_vertices(.nexus, 1,
                                         name = name,
                                         vtype = "var", vname = name,
                                         label = label,
                                         attr = attr)
  lvl_nodes_added <- igraph::add_vertices(var_node_added, length(levels),
                                          name = lnames,
                                          vtype = "level", vname = name,
                                          label = levels,
                                          attr = attr)
  var2lvl_edges_added <- igraph::add_edges(lvl_nodes_added,
                                           cross_edge_seq(lvl_nodes_added, name, lnames),
                                           attr = edge_attr_opt("v2l"))
  igraph::add_edges(var2lvl_edges_added,
                    path_seq(var2lvl_edges_added, lnames),
                    attr = edge_attr_opt("l2lseq"))

}

combine_graphs <- function(.nexus1, .nexus2) {
  cnexus <- igraph::union(.nexus1, .nexus2, byname = TRUE)
  # union has issues combining attributes
  for(comp in c("graph", "edge", "vertex")) {
    fnames <- getFromNamespace(paste0(comp, "_attr_names"), "igraph")
    fattr <- getFromNamespace(paste0(comp, "_attr"), "igraph")
    dattr <- getFromNamespace(paste0("delete_", comp, "_attr"), "igraph")
    sattr <- getFromNamespace(paste0("set_", comp, "_attr"), "igraph")
    nattrs <- union(fnames(.nexus1), fnames(.nexus2))
    cattrs <- fnames(cnexus)
    fix_names <- nattrs[!(nattrs %in% cattrs)]
    for(aname in fix_names) {
      cattr1 <- fattr(cnexus, paste0(aname, "_1"))
      cattr2 <- fattr(cnexus, paste0(aname, "_2"))
      # is the function vectorised?
      cnexus <- dattr(cnexus, paste0(aname, "_1"))
      cnexus <- dattr(cnexus, paste0(aname, "_2"))
      cattr1[is.na(cattr1)] <- cattr2[is.na(cattr1)]
      cnexus <- sattr(cnexus, aname, value = cattr1)
    }
  }
  cnexus
}

#' @rdname add_edibble_vertex
#' @export
add_edibble_vertex.lkbl_nexus <- function(.lnexus, name, nexus = NULL, attr) {
  levels <- gsub(paste0(name, ":"), "", V(.lnexus)$name[V(.lnexus)$ltype=="child"])
  #.nexus <- igraph::set_vertex_attr(.nexus, "name", value = V(.nexus)$name)

  anexus <- add_edibble_vertex_common(nexus, name, levels, attr)

  child_name <- name
  parent_name <- igraph::graph_attr(.lnexus, "parent_name")
  lnexus <- igraph::delete_graph_attr(.lnexus, "parent_name")
  lnexus <- igraph::delete_vertex_attr(lnexus, "ltype")

  cnexus <- combine_graphs(anexus, lnexus)
  out <- igraph::add_edges(cnexus,
                           cross_edge_seq(cnexus, parent_name, child_name),
                           attr = edge_attr_opt("v2v"))

  class(out) <- class(nexus)
  return(out)
}

#' @param x A value that may be a single number, unnamed vector,
#' a one sided formula or linkabble.
#' @param name Name of the edibble variable as string.
#' @param nexus An edibble nexus with levels converted as list used
#' for evaluation in any expressions.
#' @rdname add_edibble_vertex
#' @export
add_edibble_vertex.default <- function(x, name, nexus = NULL, attr) {
  type <- get_value_type(x)
  levels <- switch(type,
                   "numeric" = traits_levels(prefix = name, size = x),
                   "unnamed_vector" = x,
                   "formula" = {
                     lvls <- ed_levels(nexus)
                     tt <- terms(x)
                     vars <- rownames(attr(tt, "factor"))
                     n <- prod(lengths(lvls[vars]))
                     traits_levels(prefix = name, size = n)},
                   "unimplemented" = character())

  out <- add_edibble_vertex_common(nexus, name, levels, attr)
  vindex <- which(V(out)$name==name)
  # extra modifications
  switch(type,
         formula = {
           tt <- terms(x)
           vars <- rownames(attr(tt, "factor"))
           out <- out %>%
             add_edges(cross_edge_seq(vars, name))
         })

  class(out) <- class(nexus)
  return(out)
}

#' Evaluate input value type
get_value_type <- function(x) {
  if(is.numeric(x) && length(x)==1) return("numeric")
  if(is.vector(x) && !is_named(x)) return("unnamed_vector")
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

#' @export
levels.edbl_unit <- function(x) {
  attr(x, "levels")
}

#' @export
levels.edbl_trt <- function(x) {
  attr(x, "levels")
}


# currently vname has to be character.
# [TODO] make NSE compatible
# unfortunately levels only accepts one argument so it doesn't work T_T
#' @export
var_levels <- function(.nexus, vname, label = FALSE) {
  snexus <- subset_levels(.nexus)
  vindex <- var_index(snexus, vname, var = TRUE)
  if(label) {
    set_names(V(snexus)$label[vindex], V(snexus)$name[vindex])
  } else {
    V(snexus)$name[vindex]
  }
}


#' @export
vars_levels <- function(.nexus, vnames) {
  structure(lapply(vnames, function(vname) var_levels(.nexus, vname)),
            names = vnames)
}


#' @rdname utility-edibble-var
#' @export
is_edibble_var <- function(x) {
  inherits(x, "edbl_var")
}


# Alias required for help links in downstream packages
#' @aliases select_helpers
#' @importFrom tidyselect contains
#' @export
tidyselect::contains
#' @importFrom tidyselect ends_with
#' @export
tidyselect::ends_with
#' @importFrom tidyselect everything
#' @export
tidyselect::everything
#' @importFrom tidyselect matches
#' @export
tidyselect::matches
#' @importFrom tidyselect num_range
#' @export
tidyselect::num_range
#' @importFrom tidyselect one_of
#' @export
tidyselect::one_of
#' @importFrom tidyselect starts_with
#' @export
tidyselect::starts_with
#' @importFrom tidyselect last_col
#' @export
tidyselect::last_col