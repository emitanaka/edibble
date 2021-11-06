#' Set edibble variables
#'
#' @description
#' Adds variable and their level nodes in an edibble graph.
#'
#' @inheritParams set_units
#' @param .class A class for the variables.
#' @seealso [set_units()] and [set_trts()] for setting special types of nodes.
#' @importFrom vctrs vec_as_names
#' @importFrom cli col_grey
#' @importFrom tidyselect eval_select
set_vars <- function(.edibble, ..., .class = NULL,
                     .name_repair = c("check_unique", "unique", "universal", "minimal")) {

  not_edibble(.edibble)

  .name_repair <- match.arg(.name_repair)

  if(is_edibble_design(.edibble)) {
    res <- edbl_design(.edibble)
    dots <- enquos(..., .named = TRUE, .homonyms = "error")
    vnames_new <- names(dots)
    vnames_old <- names(res)
    vnames <- vec_as_names(c(vnames_old, vnames_new), repair = .name_repair)
    vlev <- NULL
    for(i in seq_along(dots)) {
      vname <- vnames[i + length(vnames_old)]
      if(nrow(res$lgraph$nodes)) vlev <- vlevels(res)
      value <- eval_tidy(dots[[i]], data = c(vlev, list(.vname = vname)))
      res <- add_edibble_vertex(value, vname, .class, res)
    }

  } else if(is_edibble_table(.edibble)) {
    res <- .edibble
    dots <- enquos(..., .named = TRUE)

    loc <- eval_select(expr(!!names(dots)), res)
    for(i in seq_along(loc)) {
      var <- res[[loc[i]]]
      lvls <- unique(as.character(var))
      vname <- names(loc)[i]
      res[[loc[i]]] <- ...
      res <- add_edibble_vertex(lvls, vname, .class, res)
    }
  }

  res
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
add_edibble_vertex <- function(x, ...) {
  UseMethod("add_edibble_vertex")
}

#' @param value A value that may be a single number, unnamed vector,
#' a one sided formula or linkabble.
#' @param name Name of the edibble variable as string.
#' @param design An edibble graph with levels converted as list used
#' for evaluation in any expressions.
#' @rdname add_edibble_vertex
#' @importFrom tibble add_row
#' @export
add_edibble_vertex.default <- function(value, vname, class, design) {

  type <- var_value_type(value)
  levels <- switch(type,
                   "numeric" = traits(n = value, prefix = vname, class = class),
                   "unnamed_vector" = traits(labels = value, class = class),
                   "named_vector" = traits(labels = names(value),
                                           rep = unname(value),
                                           class = class),
                   "unimplemented" = traits(labels = character(), class = class))

  add_edibble_vertex.edbl_trait(levels, vname, class, design)
}

last_id <- function(graph) {
  ifelse(nrow(graph$nodes), max(graph$nodes$id), 0L)
}

add_edibble_vertex.edbl_trait <- function(value, vname, class, design) {
  out <- design
  vid <- last_id(out$vgraph) + 1L
  lid <- last_id(out$lgraph) + 1L
  attrs <- attributes(value)
  missing_cols <- setdiff(names(attrs), names(out$vgraph$nodes))
  out$vgraph$nodes[missing_cols] <- NA
  out$vgraph$nodes <- add_row(out$vgraph$nodes,
                              id = vid,
                              label = vname,
                              class = class,
                              !!!attrs[missing_cols])
  out$lgraph$nodes <- add_row(out$lgraph$nodes,
                              idvar = vid,
                              id = lid:(lid + length(value) - 1),
                              label = as.character(value))
  out
}

add_edibble_vertex.formula <- function(value, vname, class, design) {
  vlevels <- vlevels(design)
  tt <- terms(value)
  vars <- rownames(attr(tt, "factor"))
  pdf <- expand.grid(vlevels[vars])
  pdf[[vname]] <- traits(prefix = vname, n = nrow(pdf), class = class)
  out <- add_edibble_vertex.edbl_trait(pdf[[vname]], vname, class, design)
  idv <- subset(out$vgraph$nodes, label == vname)$id
  for(avar in vars) {
    idp <- subset(out$vgraph$nodes, label == avar)$id
    out$vgraph$edges <- add_row(out$vgraph$edges,
                                from = idp, to = idv)
    out$lgraph$edges <- add_row(out$lgraph$edges,
                                from = vid(out$lgraph, pdf[[avar]]),
                                to = vid(out$lgraph, pdf[[vname]]))
  }
  out
}

add_edibble_vertex.nst_levels <- function(value, vname, class, design) {
  out <- design
  idv <- last_id(out$vgraph) + 1L
  idl <- last_id(out$lgraph) + 1L
  parent <- value %@% "keyname"
  idp <- vid(design$vgraph, label = parent)
  attrs <- attributes(value)
  missing_cols <- setdiff(names(attrs), c(names(out$vgraph$nodes), "names", "keyname"))
  out$vgraph[missing_cols] <- NA
  out$vgraph$nodes <- add_row(out$vgraph$nodes,
                              id = idv,
                              label = vname,
                              class = class,
                              !!!attrs[missing_cols])
  out$vgraph$edges <- add_row(out$vgraph$edges,
                              from = idp, to = idv)
  plevels <- rep(names(value), lengths(value))
  clevels <- unname(unlist(value))
  out$lgraph$nodes <- add_row(out$lgraph$nodes,
                              idvar = idv,
                              id = idl:(idl + sum(lengths(value)) - 1),
                              label = clevels)
  pids <- vid(out$lgraph, plevels)
  vids <- vid(out$lgraph, clevels)
  # TODO: fix for situation were same labels are used for levels for different factors
  out$lgraph$edges <- add_row(out$lgraph$edges,
                              from = pids, to = vids)
  out
}


var_value_type <- function(x) {
  if(is.numeric(x) && length(x)==1) return("numeric")
  if(is.vector(x) && !is_named(x)) return("unnamed_vector")
  if(is.vector(x) && is_named(x)) return("named_vector")
  return("unimplemented")
}


#' Constructor for an edibble variable
#' @importFrom vctrs new_vctr
new_edibble_var <- function(labels = character(), levels = unique(labels),
                            name = character(), rep = NULL, ..., class = NULL) {
  x <- new_vctr(labels, levels = levels, name = name,
                ..., class = "edbl_var")
  class(x) <- c(class, class(x))
  x
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
