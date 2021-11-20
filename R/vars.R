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
  res <- .edibble

  if(is_edibble_design(.edibble)) {
    dots <- enquos(..., .named = TRUE, .homonyms = "error")
    fnames_new <- names(dots)
    fnames_old <- names(res)
    fnames <- vec_as_names(c(fnames_old, fnames_new), repair = .name_repair)
    levels_fct <- NULL
    for(i in seq_along(dots)) {
      fname <- fnames[i + length(fnames_old)]
      if(fct_n(res)) levels_fct <- fct_levels(res)
      value <- eval_tidy(dots[[i]], data = c(levels_fct, list(.fname = fname)))
      res <- add_edibble_vertex(value, fname, .class, res)
    }

  } else if(is_edibble_table(.edibble)) {
    dots <- enquos(..., .named = TRUE)

    loc <- eval_select(expr(!!names(dots)), res)
    for(i in seq_along(loc)) {
      var <- res[[loc[i]]]
      lvls <- unique(as.character(var))
      fname <- names(loc)[i]
      res[[loc[i]]] <- ...
      res <- add_edibble_vertex(lvls, fname, .class, res)
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
#' @export
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
add_edibble_vertex.default <- function(value, fname, class, design) {

  type <- fct_value_type(value)
  levels <- switch(type,
                   "numeric" = traits(n = value, prefix = fname, class = class),
                   "unnamed_vector" = traits(labels = value, class = class),
                   "named_vector" = traits(labels = names(value),
                                           rep = unname(value),
                                           class = class),
                   "unimplemented" = traits(labels = character(), class = class))

  add_edibble_vertex.edbl_trait(levels, fname, class, design)
}

fct_last_id <- function(design) {
  ifelse(fct_n(design), max(fct_id(design)), 0L)
}

lvl_last_id <- function(design) {
  ifelse(lvl_n(design), max(lvl_id(design)), 0L)
}


add_edibble_vertex.edbl_trait <- function(value, fname, class, design) {
  out <- design
  fid <- fct_last_id(out) + 1L
  lid <- lvl_last_id(out) + 1L
  attrs <- attributes(value)
  missing_cols <- setdiff(names(attrs), names(fct_nodes(design)))
  out$graph$nodes[missing_cols] <- NA
  out$graph$nodes <- add_row(out$graph$nodes,
                            id = fid,
                            label = fname,
                            class = class,
                            !!!attrs[missing_cols])
  out$graph$levels$nodes <- add_row(out$graph$levels$nodes,
                                    idvar = fid,
                                    id = lid:(lid + length(value) - 1),
                                    label = as.character(value))
  out
}

add_edibble_vertex.formula <- function(value, fname, class, design) {
  flevels <- fct_levels(design)
  tt <- terms(value)
  vars <- rownames(attr(tt, "factor"))
  pdf <- expand.grid(flevels[vars])
  pdf[[fname]] <- traits(prefix = fname, n = nrow(pdf), class = class)
  out <- add_edibble_vertex.edbl_trait(pdf[[fname]], fname, class, design)
  idv <- fct_nodes_filter(out, label == fname)$id
  for(avar in vars) {
    idp <- fct_nodes_filter(out, label == avar)$id
    out$graph$edges <- add_row(out$graph$edges,
                              from = idp, to = idv)
    out$graph$levels$edges <- add_row(out$graph$levels$edges,
                                      from = lvl_id(out, pdf[[avar]]),
                                      to = lvl_id(out, pdf[[fname]]))
  }
  out
}

add_edibble_vertex.nst_levels <- function(value, fname, class, design) {
  out <- design
  idv <- fct_last_id(out) + 1L
  idl <- lvl_last_id(out) + 1L
  parent <- value %@% "keyname"
  idp <- fct_id(design, label = parent)
  attrs <- attributes(value)
  missing_cols <- setdiff(names(attrs), c(names(fct_nodes(design)), "names", "keyname"))
  out$graph[missing_cols] <- NA
  out$graph$nodes <- add_row(out$graph$nodes,
                             id = idv,
                             label = fname,
                             class = class,
                             !!!attrs[missing_cols])
  out$graph$edges <- add_row(out$graph$edges,
                             from = idp, to = idv)
  plevels <- rep(names(value), lengths(value))
  clevels <- unname(unlist(value))
  out$graph$levels$nodes <- add_row(out$graph$levels$nodes,
                                   idvar = idv,
                                   id = idl:(idl + sum(lengths(value)) - 1),
                                   label = clevels)
  pids <- lvl_id(out, plevels)
  vids <- lvl_id(out, clevels)
  # TODO: fix for situation were same labels are used for levels for different factors
  out$graph$levels$edges <- add_row(out$graph$levels$edges,
                                    from = pids, to = vids)
  out
}


fct_value_type <- function(x) {
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
levels.edbl_var <- function(x) {
  attr(x, "levels")
}

#' @rdname utility-edibble-var
#' @export
is_edibble_var <- function(x) {
  inherits(x, "edbl_var")
}
