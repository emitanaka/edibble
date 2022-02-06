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
                     .name_repair = c("check_unique", "unique", "universal", "minimal"),
                     .code = NULL) {

  not_edibble(.edibble)

  .name_repair <- match.arg(.name_repair)
  res <- .edibble

  if(is_edibble_design(.edibble)) {
    dots <- enquos(..., .named = TRUE, .homonyms = "error")
    fnames_new <- names(dots)
    fnames_old <- names(res)
    fnames <- vec_as_names(c(fnames_old, fnames_new), repair = .name_repair)
    for(i in seq_along(dots)) {
      fname <- fnames[i + length(fnames_old)]
      value <- eval_tidy(dots[[i]], data = c(fct_levels(res), list(des = res, .fname = fname)))
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

#' Add an edibble vertex
#'
#' @param value A value that may be a single number, unnamed vector,
#' a one sided formula or linkabble.
#' @param name Name of the edibble variable as string.
#' @param design An edibble graph with levels converted as list used
#' for evaluation in any expressions.
#' @describeIn add_edibble_vertex default method
#' @importFrom tibble add_row
#' @export
add_edibble_vertex.default <- function(value, fname, class, design) {

  type <- fct_value_type(value)
  levels <- switch(type,
                   "numeric" = fct_attrs(levels = lvl_attrs(1:value, prefix = fname),
                                          class = class),
                   "unnamed_vector" = fct_attrs(levels = lvl_attrs(value),
                                                 class = class),
                   "named_vector" = fct_attrs(levels = lvl_attrs(names(value),
                                                                   rep = unname(value)),
                                               class = class),
                   "unimplemented" = abort(paste0("Not sure how to handle ", class(value)[1])))

  add_edibble_vertex.edbl_levels(levels, fname, class, design)
}

fct_last_id <- function(design) {
  ifelse(fct_n(design), max(fct_id(design)), 0L)
}

lvl_last_id <- function(design) {
  ifelse(lvl_n(design), max(lvl_id(design)), 0L)
}

rbind_ <- function(df1, df2) {
  if(nrow(df1) & nrow(df2)) {
    df1[setdiff(names(df2), names(df1))] <- NA
    df2[setdiff(names(df1), names(df2))] <- NA
    out <- rbind(df1, df2)
  } else if(nrow(df1)) {
    df1[setdiff(names(df2), names(df1))] <- NA
    out <- df1
  } else if(nrow(df2)) {
    df2[setdiff(names(df1), names(df2))] <- NA
    out <- df2
  } else {
    out <- cbind(df1, df2[setdiff(names(df2), names(df1))])
  }
  out[c(names(df1), setdiff(names(out), names(df1)))]
}

add_edibble_vertex.edbl_levels <- function(value, fname, class, design) {
  out <- design
  fid <- fct_last_id(out) + 1L
  lid <- lvl_last_id(out) + 1L
  attrs <- attributes(value)
  fnodes <- fct_nodes(design)
  fattrs <- do.call(data.frame, c(attrs[setdiff(names(attrs), c("names", "class"))],
                                  list(stringsAsFactors = FALSE,
                                       id = fid,
                                       name = fname,
                                       class = class)))
  out$graph$nodes <- rbind_(fnodes, fattrs)

  lnodes <- lvl_nodes(design)
  lattrs <- lvl_data(value)
  lattrs$idvar <- fid
  lattrs$var <- fname
  lattrs$id <- lid:(lid + length(value) - 1)
  out$graph$levels$nodes <- rbind_(lnodes, lattrs)
  out
}

add_edibble_vertex.formula <- function(value, fname, class, design) {
  flevels <- fct_levels(design)
  tt <- terms(value)
  vars <- rownames(attr(tt, "factor"))

  pdf <- expand.grid(flevels[vars])
  pdf[[fname]] <- fct_attrs(levels = lvl_attrs(1:nrow(pdf), prefix = fname),
                             class = class)
  out <- add_edibble_vertex.edbl_levels(pdf[[fname]], fname, class, design)
  idv <- fct_nodes_filter(out, name == fname)$id
  for(avar in vars) {
    idp <- fct_nodes_filter(out, name == avar)$id
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
  cross_parents <- value %@% "parents"
  idp <- fct_id(design, name = c(parent, colnames(cross_parents[[1]])))
  attrs <- attributes(value)
  fnodes <- fct_nodes(design)
  fattrs <- do.call(data.frame, c(attrs[setdiff(names(attrs), c("names", "keyname", "class", "parents"))],
                                  list(stringsAsFactors = FALSE,
                                       id = idv,
                                       name = fname,
                                       class = class)))
  out$graph$nodes <- rbind_(fnodes, fattrs)
  out$graph$edges <- add_row(out$graph$edges,
                             from = idp, to = idv)
  plevels <- rep(names(value), lengths(value))
  clevels <- unname(unlist(value))
  out$graph$levels$nodes <- add_row(out$graph$levels$nodes,
                                   idvar = idv,
                                   id = idl:(idl + sum(lengths(value)) - 1),
                                   name = clevels,
                                   # FIXME: should allow labels to be user supplied one
                                   label = clevels)
  pids <- lvl_id(out, plevels)
  vids <- lvl_id(out, clevels)
  # TODO: fix for situation were same labels are used for levels for different factors
  out$graph$levels$edges <- add_row(out$graph$levels$edges,
                                    from = pids, to = vids)
  if(!is_null(cross_parents)) {
    cross_df <- do.call("rbind", cross_parents[names(value)])
    cross_parent_names <- colnames(cross_df)
    for(across in cross_parent_names) {
      cpids <- lvl_id(out, cross_df[[across]])
      out$graph$levels$edges <- add_row(out$graph$levels$edges,
                                        from = cpids, to = vids)
    }
  }
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
                ..., class = c("edbl_var", "character"))
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

#' @export
as.character.edbl_levels <- function(x, ...) {
  format(x)
}

#' @export
as.integer.edbl_levels <- function(x, ...) {
  out <- as.integer(as.factor(as.character(x)))
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
  if(inherits(x, "edbl_rcrd")) {
    unique(attr(x, "unit_values"))
  } else {
    attr(x, "levels")
  }
}

#' @rdname utility-edibble-var
#' @export
is_edibble_var <- function(x) {
  inherits(x, "edbl_var")
}

#' @importFrom vctrs vec_math
#' @export
vctrs::vec_math

#' @method vec_math edbl_var
#' @export
vec_math.edbl_var <- function(.fn, .x, ...) {
  if(.fn %in% c("is.nan", "is.infinite")) return(rep_len(FALSE, length(.x)))
  if(.fn == "is.finite") return(rep_len(TRUE, length(.x)))
  out <- lapply(as.character(.x), get(.fn), ...)
  vctrs::vec_restore(out, .x)
}


