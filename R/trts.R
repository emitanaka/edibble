#' Set the treatment variables
#'
#' @description
#' This function creates a special class, called `edbl_trt`, of edibble variables.
#'
#' @section Definition of _treatment_:
#' The word _treatment_ is sometimes used to refer to one of these variables.
#' When there are more than one treatment variables then this unfortunately
#' confuses whether treatment refers to the variable or the combination of
#' all treatment variables.
#'
#' Treatment is the whole description of what is applied in an experiment.
#'
#' @inheritParams set_vars
#' @seealso See [set_units()] for setting units.
#' @export
set_trts <- function(.data, ...,
                      .name_repair = c("check_unique", "unique", "universal", "minimal")) {
  .data$set_vars(..., .name_repair = .name_repair, .class = "edbl_trt")
}

#' @export
allocate_trts <- function(.data, ..., class = NULL) {
  .data$allocate_trts(..., class = class)
}

#' Define which unit to apply treatment
#'
#' @param .data An `edbl_graph` object.
#' @param ... One-sided or two-sided formula. If the input is a one-sided formula
#' then the whole treatment is applied to the specified unit.
#' @param class A sub-class. This is meant so that it can invoke the method
#' `randomise_trts.class`.
#' @importFrom rlang f_lhs
#' @export
allocate_trts_ext <- function(.data, ..., class = NULL) {
  # doesn't support : or * right now
  dots <- enquos(...)
  specials <- c(":", "*")
  out <- .data
  for(i in seq_along(dots)) {
    expr <- quo_get_expr(dots[[i]])
    .trt <- setdiff(as.character(f_lhs(expr)), specials)
    # there should be only a unit
    .EU <- as.character(f_rhs(expr)) #setdiff(as.character(f_rhs(expr)), specials)
    if(length(.trt)) {
      # there should be an error if .trt is not within .data
      # maybe there should be a check that .trt is edbl_trt
      vnames_from <- .trt
    } else {
      vnames_from <- names(subset(.data, class=="edbl_trt", .vtype = "var"))
    }
    out <- igraph::add_edges(out,
                             cross_edge_seq(.data,
                                            var_levels(.data, vnames_from),
                                            var_levels(.data, .EU)),
                             attr = edge_attr_opt("t2vmay"))
    out <- igraph::add_edges(out,
                             cross_edge_seq(.data,
                                            vnames_from,
                                            vnames_to = .EU),
                             attr = edge_attr_opt("t2v"))
  }

  structure(out, class = c(class, class(.data)))
}

#' @export
get_trt_vars <- function(x) {
  if(is_edibble_graph(x)) {
    return(V(x)$vname[V(x)$class=="edbl_trt"])
  }
  if(is_edibble(x)) {
    ind <- unlist(lapply(x, function(var) "edbl_trt" %in% class(var)))
    return(names(x)[ind])
  }
}

#' @export
get_trt_levels <- function(.data) {
  out <- list()
  vars <- get_trt_vars(.data)
  for(avar in vars) {
    out[[avar]] <- var_levels(.data, avar)
  }
  out
}

#' Unlike `vars_levels`, there is a check in place
#' that it is a treatment.
trts_levels <- function(.data, vnames = NULL) {
  if(is_null(vnames)) {
    vars_levels(.data, names_trts(.data))
  } else {
    if(!all(vnames %in% names_trts(.data))) {
      abort("Some {vnames} not a treatment.")
    }
    vars_levels(.data, vnames)
  }
}

# make this to a replicabble?
trts_levels_df <- function(.data, vnames = NULL) {
  tibble::as_tibble(expand.grid(trts_levels(.data, vnames = vnames), stringsAsFactors = FALSE))
}

#' @export
n_trts <- function(.data, ...) {
  UseMethod("n_trts")
}

n_trts.edbl_graph <- function(.data) {
  return(prod(lengths(get_trt_levels(.data))))
}

n_trts.edbl_df <- function(.data) {
  ind <- unlist(lapply(.data, function(var) "edbl_trt" %in% class(var)))
  return(prod(lengths(lapply(.data, levels)[ind])))
}

#' @export
n_vars <- function(.data, class = NULL) {
  UseMethod("n_vars")
}

#' @export
n_vars.edbl_df <- function(.data, class = NULL) {
  if(is_null(class)) return(ncol(.data))
  sum(map_lgl(.data, function(x) any(class(x) %in% class)))
}

#' @export
n_vars.edbl_graph <- function(.data, class = NULL) {
  vgraph <- subset(.data, class %in% class, .vtype = "var")
  if(is_null(class)) return(sum(V(.data)$vtype=="var"))
  sum(V(.data)$class %in% class)
}

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.edbl_trt <- function(x, ...)  {
  paste0("trt(", number_si_prefix(nlevels(x)), ")")
}

#' @importFrom vctrs vec_ptype_full
#' @export
vec_ptype_full.edbl_trt <- function(x, ...) paste0("trt(", nlevels(x), ")")

#' @importFrom vctrs vec_cast
#' @export
vec_cast.edbl_trt.edbl_trt <- function(x, to, ...) {
  x
}


