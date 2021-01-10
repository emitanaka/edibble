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
#' @family user-facing functions
#' @export
set_trts <- function(.design, ...,
                      .name_repair = c("check_unique", "unique", "universal", "minimal")) {
  set_vars(.design, ..., .name_repair = .name_repair, .class = "edbl_trt")
}

#' Define which unit to apply treatment
#'
#' @param .data An `edbl_graph` object.
#' @param ... One-sided or two-sided formula. If the input is a one-sided formula
#' then the whole treatment is applied to the specified unit.
#' @param class A sub-class. This is meant so that it can invoke the method
#' `randomise_trts.class`.
#' @importFrom rlang f_lhs f_rhs
#' @importFrom igraph add_edges
#' @family user-facing functions
#' @export
allocate_trts <- function(.design, ...) {
  dots <- enquos(...)
  specials <- c(":", "*")
  for(i in seq_along(dots)) {
    expr <- quo_get_expr(dots[[i]])
    .trt <- setdiff(as.character(f_lhs(expr)), specials)
    # there should be only one unit
    .EU <- as.character(f_rhs(expr)) #setdiff(as.character(f_rhs(expr)), specials)
    .design$add_allocation(.trt, .EU)
  }
  .design
}


#' @export
get_trt_vars <- function(x) {
  if(is_edibble_graph(x)) {
    return(V(x)$vname[V(x)$class=="edbl_trt"])
  }
  if(is_edibble_df(x)) {
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


