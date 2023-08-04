
#' A function to process input as input for graph manipulation
#'
#' @param input An input.
#' @param prov A provenance object.
#' @export
graph_input <- function(input, prov, ...) {
  UseMethod("graph_input")
}

graph_input_type = function(input) {
  if(is_edibble_levels(input)) return("edbl_lvls")
  if(is_nest_levels(input)) return("nest_lvls")
  if(vec_is(input, numeric(), 1)) return("numeric")
  if(vec_is(input, integer(), 1)) return("numeric")
  if(is.vector(input) && !is_named(input)) return("unnamed_vector")
  if(is.vector(input) && is_named(input)) return("named_vector")
  return("unimplemented")
}

graph_input.default <- function(input, prov, name, class) {
  type <- graph_input_type(input)
  levels <- switch(type,
                   "numeric" = fct_attrs(levels = lvl_attrs(label_seq_length(input, prefix = name)),
                                         class = class),
                   "unnamed_vector" = fct_attrs(levels = lvl_attrs(input),
                                                class = class),
                   "named_vector" = fct_attrs(levels = lvl_attrs(names(input),
                                                                 rep = unname(input)),
                                              class = class),
                   "unimplemented" = abort(paste0("Not sure how to handle ", class(input)[1])))
  graph_input.edbl_lvls(levels, name, class)
}

graph_input.edbl_lvls <- function(input, prov, name, class) {
  fid <- private$fct_new_id(n = 1)
  attrs <- attributes(input)

  fattrs <- data.frame(id = fid, name = name, class = class)
  self$append_fct_nodes(fattrs)

  lattrs <- lvl_data(input)
  lattrs$id <- private$lvl_new_id(length(input))

  self$append_lvl_nodes(lattrs, fid)
}

graph_input.formula <- function(input, prov, name, class) {
  flevels <- self$fct_levels()
  tt <- terms(input)
  vars <- rownames(attr(tt, "factor"))

  private$graph_input.cross_lvls(vars, prov, name, class)
}

graph_input.edbl_fct <- function(input, prov, name, class) {
  # this looks the same as graph_input.edbl_levels???
  fid <- private$fct_new_id
  self$append_fct_nodes(tibble(id = fid, name = name, class = class))

  lvls <- levels(input)
  lattrs <- tibble(id = private$lvl_new_id(length(lvls)),
                   value = lvls)

  self$append_lvl_nodes(lattrs, fid)
}

graph_input.cross_lvls <- function(input, prov, name, class) {
  flevels <- self$fct_levels()
  vars <- input

  pdf <- expand.grid(flevels[vars])
  pdf[[name]] <- fct_attrs(levels = lvl_attrs(1:nrow(pdf), prefix = name),
                           class = class)
  private$graph_input.edbl_lvls(pdf[[name]], name, class)
  idv <- self$fct_id_by_name(name)
  for(avar in vars) {
    idp <- self$fct_id_by_name(avar)
    self$append_fct_edges(data.frame(from = idp, to = idv, type = "nest"))
    self$append_lvl_edges(data.frame(from = self$lvl_id_by_value(pdf[[avar]], idp),
                                     to = self$lvl_id_by_value(pdf[[name]], idv)))
  }
  idvs <- self$fct_id_by_name(vars)
  cross_df <- expand.grid(from = idvs, to = idvs)
  cross_df <- subset(cross_df, from!=to)
  cross_df$type <- "cross"
  self$append_fct_edges(cross_df)
}

graph_input.nest_lvls <- function(input, name, class) {

  idv <- private$fct_new_id()
  parent <- input %@% "keyname"
  cross_parents <- input %@% "parents"
  clabels <- input %@% "labels"
  idp <- self$fct_id_by_name(c(parent, colnames(cross_parents[[1]])))
  attrs <- attributes(input)
  fattrs <- tibble::tibble(id = idv,
                           name = name,
                           class = class)
  self$append_fct_nodes(fattrs)
  self$append_fct_edges(tibble(from = idp, to = idv, type = "nest"))
  plevels <- rep(names(input), lengths(input))
  clevels <- unname(unlist(input))
  pids <- self$lvl_id_by_value(plevels, idp)
  vids <- private$lvl_new_id(length(clevels))
  self$append_lvl_nodes(tibble::tibble(id = vids,
                                       value = clevels),
                        idv)

  self$append_lvl_edges(tibble::tibble(from = pids, to = vids))
  if(!is_null(cross_parents)) {
    cross_df <- do.call("rbind", cross_parents[names(input)])
    cross_parent_names <- colnames(cross_df)
    for(across in cross_parent_names) {
      cpids <- self$lvl_id(cross_df[[across]])
      self$append_lvl_edges(tibble::tibble(from = cpids, to = vids))
    }
  }
}
