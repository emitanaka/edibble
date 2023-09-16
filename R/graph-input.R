
#' A function to process input as input for graph manipulation
#'
#' @param input An input.
#' @param prov A provenance object.
#' @param ... Unused.
#' @keywords internal
graph_input <- function(input, prov, ...) {
  UseMethod("graph_input")
}

graph_input_type = function(input) {
  if(is_edibble_levels(input)) return("edbl_lvls")
  if(is_nest_levels(input)) return("nest_lvls")
  if(is.numeric(input) & length(input) == 1) return("numeric")
  if(is_vector(input) && !is_named(input)) return("unnamed_vector")
  if(is_vector(input) && is_named(input)) return("named_vector")
  return("unimplemented")
}

graph_input.default <- function(input, prov, name, class, ...) {
  type <- graph_input_type(input)
  levels <- switch(type,
                   "numeric" = fct_attrs(lvls(label_seq_length(input, prefix = name)), !!!attr(input, "attrs")),
                   "unnamed_vector" = fct_attrs(lvls(input), !!!attr(input, "attrs")),
                   "named_vector" = fct_attrs(lvls(names(input), n = unname(input)),
                                              !!!attr(input, "attrs")),
                   "unimplemented" = abort(paste0("Not sure how to handle ", class(input)[1])))
  graph_input.edbl_lvls(levels, prov, name, class)
}

graph_input.edbl_lvls <- function(input, prov, name, class, ...) {
  fattrs <- as.data.frame(attr(input, "attrs"))
  prov$append_fct_nodes(name = name, role = class, attrs = fattrs)
  lattrs <- vec_data(input)
  value <- lattrs$..value..
  n <- lattrs$..n..
  lattrs <- lattrs[setdiff(names(lattrs), c("..value..", "..n.."))]
  prov$append_lvl_nodes(value = value, n = n, fid = prov$fct_id(name = name), attrs = lattrs)
}

graph_input.formula <- function(input, prov, name, class, ...) {
  tt <- stats::terms(input)
  vars <- rownames(attr(tt, "factors"))
  graph_input.cross_lvls(vars, prov, name, class)
}

graph_input.cross_lvls <- function(input, prov, name, class, ...) {
  flevels <- prov$fct_levels(return = "value")
  vars <- input

  pdf <- expand.grid(flevels[vars])
  pdf[[name]] <- fct_attrs(lvls(label_seq_length(nrow(pdf), prefix = name)))
  # create notes for the crossed unit
  graph_input.edbl_lvls(pdf[[name]], prov, name, class)
  # for every parent unit, draw edges for factor and level graphs
  for(var in vars) {
    puid <- prov$fct_id(name = var)
    cuid <- prov$fct_id(name = name)
    prov$append_fct_edges(from = puid, to = cuid, type = "cross")
    prov$append_lvl_edges(from = prov$lvl_id(value = pdf[[var]], fid = puid),
                          # TODO: this asserts that the level is a character
                          # which is reasonable at this stage, but I may like to make
                          # this more flexible in future
                          to = prov$lvl_id(value = as.character(pdf[[name]]), fid = cuid))
  }
}

graph_input.nest_lvls <- function(input, prov, name, class, ...) {
  parent <- input %@% "keyname"
  cross_parents <- input %@% "parents"
  clabels <- input %@% "labels"
  attrs <- NULL # attributes(input)
  prov$append_fct_nodes(name = name, role = class)
  idp <- prov$fct_id(name = parent)
  idv <- prov$fct_id(name = name)
  prov$append_fct_edges(from = idp, to = idv, type = "nest")
  plevels <- rep(names(input), lengths(input))
  clevels <- unname(unlist(input))
  pids <- prov$lvl_id(value = plevels, fid = idp)
  prov$append_lvl_nodes(value = clevels, fid = idv)
  vids <- prov$lvl_id(value = clevels, fid = idv)
  prov$append_lvl_edges(from = pids, to = vids)

  if(!is_null(cross_parents)) {
    cross_df <- do.call("rbind", cross_parents[names(input)])
    cross_parent_names <- colnames(cross_df)
    for(across in cross_parent_names) {
      prov$append_fct_edges(from = prov$fct_id(name = across), to = idv, type = "cross")
      cpids <- prov$lvl_id(value = cross_df[[across]])
      prov$append_lvl_edges(from = cpids, to = vids)
    }
  }
}



graph_input.cond_lvls <- function(input, prov, name, class, ...) {
  parent <- input %@% "keyname"
  cross_parents <- input %@% "parents"
  clabels <- input %@% "labels"
  attrs <- NULL # attributes(input)
  prov$append_fct_nodes(name = name, role = class)
  idp <- prov$fct_id(name = parent)
  idv <- prov$fct_id(name = name)
  prov$append_fct_edges(from = idp, to = idv, type = "nest")
  plevels <- rep(names(input), lengths(input))
  clevels <- unname(unlist(input))
  pids <- prov$lvl_id(value = plevels, fid = idp)
  ## unique(clevels) is the only part that's different to nest_lvls
  prov$append_lvl_nodes(value = unique(clevels), fid = idv)
  vids <- prov$lvl_id(value = clevels, fid = idv)
  prov$append_lvl_edges(from = pids, to = vids)

  if(!is_null(cross_parents)) {
    cross_df <- do.call("rbind", cross_parents[names(input)])
    cross_parent_names <- colnames(cross_df)
    for(across in cross_parent_names) {
      prov$append_fct_edges(from = prov$fct_id(name = across), to = idv, type = "cross")
      cpids <- prov$lvl_id(value = cross_df[[across]])
      prov$append_lvl_edges(from = cpids, to = vids)
    }
  }
}

