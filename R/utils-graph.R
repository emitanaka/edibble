

# IDs ---------------------------------------------------------------------

#' Get the vertex IDs of the edibble design
#'
#' Set of operators to get the vertex/node IDs of an `edbl_design` object.
#'
#' @param design An edibble design.
#' @param type A string value of either `"fct"` or `"lvl"`. Use `"fct"` for the
#'   factor graph or `"lvl"` for the level graph.
#' @param vclass The class for the vertex/node.
#'
#' @family design manipulators
#' @export
var_ids <- function(design, type = c("fct", "lvl"), vclass = NULL) {
  type <- match.arg(type)
  fnodes <- fct_nodes(design)
  lnodes <- lvl_nodes(design)
  if(is_null(vclass)) {
    ids <- fnodes$id
  } else {
    ids <- subset(fnodes, class %in% vclass)$id
  }
  switch(type,
         fct = ids,
         lvl = subset(lnodes, idvar %in% ids)$id)
}

#' @rdname var_ids
#' @export
trt_ids <- function(design, type = "fct") {
  var_ids(design, type = type, vclass = "edbl_trt")
}

#' @rdname var_ids
#' @export
unit_ids <- function(design, type = "fct") {
  var_ids(design, type = type, vclass = "edbl_unit")
}

#' @rdname var_ids
#' @export
rcrd_ids <-  function(design, type = "fct") {
  var_ids(design, type = type, vclass = "edbl_rcrd")
}


# labels ------------------------------------------------------------------

#' Get the node labels of the edibble design
#'
#' @inheritParams var_ids
#' @family design manipulators
#' @export
var_labels <- function(design, vclass = NULL) {
  nodes <- fct_nodes(design)
  if(is_null(vclass)) {
    labels <- nodes$label
  } else {
    labels <- subset(nodes, class %in% vclass)$label
  }
  labels
}

#' @rdname var_labels
#' @export
trt_labels <- function(design) {
  var_labels(design, vclass = "edbl_trt")
}

#' @rdname var_labels
#' @export
unit_labels <- function(design) {
  var_labels(design, vclass = "edbl_unit")
}

#' @rdname var_labels
#' @export
rcrd_labels <- function(design) {
  var_labels(design, vclass = "edbl_rcrd")
}


# data or vector --------------------------------------------------

#' Get the node or edge data from an edibble design
#'
#' @inheritParams var_ids
#' @family design manipulators
#' @name design_data
NULL

#' @rdname design_data
#' @export
fct_nodes <- function(design) {
  nodes <- design$graph$nodes
  nodes$n <- lengths(fct_levels(design)[nodes$label])
  nodes
}

#' @rdname design_data
#' @export
fct_edges <- function(design) {
  edges <- design$graph$edges
  edges$var_from <- fct_label(design, id = edges$from)
  edges$var_to <- fct_label(design, id = edges$to)
  edges
}

#' @rdname design_data
#' @export
lvl_nodes <- function(design) {
  nodes <- design$graph$levels$nodes
  nodes$var <- fct_label(design, id = nodes$idvar)
  nodes
}

#' @rdname design_data
#' @export
lvl_edges <- function(design) {
  edges <- design$graph$levels$edges
  edges$lvl_from <- lvl_label(design, id = edges$from)
  edges$lvl_to <- lvl_label(design, id = edges$to)
  edges
}

template_pull <- function(data, var = -1, name = NULL, ...) {
  var <- tidyselect::vars_pull(names(data), !!enquo(var))
  name <- enquo(name)
  if (quo_is_null(name)) {
    return(data[[var]])
  }
  name <- tidyselect::vars_pull(names(data), !!name)
  set_names(data[[var]], nm = data[[name]])
}

#' @rdname design_data
#' @export
fct_nodes_pull <- function(design, var = -1, name = NULL, ...) {
  template_pull(design$graph$nodes, var = {{ var }}, name = {{ name }}, ...)
}

#' @rdname design_data
#' @export
fct_edges_pull <- function(design, var = -1, name = NULL, ...) {
  template_pull(design$graph$edges, var = {{ var }}, name = {{ name }}, ...)
}

#' @rdname design_data
#' @export
lvl_nodes_pull <- function(design, var = -1, name = NULL, ...) {
  template_pull(design$graph$levels$nodes, var = {{ var }}, name = {{ name }}, ...)
}

#' @rdname design_data
#' @export
lvl_edges_pull <- function(design, var = -1, name = NULL, ...) {
  template_pull(design$graph$levels$edges, var = {{ var }}, name = {{ name }}, ...)
}

template_filter <- function(data, ...) {
  dots <- enquos(...)
  ind <- !logical(length = nrow(data))
  for(i in seq_along(dots)) {
    ind <- ind & eval_tidy(dots[[i]], data = data)
  }
  data[ind, ]
}

#' @rdname design_data
#' @export
fct_nodes_filter <- function(design, ...) {
  template_filter(fct_nodes(design), ...)
}

#' @rdname design_data
#' @export
lvl_nodes_filter <- function(design, ...) {
  template_filter(lvl_nodes(design), ...)
}

#' @rdname design_data
#' @export
fct_edges_filter <- function(design, ...) {
  template_filter(fct_edges(design), ...)
}

#' @rdname design_data
#' @export
lvl_edges_filter <- function(design, ...) {
  template_filter(lvl_edges(design), ...)
}


#' @rdname design_data
#' @export
fct_n <- function(design) {
  nrow(design$graph$node)
}

#' @rdname design_data
#' @export
lvl_n <- function(design) {
  nrow(design$graph$levels$node)
}



# vectorised
#' @rdname var_ids
#' @export
fct_id <- function(design, label = NULL) {
  label_to_id_fct <- fct_nodes_pull(design, id, label)
  label <- label %||% names(label_to_id_fct)
  unname(label_to_id_fct[as.character(label)])
}

#' @rdname var_ids
#' @export
lvl_id <- function(design, label = NULL) {
  label_to_id_lvl <- lvl_nodes_pull(design, id, label)
  label <- label %||% names(label_to_id_lvl)
  unname(label_to_id_lvl[as.character(label)])
}

#' @rdname var_labels
#' @export
fct_label <- function(design, id = NULL) {
  id_to_label_fct <- fct_nodes_pull(design, label, id)
  ids_fct <-  id %||% fct_nodes_pull(design, id)
  unname(id_to_label_fct[as.character(ids_fct)])
}

#' @rdname var_labels
#' @export
lvl_label <- function(design, id = NULL) {
  id_to_label_lvl <- lvl_nodes_pull(design, label, id)
  ids_lvl <-  id %||% lvl_nodes_pull(design, id)
  unname(id_to_label_lvl[as.character(ids_lvl)])
}

#' @rdname design_data
#' @export
fct_class <- function(design, id = NULL) {
  id_to_class_fct <- fct_nodes_pull(design, class, id)
  ids_fct <-  id %||% fct_nodes_pull(design, id)
  unname(id_to_class_fct[as.character(ids_fct)])
}

#' @rdname design_data
#' @export
lvl_class <- function(design, id = NULL) {
  id_to_class_lvl <- lvl_nodes_pull(design, class, id)
  ids_lvl <-  id %||% lvl_nodes_pull(design, id)
  unname(id_to_class_lvl[as.character(ids_lvl)])
}

#' @rdname var_ids
#' @export
fct_child <- function(design, id = NULL) {
  child_ids <- fct_edges_pull(design, to)
  parent_ids <- fct_edges_pull(design, from)
  child_ids[parent_ids %in% id]
}

#' @rdname var_ids
#' @export
lvl_child <- function(design, id = NULL) {
  child_ids <- lvl_edges_pull(design, to)
  parent_ids <- lvl_edges_pull(design, from)
  child_ids[parent_ids %in% id]
}

#' @rdname var_ids
#' @export
fct_parent <- function(design, id = NULL) {
  parent_ids <- fct_edges_pull(design, from)
  child_ids <- fct_edges_pull(design, to)
  parent_ids[child_ids %in% id]
}

#' @rdname var_ids
#' @export
lvl_parent <- function(design, id = NULL) {
  parent_ids <- lvl_edges_pull(design, from)
  child_ids <- lvl_edges_pull(design, to)
  parent_ids[child_ids %in% id]
}

# includes self
#' @rdname var_ids
#' @export
fct_ancestor <- function(design, id = NULL) {
  out <- id
  parent_ids <- fct_parent(design, id)
  if(!is_empty(parent_ids)) {
    out <- c(out, fct_ancestor(design, parent_ids))
  }
  out
}

#' @rdname var_ids
#' @export
fct_obs_unit <- function(design, initial = NULL) {
  if(is.null(initial)) {
    uids <- unit_ids(design)
    initial <- uids[1]
  }
  id <- fct_child(design, initial)
  if(length(id)) return(fct_obs_unit(design, initial = id[1]))
  return(initial)
}

#' @rdname var_ids
#' @export
lvl_ancestor <- function(design, id = NULL) {
  out <- id
  parent_ids <- lvl_parent(design, id)
  if(!is_empty(parent_ids)) {
    out <- c(out, lvl_ancestor(design, parent_ids))
  }
  out
}


#' @rdname var_labels
#' @export
fct_levels <- function(design, id = NULL, label = NULL) {
  fcts_id <- fct_nodes_pull(design, id)
  fcts_name <-  fct_nodes_pull(design, label)
  qid <- id %||% fct_id(design, label)
  out <- lvl_nodes_filter(design, idvar %in% qid)
  out$var <- fct_label(design, out$idvar)
  split(out$label, out$var)
}


