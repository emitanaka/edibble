

# IDs ---------------------------------------------------------------------

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

trt_ids <- function(design, type = "fct") {
  var_ids(design, type = type, vclass = "edbl_trt")
}

unit_ids <- function(design, type = "fct") {
  var_ids(design, type = type, vclass = "edbl_unit")
}

rcrd_ids <-  function(design, type = "fct") {
  var_ids(design, type = type, vclass = "edbl_rcrd")
}


# labels ------------------------------------------------------------------

var_labels <- function(design, vclass = NULL) {
  nodes <- fct_nodes(design)
  if(is_null(vclass)) {
    labels <- nodes$label
  } else {
    labels <- subset(nodes, class %in% vclass)$label
  }
  labels
}

trt_labels <- function(design) {
  var_labels(design, vclass = "edbl_trt")
}

unit_labels <- function(design) {
  var_labels(design, vclass = "edbl_unit")
}

rcrd_labels <- function(design) {
  var_labels(design, vclass = "edbl_rcrd")
}


# data or vector --------------------------------------------------

fct_nodes <- function(design) {
  nodes <- design$graph$nodes
  nodes$n <- lengths(fct_levels(design)[nodes$label])
  nodes
}

fct_edges <- function(design) {
  edges <- design$graph$edges
  edges$var_from <- fct_label(design, id = edges$from)
  edges$var_to <- fct_label(design, id = edges$to)
  edges
}

lvl_nodes <- function(design) {
  nodes <- design$graph$levels$nodes
  nodes$var <- fct_label(design, id = nodes$idvar)
  nodes
}

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

fct_nodes_pull <- function(design, var = -1, name = NULL, ...) {
  template_pull(design$graph$nodes, var = {{ var }}, name = {{ name }}, ...)
}

fct_edges_pull <- function(design, var = -1, name = NULL, ...) {
  template_pull(design$graph$edges, var = {{ var }}, name = {{ name }}, ...)
}

lvl_nodes_pull <- function(design, var = -1, name = NULL, ...) {
  template_pull(design$graph$levels$nodes, var = {{ var }}, name = {{ name }}, ...)
}

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

fct_nodes_filter <- function(design, ...) {
  template_filter(fct_nodes(design), ...)
}

lvl_nodes_filter <- function(design, ...) {
  template_filter(lvl_nodes(design), ...)
}

fct_edges_filter <- function(design, ...) {
  template_filter(fct_edges(design), ...)
}

lvl_edges_filter <- function(design, ...) {
  template_filter(lvl_edges(design), ...)
}




fct_n <- function(design) {
  nrow(design$graph$node)
}

lvl_n <- function(design) {
  nrow(design$graph$levels$node)
}



# vectorised
fct_id <- function(design, label = NULL) {
  label_to_id_fct <- fct_nodes_pull(design, id, label)
  label <- label %||% names(label_to_id_fct)
  unname(label_to_id_fct[as.character(label)])
}

lvl_id <- function(design, label = NULL) {
  label_to_id_lvl <- lvl_nodes_pull(design, id, label)
  label <- label %||% names(label_to_id_lvl)
  unname(label_to_id_lvl[as.character(label)])
}

fct_label <- function(design, id = NULL) {
  id_to_label_fct <- fct_nodes_pull(design, label, id)
  ids_fct <-  id %||% fct_nodes_pull(design, id)
  unname(id_to_label_fct[as.character(ids_fct)])
}

lvl_label <- function(design, id = NULL) {
  id_to_label_lvl <- lvl_nodes_pull(design, label, id)
  ids_lvl <-  id %||% lvl_nodes_pull(design, id)
  unname(id_to_label_lvl[as.character(ids_lvl)])
}

fct_class <- function(design, id = NULL) {
  id_to_class_fct <- fct_nodes_pull(design, class, id)
  ids_fct <-  id %||% fct_nodes_pull(design, id)
  unname(id_to_class_fct[as.character(ids_fct)])
}

lvl_class <- function(design, id = NULL) {
  id_to_class_lvl <- lvl_nodes_pull(design, class, id)
  ids_lvl <-  id %||% lvl_nodes_pull(design, id)
  unname(id_to_class_lvl[as.character(ids_lvl)])
}


fct_child <- function(design, id = NULL) {
  child_ids <- fct_edges_pull(design, to)
  parent_ids <- fct_edges_pull(design, from)
  child_ids[parent_ids %in% id]
}

lvl_child <- function(design, id = NULL) {
  child_ids <- lvl_edges_pull(design, to)
  parent_ids <- lvl_edges_pull(design, from)
  child_ids[parent_ids %in% id]
}

fct_parent <- function(design, id = NULL) {
  parent_ids <- fct_edges_pull(design, from)
  child_ids <- fct_edges_pull(design, to)
  parent_ids[child_ids %in% id]
}

lvl_parent <- function(design, id = NULL) {
  parent_ids <- lvl_edges_pull(design, from)
  child_ids <- lvl_edges_pull(design, to)
  parent_ids[child_ids %in% id]
}

# includes self
fct_ancestor <- function(design, id = NULL) {
  out <- id
  parent_ids <- fct_parent(design, id)
  if(!is_empty(parent_ids)) {
    out <- c(out, fct_ancestor(design, parent_ids))
  }
  out
}

lvl_ancestor <- function(design, id = NULL) {
  out <- id
  parent_ids <- lvl_parent(design, id)
  if(!is_empty(parent_ids)) {
    out <- c(out, lvl_ancestor(design, parent_ids))
  }
  out
}



fct_levels <- function(design, id = NULL, label = NULL) {
  fcts_id <- fct_nodes_pull(design, id)
  fcts_name <-  fct_nodes_pull(design, label)
  qid <- id %||% fct_id(design, label)
  out <- lvl_nodes_filter(design, idvar %in% qid)
  out$var <- fct_label(design, out$idvar)
  split(out$label, out$var)
}


