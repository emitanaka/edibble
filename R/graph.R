





# data or vector --------------------------------------------------

#' Activate the provenance in the edibble design object
#'
#' This is a developer function to create a new Kitchen class with
#' the existing design.
#'
#' @param x An edibble object.
#' @return A Kitchen object.
#' @examples
#' activate_provenance(takeout())
#' @export
activate_provenance <- function(.edibble,
                                overwrite = c("graph", "anatomy", "recipe")) {
  des <- edbl_design(.edibble)
  prov <- des$provenance
  if(!is_environment(prov)) {
    abort("The provenance is not included in the design.")
  }
  prov$reactivate(des, overwrite)
  return(prov)
}


#' Get the node or edge data from an edibble design
#'
#' @param edibble An edibble object.
#' @family design manipulators
#' @name design_data
NULL

#' @rdname design_data
#' @export
fct_nodes <- function(x) {
  prov <- activate_provenance(x)
  fnodes <- prov$fct_nodes
  fnodes[, c("name", "role", "attrs")]
}

#' @rdname design_data
#' @export
fct_edges <- function(x) {
  prov <- activate_provenance(x)
  fedges <- prov$fct_edges
  fedges[, c("var_from", "var_to", "type", "group", "attrs")]
}


#' @rdname design_data
#' @export
lvl_nodes <- function(x) {
  prov <- activate_provenance(x)
  lnodes <- prov$lvl_nodes
  lnodes <- lapply(lnodes, function(x) x[setdiff(names(x), "id")])
  names(lnodes) <- prov$fct_names(id = as.numeric(names(lnodes)))
  lnodes
}

#' @rdname design_data
#' @export
lvl_edges <- function(x) {
  prov <- activate_provenance(x)
  ledges <- prov$lvl_edges
  lnodes <- prov$lvl_nodes
  lnodes_df <- do.call("rbind", lapply(names(lnodes), function(x) data.frame(fid = as.numeric(x), id = lnodes[[x]]$id)))
  ledges$var_from <- prov$fct_names(id = lnodes_df[match(ledges$from, lnodes_df$id), "fid"])
  ledges$var_to <- prov$fct_names(id = lnodes_df[match(ledges$to, lnodes_df$id), "fid"])
  ledges <- split(ledges, paste(ledges$var_from, "->", ledges$var_to))
  lapply(ledges, function(df) {
    df$val_from <- prov$lvl_values(id = df$from, fid = prov$fct_id(name = df$var_from[1])) %||% character(0)
    df$val_to <- prov$lvl_values(id = df$to, fid = prov$fct_id(name = df$var_to[1])) %||% character(0)
    df[, c("var_from", "var_to", "val_from", "val_to", "attrs")]
  })
}

#' @export
fct_graph <- function(x) {
  prov <- activate_provenance(x)
  fnodes <- fct_nodes(x)
  fedges <- fct_edges(x)
  new_edibble_graph(fnodes = fnodes, fedges = fedges)
}


lvl_graph <- function(x) {
  prov <- activate_provenance(x)
  lnodes <- lvl_nodes(x)
  ledges <- lvl_edges(x)
  new_edibble_graph(lnodes = lnodes, ledges = ledges)
}

