





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
  prov <- get_provenance(x)
  fnodes <- prov$fct_nodes
  fnodes[, c("name", "role", "attrs")]
}

#' @rdname design_data
#' @export
fct_edges <- function(x) {
  prov <- get_provenance(x)
  fedges <- prov$fct_edges
  fedges[, c("var_from", "var_to", "type", "group", "attrs")]
}


#' @rdname design_data
#' @export
lvl_nodes <- function(x) {
  prov <- get_provenance(x)
  lnodes <- prov$lvl_nodes
  lnodes <- lapply(lnodes, function(x) x[, "value", drop = FALSE])
  names(lnodes) <- prov$fct_names(id = as.numeric(names(lnodes)))
  lnodes
}

#' @rdname design_data
#' @export
lvl_edges <- function(x) {
  prov <- get_provenance(x)
  ledges <- prov$lvl_edges
  lnodes <- prov$lvl_nodes
  lnodes_df <- do.call("rbind", lapply(names(lnodes), function(x) data.frame(fid = as.numeric(x), id = lnodes[[x]]$id)))
  ledges$var_from <- prov$fct_names(id = lnodes_df[match(ledges$from, lnodes_df$id), "fid"])
  ledges$var_to <- prov$fct_names(id = lnodes_df[match(ledges$to, lnodes_df$id), "fid"])
  ledges <- split(ledges, paste(ledges$var_from, "->", ledges$var_to))
  lapply(ledges, function(df) {
    df$val_from <- prov$lvl_values(id = df$from, fid = prov$fct_id(name = df$var_from[1]))
    df$val_to <- prov$lvl_values(id = df$to, fid = prov$fct_id(name = df$var_to[1]))
    df[, c("var_from", "var_to", "val_from", "val_to", "attrs")]
  })
}


get_provenance <- function(x) {
  if(is_provenance(x)) {
    x
  } else {
    not_edibble(x)
    activate_provenance(x)
  }
}

