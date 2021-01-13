#' @importFrom igraph neighbors V
find_unit_parents <- function(graph, vname) {
  ugraph <- subset(graph, class=="edbl_unit", .vtype = "var")
  unit_vertex <- which(V(ugraph)$name==vname)
  # get direct parents
  parent_vertices <- neighbors(ugraph, unit_vertex, mode = "in")
  if(length(parent_vertices) > 0) {
    var_names(ugraph, parent_vertices)
  } else {
    NULL
  }
}

find_unit_ancestors <- function(graph, vname) {
  aname <- vname
  ancestors <- NULL
  repeat({
    aname <- find_unit_parents(graph, aname)
    if(is_null(aname)) break
    ancestors <- c(ancestors, aname)
  })
  ancestors
}

#' @importFrom igraph E ends
endpoints <- function(graph, etype, vtype_entry) {
  sgraph <- subset(graph, vtype==vtype_entry)
  ind <- which(E(sgraph)$etype == etype)
  es <- E(sgraph)[ind]
  ends <- ends(sgraph, es)
  from <- ends[, 1]
  to <- ends[, 2]
  data.frame(from = from, to = to, es = as.numeric(es), stringsAsFactors = FALSE)
}


#' Randomise treatments
#'
#' @inheritParams design-context
#' @param ... Currently unused.
#' @seealso Set the treatments by [set_trts()], set units by [set_units()],
#' and specify which treatment factors are applied to which units by using
#' [allocate_trts()].
#' @family user-facing functions
#' @export
randomise_trts <- function(.edibble, ...) {
  UseMethod("randomise_trts")
}

#' @rdname randomise_trts
#' @export
randomise_trts.EdibbleDesign <- function(.edibble) {
  not_edibble(.edibble)
  .design <- get_edibble_design(.edibble)
  .design$save_seed()
  .design$assign_allocation()
  update_design(.edibble, .design)
}

# possibly DELETE
#' Get the variable names given the vertex name of edibble graph
#' @param .design An edibble graph.
#' @param names A vector of character with vertex names.
#' @return A character vector of the variable name.
names_to_vnames <- function(.design, names) {
  dict <- setNames(V(.design)$vname, V(.design)$name)
  unname(dict[names])
}

names_to_lnames <- function(.design, names) {
  vnames <- names_to_vnames(.design, names)
  map_chr(seq_along(names),
          function(i) gsub(paste0("^", vnames[i], ":"), "", names[i]))
}

names_to_nesting_names <- function(.design, names) {
  labels <- V(.design)$label2
  if(is_null(labels)) return(names_to_lnames(.design, names))
  dict <- setNames(V(.design)$label2, V(.design)$name)
  res <- unname(dict[names])
  if(any(is.na(res))) return(names_to_lnames(.design, names))
  res
}

#' Get the treatment variables for every unit
#'
#' @return A named list where names are the unit variable name and
#'   element is the character vector with the treatment factors applied
unit_to_trts <- function(.design, unit = NULL, trts = NULL, type = c("var", "level")) {
  type <- match.arg(type)
  .graph <- .design$graph
  switch(type,
         var = {
           tv <- endpoints(.graph, "t2v", "var")
           split(tv$from, tv$to)
         },
         level = {
           sgraph <- subset(.graph, vname %in% c(unit, trts), .vtype = "level")
           tvl <- endpoints(sgraph, "t2vmay", "level")
           split(tvl$from, tvl$to)
         })
}

#' @export
rep_rpbl <- function(.design, times, length.out) {
  res <- .design
  if(times > 1) {
    for(i in seq(times - 1)) {
      res <- rbind(res, .design)
    }
  }
  res[1:length.out, ]
}


#' @importFrom dae designRandomize
#' @importFrom igraph add_edges delete_edges
randomise_trts_internal <- function(.design) {
  unit_to_trts_list <- unit_to_trts(.design)
  units_with_trts_applied <- names(unit_to_trts_list)
  .graph <- .design$graph
  for(i in seq_along(unit_to_trts_list)) {
    unit <- units_with_trts_applied[i]
    trts <- unit_to_trts_list[[i]]
    ancestors <- find_unit_ancestors(.graph, unit)
    units <- c(unit, ancestors)
    sgraph <- subset(.graph, vname %in% units)
    # need to fix below
    reps <- replicabble(.design, unit, trts)

    units_df <- as_data_frame(serve_units(sgraph))
    units_nesting_df <- units_df
    for(aunit in units) {
      vnames <- vertex_level_names(aunit, units_df[[aunit]])
      units_nesting_df[[aunit]] <- as.factor(names_to_nesting_names(sgraph, vnames))
    }

    nunit <- nrow(units_df)
    ntrt <- nrow(reps)

    trts_df <- as.data.frame(rep_rpbl(reps[sample(nrow(reps)),trts, drop = FALSE],
                   times = ceiling(nunit / ntrt),
                   length.out = nrow(units_df)))

    nesting <- structure(lapply(units,
                                function(aunit) find_unit_parents(sgraph, aunit)),
                         names = units)
    nesting <- remove_nulls(nesting)

    if(!is_empty(nesting)) {
      des <- designRandomize(allocated = trts_df,
                             recipient = units_nesting_df,
                             nested.recipients = nesting)
    } else {
      des <- designRandomize(allocated = trts_df,
                             recipient = units_nesting_df)

    }

    for(atrt in trts) {
      es <- match_edge_seq(.design$graph, des[[atrt]], vertex_level_names(unit, units_df[[unit]]))
      .graph <- add_edges(.graph, es, attr = edge_attr_opt("t2v"))
    }
    .graph <- reinstate_graph_attrs(.graph, .design$graph)
  }

  .graph <- delete_edges(.graph, which(E(.graph)$etype == "t2vmay"))

  reinstate_graph_attrs(.graph, .design$graph)
}
