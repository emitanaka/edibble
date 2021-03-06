#' Make an edge sequence for every pair from two vectors
#'
#' Given two vectors of node names, return the edge sequence that connects
#' every node from first vector to the second vector.
#'
#' @param .graph An edibble graph.
#' @param vnames_from A character vector of vertex names.
#' @param vnames_to A character vector of vertex names.
#' @family edge-seq
#' @return An edge sequence (vector of vertex indices) that connects every node
#' from first vector to every node in the second vector.V()
cross_edge_seq <- function(.graph, vnames_from, vnames_to) {
  vindex_from <- which(V(.graph)$name %in% vnames_from)
  vindex_to <- which(V(.graph)$name %in% vnames_to)
  as.vector(t(expand.grid(vindex_from, vindex_to)))
}

#' Make an edge sequence from two vectors of vertex names
#'
#' For a pair of vectors of vertex names in `.data`, get the edge sequence
#' that connects the vertices in the matching index. For example, if the graph
#' has 4 nodes named A, B, C and D, then `vnames_from = C("A", "B")` and
#' `vnames_to = c("C", "D")` means it returns the edge sequence so that node A
#' will connect to node C and node B to node D.
#'
#' @param .data An edibble graph.
#' @param vnames_from The vertex name to start the edge.
#' @param vnames_to The vertex name to end the edge.
#' @return An edge sequence.
#' @family edge-seq
match_edge_seq <- function(.data, vnames_from, vnames_to) {
  vindex_from <- map_int(vnames_from, function(vname) var_index(.data, vname))
  vindex_to <- map_int(vnames_to, function(vname) var_index(.data, vname))
  as.vector(t(data.frame(vindex_from, vindex_to)))
}

#' Get the edge sequence that draws the apth.
#'
#' @param .data An edibble graph.
#' @param vnames The vertex names.
#' @export
path_seq <- function(.data, vnames) {
  if(!is_empty(vnames)) {
    vindex <- match(vnames, V(.data)$name)
    n <- length(vindex)
    data.frame(vindex[1:(n - 1)], vindex[2:n]) %>%
      t() %>%
      as.vector()
  }
}

#' Specify the nesting structure for units
#'
#' @param .unit The name of the parent unit to nest under.
#' @param ... a single number OR a sequence of two-sided formula where the
#' left-hand side corresponds to the name of the level (or the level number) of `.unit`
#' and the right-hand side is an integer specifying the number of levels nested under the
#' corresponding levels.
#' @param .vname the child variable name
#' @return Returns an isolated graph
#' @seealso See [set_units()] for examples of how to use this.
#' @importFrom rlang enexpr enquos is_symbol as_string f_lhs f_rhs
#' @importFrom igraph add_vertices make_empty_graph add_edges set_graph_attr
#' @export
nested_in <- function(.unit, ..., .vname) {
  parent_name <- as_string(enexpr(.unit))
  parent_vlevels <- .unit
  parent_nlevels <- length(parent_vlevels)

  g <- add_vertices(make_empty_graph(),
                    nv = parent_nlevels,
                    name = parent_vlevels,
                    ltype = "parent")

  dots <- enquos(...)
  levels_left <- parent_vlevels
  reps <- vector(mode = "integer", length = parent_nlevels)

  for(i in seq_along(dots)) {
    expr <- quo_get_expr(dots[[i]])
    # nested_in(unit, 2)
    if(is.numeric(expr) & length(dots)==1L) {
      nrep <- expr
      reps <- rep(nrep, parent_nlevels)
    } else {

      # nested_in(unit, . ~ 2)
      # support for single nesting unit only
      lhs <- f_lhs(expr)
      rhs <- f_rhs(expr)
      nc <- eval(rhs) # only numeric value supported for now
      if(is_symbol(lhs, name = ".")) {
        np_left <- length(levels_left)
        reps[parent_vlevels %in% levels_left] <- nc
      } else {
        elhs <- eval(lhs)
        igroup <- switch(class(elhs),
                         integer = elhs,
                         numeric = elhs,
                         character = match(vertex_level_names(parent_name, elhs), parent_vlevels),
                         abort("LHS currently needs to be integer, numeric, or character"))
        reps[igroup] <- nc
        levels_left <- setdiff(levels_left, parent_vlevels[igroup])
      }
    }
  }

  child_levels <- traits_levels(prefix = .vname,
                                size = sum(reps))
  child_levels <- vertex_level_names(.vname, child_levels)
  child_non_distinct_levels <- unname(unlist(lapply(reps,
                                       function(r) {
                                             traits_levels(prefix = .vname,
                                                           size = r)
                                       })))
  group <- rep(1:parent_nlevels, times = reps)

  g <- add_vertices(g,
                    nv = length(child_levels),
                    name = child_levels,
                    label2 = child_non_distinct_levels,
                    ltype = "child",
                    group = group)
  marks <- c(1, 1 + cumsum(reps))
  es <- unname(unlist(lapply(seq_along(parent_vlevels), function(i) {
    cross_edge_seq(g, parent_vlevels[i], child_levels[seq(marks[i], marks[i + 1] - 1)])
  })))
  g <- add_edges(g, es, attr = edge_attr_opt("l2l"))
  g <- set_graph_attr(g, "parent_name", parent_name)
  class(g) <- c("lkbl_graph", class(g))
  return(g)
}

