#' Make an edge sequence for every pair from two vectors
#'
#' Given two vectors of node names, return the edge sequence that connects
#' every node from first vector to the second vector.
#'
#' @param .data An edibble graph.
#' @param vnames_from A character vector of vertex names.
#' @param vnames_to A character vector of vertex names.
#' @family edge-seq
#' @return An edge sequence (vector of vertex indices) that connects every node
#' from first vector to every node in the second vector.V()
#' @export
cross_edge_seq <- function(.data, vnames_from, vnames_to) {
  vindex_from <- which(V(.data)$name %in% vnames_from)
  vindex_to <- which(V(.data)$name %in% vnames_to)
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
#' @export
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
  if(length(vnames) > 0) {
    vindex <- map_int(vnames, function(x) which(V(.data)$name==x))
    n <- length(vindex)
    data.frame(vindex[1:(n - 1)], vindex[2:n]) %>%
      t() %>%
      as.vector()
  }
}

#' Specify the nesting structure for units
#'
#' @param x A vector with the levels of the list with the name corresponding to
#'   the name of the unit to nest under.
#' @param ... a single number OR a sequence of two-sided formula where the left-hand side corresponds to the level of `unit`
#' and the right-hand side is the levels of the new unit OR the number of levels
#' @param .vname an optional variable name
#' @return Returns an isolated graph
#' @export
nested_in <- function(.var, ..., .vname) {
  parent_name <- as_string(quo_get_expr(enquo(.var)))
  parent_vlevels <- .var
  parent_nlevels <- length(parent_vlevels)

  g <- igraph::add_vertices(igraph::make_empty_graph(),
                            nv = parent_nlevels,
                            name = parent_vlevels,
                            ltype = "parent")

  dots <- enquos(...)
  levels_left <- parent_vlevels

  for(i in seq_along(dots)) {
    expr <- quo_get_expr(dots[[i]])
    # nested_in(unit, 2)
    if(is.numeric(expr) & length(dots)==1L) {
      nrep <- expr
      n <- nrep * parent_nlevels
      child_levels <- traits_levels(prefix = ifelse(missing(.vname), "", .vname),
                                    size = n)
      child_non_distinct_levels <- traits_levels(prefix = ifelse(missing(.vname), "", .vname),
                                                 size = nrep)
      # I need to change how this is done.
      # I suspect this is the part that's slowing it down a lot.
      for(i in seq_along(parent_vlevels)) {
        subchild_vlevels <- paste0(.vname, ":", child_levels[seq((i - 1) * nrep + 1, i * nrep)])

        g <- igraph::add_vertices(g, nv = length(subchild_vlevels),
                                  name = subchild_vlevels,
                                  label2 = child_non_distinct_levels,
                                  ltype = "child", group = i)
        g <- igraph::add_edges(g,
                            cross_edge_seq(g, parent_vlevels[i], subchild_vlevels),
                            attr = edge_attr_opt("l2l"))

      }

      g <- igraph::set_graph_attr(g, "parent_name", parent_name)

      class(g) <- c("lkbl_graph", class(g))
      return(g)
    }

    # nested_in(unit, . ~ 2)
    # support for single nesting unit only
    lhs <- f_lhs(expr)
    rhs <- f_rhs(expr)
    erhs <- eval(rhs)
    if(is_symbol(lhs, name = ".")) {
      df <- set_lkbl(df, parent_vlevels %in% levels_left, erhs)
    } else {
      elhs <- eval(lhs)
      if(is.numeric(elhs)) {
        df <- set_lkbl(df, elhs, erhs)
        levels_left <- setdiff(levels_left, parent_vlevels[elhs])
      } else if(is.character(elhs)) {
        df <- set_lkbl(df, parent_vlevels %in% elhs, erhs)
        levels_left <- setdiff(levels_left, elhs)
      }
    }
  }
  return(df)

}
