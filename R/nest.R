#' Specify the nesting structure for units
#'
#' @param x The name of the parent unit to nest under.
#' @param ... a single number OR a sequence of two-sided formula where the
#' left-hand side corresponds to the name of the level (or the level number) of `x`
#' and the right-hand side is an integer specifying the number of levels nested under the
#' corresponding levels.
#' @param prefix The prefix of the label.
#' @param suffix The suffix of the label.
#' @param distinct whether the child variables are distinct in the levels that it is nesting
#' @param leading0 Whether there should be a leading 0 if labels are made.
#' @param sep A separator added between prefix and the number if prefix is empty.
#' @param traits A named vector where names and values correspond to attribute names and values of the variable.
#' @seealso See [set_units()] for examples of how to use this.
#' @export
nested_in <- function(x, ..., prefix = "", suffix = "",
                      distinct = TRUE, leading0 = FALSE,
                      sep = edibble_labels_opt("sep"),
                      traits = NULL) {
  if(prefix=="") prefix <- paste0(caller_env()$.top_env$.vname, sep)
  parent_name <- as_string(enexpr(x))
  parent_vlevels <- x

  child_levels <- nestr::nest_in(factor(parent_vlevels, levels = parent_vlevels),
                                 ...,
                                 prefix = prefix,
                                 suffix = suffix,
                                 distinct = distinct,
                                 leading0 = leading0,
                                 compact = FALSE,
                                 keyname = parent_name)
  ltraits <- as.list(traits)
  attributes(child_levels) <- c(ltraits, attributes(child_levels))
  class(child_levels) <- c("nst_levels", class(child_levels))
  return(child_levels)
}

#' Get the nesting structure for the units
#'
#' @param design An edibble design
#' @return Return a named list. Only shows the direct parent.
#' @export
nesting <- function(design) {
  uids <- unit_ids(design)
  ndf <- subset(vgraph(design)$edges, from %in% uids & to %in% uids)
  split(ndf$var_from, ndf$var_to)
}


