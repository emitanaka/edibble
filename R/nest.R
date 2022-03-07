#' Specify the nesting structure for units
#'
#' @param x The name of the parent unit to nest under.
#' @param ... a single number OR a sequence of two-sided formula where the
#' left-hand side corresponds to the name of the level (or the level number) of `x`
#' and the right-hand side is an integer specifying the number of levels nested under the
#' corresponding levels.
#' @param prefix The prefix of the label.
#' @param suffix The suffix of the label.
#' @param leading0 Whether there should be a leading 0 if labels are made.
#' @param sep A separator added between prefix and the number if prefix is empty.
#' @param attrs A named vector where names and values correspond to attribute names and values of the variable, or
#'   a data frame.
#' @seealso See [set_units()] for examples of how to use this.
#' @export
nested_in <- function(x, ..., prefix = "", suffix = "",
                      leading0 = FALSE,
                      sep = edibble_labels_opt("sep"),
                      attrs = NULL) {
  top <- caller_env()$.top_env
  if(is.null(top$.fname)) abort("The `nested_in` function must be used within `set_units` function.")
  prep <- top$prep
  vlevs <- prep$fct_levels()
  if(prefix=="") prefix <- paste0(top$.fname, sep)
  parent_name <- as_string(enexpr(x))
  parent_vlevels <- vlevs[[parent_name]]
  dots <- list2(...)
  args <- list()
  for(.x in dots) {
    ind <- is_cross_levels(.x) | is_formula(.x, lhs = FALSE)
    if(ind) {
      if(is_formula(.x, lhs = FALSE)) {
        vars <- rownames(attr(terms(.x), "factors"))
      } else {
        vars <- .x
      }
      child_lvls_by_parent <- map(vars, function(.var) {
        out <- serve_units(select_units(prep, c(.var, parent_name)))
        split(out[[.var]], out[[parent_name]])
      })
      names(child_lvls_by_parent) <- vars
      cross_lvls_by_parent <- map(parent_vlevels, function(.lvl) {
        out <- setNames(map(vars, function(.var) child_lvls_by_parent[[.var]][[.lvl]]),
                        vars)
        do.call("expand.grid", out)
      })
      names(cross_lvls_by_parent) <- parent_vlevels
      args <- c(args, map(names(cross_lvls_by_parent), function(.parent)
        as.formula(paste0('"', .parent, '"', " ~ ", nrow(cross_lvls_by_parent[[.parent]])))))
      attr(args, "parents") <- cross_lvls_by_parent
    } else {
      args <- c(args, list(.x))
    }
  }

  child_levels <- nestr::nest_in(parent_vlevels,
                                 !!!args,
                                 prefix = prefix,
                                 suffix = suffix,
                                 distinct = TRUE,
                                 leading0 = leading0,
                                 compact = FALSE,
                                 keyname = parent_name)
  child_labels <- nestr::nest_in(parent_vlevels,
                                 !!!args,
                                 prefix = prefix,
                                 suffix = suffix,
                                 distinct = FALSE,
                                 leading0 = leading0,
                                 compact = FALSE,
                                 keyname = parent_name)
  lattrs <- as.list(attrs)
  attributes(child_levels) <- c(lattrs, attributes(child_levels),
                                list(parents = attr(args, "parents"),
                                     labels = child_labels))
  class(child_levels) <- c("nest_lvls", class(child_levels))
  return(child_levels)
}

#' Get the nesting structure for the units
#'
#' @param design An edibble design
#' @return Return a named list. Only shows the direct parent.
#' @export
nesting_structure <- function(design) {
    prep <- cook_design(design)
    uids <- prep$unit_ids
    fedges <- prep$fct_edges
    ndf <- fedges[fedges$from %in% uids & fedges$to %in% uids & !fedges$type %in% c("depends", "cross"),]
    from <- prep$fct_names(ndf$from)
    to <- prep$fct_names(ndf$to)
    split(from, to)
}


