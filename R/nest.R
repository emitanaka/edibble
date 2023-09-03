#' Specify the nesting structure for units
#'
#' @param x The name of the parent unit to nest under.
#' @param ... a single number OR a sequence of two-sided formula where the
#' left-hand side corresponds to the name of the level (or the level number) of `x`
#' and the right-hand side is an integer specifying the number of levels nested under the
#' corresponding levels.
#' @seealso See [set_units()] for examples of how to use this.
#' @return A nested level.
#' @examples
#' design("Split-Plot Design | Split-Unit Design") %>%
#'   set_units(mainplot = 60,
#'             subplot = nested_in(mainplot, 10))
#' @export
nested_in <- function(x, ...) {
  top <- caller_env()$.top_env
  if(is.null(top$.fname)) abort("The `nested_in` function must be used within `set_units` function.")
  prov <- top$prov
  vlevs <- prov$fct_levels(return = "value")
  parent_name <- as_string(enexpr(x))
  parent_vlevels <- vlevs[[parent_name]]
  dots <- list2(...)
  args <- list()
  for(.x in dots) {
    if(is_cross_levels(.x) | is_formula(.x, lhs = FALSE)) {
      vars <- if(is_formula(.x, lhs = FALSE)) rownames(attr(stats::terms(.x), "factors")) else .x
      child_lvls_by_parent <- map(vars, function(.var) {
        out <- prov$serve_units(id = prov$fct_id(name = c(.var, parent_name)), return = "value")
        split(out[[.var]], out[[parent_name]])
      })
      names(child_lvls_by_parent) <- vars
      cross_lvls_by_parent <- map(parent_vlevels, function(.lvl) {
        out <- stats::setNames(map(vars, function(.var) child_lvls_by_parent[[.var]][[.lvl]]),
                        vars)
        do.call("expand.grid", out)
      })
      names(cross_lvls_by_parent) <- parent_vlevels
      args <- c(args, map(names(cross_lvls_by_parent), function(.parent)
        stats::as.formula(paste0('"', .parent, '"', " ~ ", nrow(cross_lvls_by_parent[[.parent]])))))
      attr(args, "parents") <- cross_lvls_by_parent
    } else {
      args <- c(args, list(.x))
    }
  }

  child_levels <- nestr::nest_in(parent_vlevels,
                                 !!!args,
                                 prefix = top$.fname,
                                 suffix = "",
                                 distinct = TRUE,
                                 leading0 = TRUE,
                                 compact = FALSE,
                                 keyname = parent_name)
  child_labels <- nestr::nest_in(parent_vlevels,
                                 !!!args,
                                 prefix = top$.fname,
                                 suffix = "",
                                 distinct = FALSE,
                                 leading0 = TRUE,
                                 compact = FALSE,
                                 keyname = parent_name)
  attributes(child_levels) <- c(attributes(child_levels),
                                list(parents = attr(args, "parents"),
                                     labels = child_labels))
  class(child_levels) <- c("nest_lvls", class(child_levels))
  return(child_levels)
}

#' Get the nesting structure for the units
#'
#' @param design An edibble design
#' @examples
#' nesting_structure(takeout(menu_split()))
#' @return Return a named list. Only shows the direct parent.
#' @export
nesting_structure <- function(design) {
    prov <- activate_provenance(design)
    uids <- prov$unit_ids
    fedges <- prov$fct_edges
    ndf <- fedges[fedges$from %in% uids & fedges$to %in% uids, ]
    from <- prov$fct_names(id = ndf$from)
    to <- prov$fct_names(id = ndf$to)
    split(from, to)
}


