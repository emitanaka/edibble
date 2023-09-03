#' Serve edibble table
#' @description
#' This converts an edibble graph object to a data frame called edibble.
#' This function should be used when the design is in the final form
#' (or close to the final form). The table can only be formed when the
#' variables can be reconciled, otherwise it will be a data frame with
#' zero rows.
#'
#' @inheritParams set_units
#' @param use_labels To show the labels instead of names.
#' @param fail What to do when failing to convert graph to table.
#' @return An `edbl` data frame with columns defined by vertices and
#' rows displayed only if the vertices are connected and reconcile for output.
#' @family user-facing functions
#' @examples
#' design("Completely Randomised Design") %>%
#'   set_units(unit = 28) %>%
#'   set_trts(trt = 6) %>%
#'   allot_trts(trt ~ unit) %>%
#'   assign_trts("random", seed = 521) %>%
#'   serve_table()
#' @export
serve_table <- function(.edibble, use_labels = FALSE, fail = c("error", "warn", "ignore"),  .record = TRUE) {
  prov <- activate_provenance(.edibble)
  fail <- match.arg(fail)
  if(.record) prov$record_step()

  if(!prov$is_connected) {
    if(fail == "error") abort("The graph cannot be converted to a table format.")
    if(fail == "warn") warn("The graph cannot be converted to a table format.")
    lout <- serve_vars_not_reconciled(prov)
  } else {
    roles <- prov$fct_role()
    lunit <- ltrt <- lrcrd <- list()
    if("edbl_unit" %in% roles) {
      lunit <- prov$serve_units(return = "value")
    } else {
      abort("At least one `unit` factor needs to be set.")
    }
    if("edbl_trt" %in% roles) ltrt <- prov$serve_trts(return = "value")
    if(length(lunit) | length(ltrt)) {
      if("edbl_rcrd" %in% roles) lrcrd <- prov$serve_rcrds(return = "value")
      lout <- c(lunit, ltrt, lrcrd)
    } else {
      lout <- serve_vars_not_reconciled(prov)
    }
  }

  namesv <- prov$fct_names()
  if(use_labels) {
    translate <- stats::setNames(prov$lvl_nodes$label, prov$lvl_nodes$name)
    # FIXME: it loses the classes when this is done
    lout <- lapply(lout, function(.x) translate[.x])
  }

  new_edibble(lout[namesv], design = .edibble)
}









# Returns list of edibble variables
serve_vars_not_reconciled <- function(prov) {
  namesv <- prov$fct_names()
  res <- lapply(namesv,
                function(avar) {
                  # FIXME: labels should not necessary be character?
                  new_edibble_fct(levels = prov$lvl_values(fid = prov$fct_id(name = avar)),
                                  name = avar,
                                  class = prov$fct_role(id = prov$fct_id(name = avar)))
                })
  names(res) <- namesv
  res
}





