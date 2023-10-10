#' Serve edibble table
#' @description
#' This converts an edibble graph object to a data frame called edibble.
#' This function should be used when the design is in the final form
#' (or close to the final form). The table can only be formed when the
#' variables can be reconciled, otherwise it will be a data frame with
#' zero rows.
#'
#' @inheritParams set_units
#' @param label_nested The columns to show nested labels (if available). Tidyselect compatible.
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
#' @import tidyselect
#' @export
serve_table <- function(.edibble, label_nested, fail = c("error", "warn", "ignore"),  .record = TRUE) {
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

  dots <- eval_select(enexpr(label_nested), lout)
  if(length(dots)) {
    lnodes <- lvl_nodes(.edibble)
    for(aname in names(dots)) {
      ln <- lnodes[[aname]]
      if("label" %in% colnames(ln)) {
        res <- ln$label[match(lout[[aname]], ln$value)]
        class(res) <- class(lout[[aname]])
        attributes(res) <- attributes(lout[[aname]])
        lout[[aname]] <- res
      }
    }
  }

  namesv <- prov$fct_names()
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





