#' Set the treatment variables
#'
#' @description
#' This function add a special class, called `edbl_trt`, of edibble variables.
#'
#' @section Definition of _treatment_:
#' The word _treatment_ is sometimes used to refer to one of these variables.
#' When there are more than one treatment variables then this unfortunately
#' confuses whether treatment refers to the variable or the combination of
#' all treatment variables.
#'
#' Treatment is the whole description of what is applied in an experiment.
#'
#' @inheritParams set_units
#' @family user-facing functions
#' @examples
#'
#' design() %>%
#'   set_trts(pesticide = c("A", "B", "C"),
#'            dosage = c(0, 10, 20, 30, 40))
#'
#' @return An edibble design.
#' @export
set_trts <- function(.edibble = design(), ...,
                     .name_repair = c("check_unique", "unique", "universal", "minimal"),
                     .record = TRUE) {
  prov <- activate_provenance(.edibble)
  if(.record) prov$record_step()
  set_fcts(.edibble, ..., .name_repair = .name_repair, .class = "edbl_trt")
}





#' @importFrom pillar pillar_shaft new_pillar_shaft_simple
#' @export
pillar_shaft.edbl_trt <- function(x, ...) {
  out <- format(x)
  new_pillar_shaft_simple(out, align = "right", min_width = 10)
}

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.edbl_trt <- function(x, ...)  {
  paste0("trt(", number_si_prefix(nlevels(x)), ")")
}

#' @importFrom vctrs vec_ptype_full
#' @export
vec_ptype_full.edbl_trt <- function(x, ...) paste0("trt(", nlevels(x), ")")


#' Treatments table
#'
#' @param .edibble An edibble table
#' @export
trts_table <- function(.edibble) {
  prov <- activate_provenance(.edibble)
  tids <- prov$trt_ids
  prov$make_trts_table(id = tids, return = "value")
}

# TODO
# add_trts()
