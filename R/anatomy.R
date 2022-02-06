
#' Anatomy of the design
#'
#' This is a convenient wrapper for `dae::designAnatomy` where the `formulae` structure is
#' automatically determined by the unit and treatment structure specified in `edibble` system.
#' Note: the computation may be long if the design is quite complicated or there are many units.
#'
#' @param .design A complete edibble design object or edibble table.
#' @param ... Any other arguments parsed to `dae::designAnatomy`.
#' @export
anatomy <- function(.design, ...) {
  des <- edbl_design(.design)
  tab <- edbl_table(.design)
  trt_str <- as.formula(paste0("~", paste0(trt_names(des), collapse = "*")))
  out <- dae::designAnatomy(list(unit = des$anatomy, trt = trt_str), data = tab, ...)
  structure(out,
            class = c("des_anatomy", class(out)))
}


#' @export
print.des_anatomy <- function(x, ...) {
  print(summary(x, ...))
}
