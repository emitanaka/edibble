
#' Anatomy of the design
#'
#' This is a convenient wrapper for `dae::designAnatomy` where the `formulae` structure is
#' automatically determined by the unit and treatment structure specified in `edibble` system.
#' Note: the computation may be long if the design is quite complicated or there are many units.
#'
#' @param .edibble A complete edibble design object or edibble table.
#' @param ... Any other arguments parsed to `dae::designAnatomy`.
#' @examples
#' split <- takeout(menu_split(t1 = 3, t2 = 2, r = 2))
#' design_anatomy(split)
#' @return An object of class "des_anatomy".
#' @export
design_anatomy <- function(.edibble, ...) {
  des <- edbl_design(.edibble)
  tab <- edbl_table(.edibble)
  prov <- activate_provenance(des)
  trt_str <- stats::as.formula(paste0("~", paste0(prov$trt_names(), collapse = "*")))
  out <- dae::designAnatomy(list(unit = des$anatomy, trt = trt_str), data = tab, ...)
  structure(out,
            class = c("des_anatomy", class(out)))
}

#' @export
print.des_anatomy <- function(x, ...) {
  print(summary(x, ...))
}


add_anatomy <- function(anatomy, fresh, name, role) {
  if(role=="edbl_unit") {
    if(is.null(anatomy)) {
      anatomy <- stats::as.formula(paste0("~", name))
    } else {
      term <- ifelse(inherits(fresh, "nest_lvls"),
                     paste0(attr(fresh, "keyname"), "/", name),
                     name)
      anatomy <- stats::update(anatomy,
                             stats::as.formula(paste0("~ . + ", term)), evaluate = FALSE)
    }
  }
  anatomy
}
