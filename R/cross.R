
#' Specify the units to cross to index a new unit
#'
#' `crossed_by(A, B)` is the same as `~A:B` but `crossed_by` offers more control over the names of the new units
#' as well as adding new attributes.
#'
#' @param ... a sequence of units
#' @param attrs Currently not implemented.
#' @examples
#' design("Strip-Plot Design | Strip-Unit Design") %>%
#'   set_units(block = 3,
#'             row = nested_in(block, 7),
#'             col = nested_in(block, 6),
#'             unit = nested_in(block, crossed_by(row, col)))
#' @return An object of class "cross_lvls".
#' @export
crossed_by <- function(..., attrs = NULL) {
  e <- exprs(...)
  structure(as.character(e),
            attrs = attrs, class = "cross_lvls")
}


