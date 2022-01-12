
#' Simulate records
#'
#' @param .data An edibble table or data frame
#' @param ... A name-value pair.
#'
#' @export
simulate_rcrds <- function(.data, ..., .censor = NA) {
  out <- simulate(.data, ...)
  srcrds <- names(list2(...))
  des <- edbl_design(.data)
  vrcrds <- names(des$validation)
  mrcrds <- srcrds[srcrds %in% vrcrds]
  if(length(mrcrds)) {
    for(arcrd in mrcrds) {
      type <- des$validation[[arcrd]]$record
      operator <- des$validation[[arcrd]]$operator
      value <- des$validation[[arcrd]]$value
      if(type!="numeric") abort("Only simulation of numeric value supported")
      y <- out[[arcrd]]
      out[[arcrd]] <- switch(operator,
                             "greaterThan" = return_value(y, y > value, .censor),
                             "greaterThanOrEqual" = return_value(y, y >= value, .censor),
                             "equal" = return_value(y, y == value, .censor),
                             "between" = return_value(y, y > value[1] & y < value[2], .censor),
                             "lessThanOrEqual" = return_value(y, y <= value, .censor),
                             "lessThan" = return_value(y, y < value, .censor))
    }
  }
  out
}

return_value <- function(y, ind, censor) {
  y[!ind] <- censor
  y
}
