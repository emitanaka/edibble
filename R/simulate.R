
#' Simulation form
#'
#' @param .data An edibble table.
#' @param ... A name-value pair where the name should correspond to either the record name
#'   that you are simulating or a process name if the return object is a data frame with
#'   columns corresponding to the name of the records. The value must be a function with
#'   set default arguments. The return object of this function should be either a
#'   vector or a data frame
#'   with the column names corresponding to the record names. The size should correspond to
#'   the number of columns.
#'
#' @details
#' When creating a function, internally you can refer to any of the factors without referring
#' to the actual data. The data referred to is expected to be from the full data. If the data
#' related to unit of the record, then you can use the special syntax `.datarcrd` to refer to it
#' internally. Like in tidyverse, syntax `.data` is reserved for the full data and `.env` can be
#' used to refer to environment variables.
#'
#' You can use the syntax `n()` to refer to `nrow(.data)` or `n(fct)` where `fct` corresponds to
#' unquoted factor name. The return value will be the number of the observed number of levels of factor `fct`
#' in the data. For `n(fct1, fct2)` it will return the observed number of distinct interaction levels for `fct1`
#' and `fct2`.
#'
#' Note that you can actually put as many process as you like if you use a process name (starting with a dot),
#' even if this is for the same record factor.
#'
#'
#'
#' @export
simulate_process <- function(.data, ...) {
  not_edibble(.data)
  prov <- activate_provenance(.data)

  dots <- enquos(..., .named = TRUE, .homonyms = "error", .check_assign = TRUE)
  nms <- names(dots)
  if(length(nms) == 0) return(.data)

  # anything starting with a "." is assumed to be a placeholder name
  ph_nms <- nms[grepl("^[.]", nms)]
  rcrd_nms <- setdiff(nms, ph_nms)

  e <- caller_env()
  e$n <- function(...) {
    loc <- eval_select(c(...), data = .data)
    if(length(loc) == 0) return(nrow(.data))
    v <- map_chr(1:nrow(.data), paste(.data[loc][i, ], collapse = "_"))
    length(unique(v))
  }

  if(length(rcrd_nms)) {
    prov$rcrd_exists(name = rcrd_nms)
    for(arcrd in rcrd_nms) {
      # e$.datarcrd <- FILL
      f <- eval_tidy(dots[[arcrd]], data = .data, env = e)
      y <- eval_tidy(f(), data = .data, env = e)
      stopifnot(length(y)==nrow(.data))
      # a check should be placed to ensure that the measurements are equal
      # among the same units
      prov$set_simulate(name = arcrd, process = f)
    }
  }

  if(length(ph_nms)) {
    for(anm in ph_nms) {
      # e$.datarcrd <- FILL
      f <- eval_tidy(dots[[anm]], data = .data, env = e)
      Y <- eval_tidy(f(), data = .data, env = e)
      stopifnot(nrow(Y)==nrow(.data))
      fnames <- colnames(Y)
      prov$rcrd_exists(name = fnames)
      # TODO: with other checks
      prov$set_simulate(name = anm, process = f)
    }
  }

  return_edibble_with_graph(.data, prov)
}


#' Simulate records
#'
#' @param .data An edibble table or data frame
#' @param ... A name-value pair.
#' @param .censor The value to replace if the value does not lie within
#'  valid values.
#' @param .seed An optional seed value.
#'
#' @export
simulate_rcrds <- function(.data, ..., .censor = NA, .seed = NULL) {
  if(!requireNamespace("simulate")) {
    stop("Please install the `simulate` package to use this function. Only the development version is available for now so please run `remotes::install_github('emitanaka/simulate')`.")
  }
  out <- simulate::simulate(.data, ..., .seed = .seed)
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
