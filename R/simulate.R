
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

  dots <- enexprs(..., .named = TRUE, .homonyms = "error", .check_assign = TRUE)
  nms <- names(dots)
  if(length(nms) == 0) return(.data)

  # anything starting with a "." is assumed to be a placeholder name
  ph_nms <- nms[grepl("^[.]", nms)]
  rcrd_nms <- setdiff(nms, ph_nms)

  # this works but it doesn't work when moved outside...
  simulate_env <- env(n = function(...) {
    loc <- eval_select(c(...), data = .data)
    if(length(loc) == 0) return(nrow(.data))
    v <- map_chr(1:nrow(.data), paste(.data[loc][i, ], collapse = "_"))
    length(unique(v))
  })

  if(length(rcrd_nms)) {
    prov$rcrd_exists(name = rcrd_nms)
    for(arcrd in rcrd_nms) {
      # simulate_env$.datarcrd <- FILL
      f <- eval_tidy(dots[[arcrd]], data = .data, env = simulate_env)
      y <- eval_tidy(f(), data = .data, env = simulate_env)
      stopifnot(length(y)==nrow(.data))
      # a check should be placed to ensure that the measurements are equal
      # among the same units
      prov$set_simulate(name = arcrd, process = f, rcrds = arcrd)
    }
  }

  if(length(ph_nms)) {
    for(anm in ph_nms) {
      # e$.datarcrd <- FILL
      f <- eval_tidy(dots[[anm]], data = .data, env = simulate_env)
      Y <- eval_tidy(f(), data = .data, env = simulate_env)
      stopifnot(nrow(Y)==nrow(.data))
      fnames <- colnames(Y)
      prov$rcrd_exists(name = fnames)
      # TODO: with other checks
      prov$set_simulate(name = anm, process = f, rcrds = fnames)
    }
  }

  return_edibble_with_graph(.data, prov)
}




#' This is a helper function to set the parameter values
#'
#' @param ... A series of name-value pair that are inputs used for the
#'  simulation process.
#' @param .censor The value to censor if it outside the valid values. If the
#'  value has a lower and upper bound then it should be a vector of size 2. Use
#'  -Inf or Inf if you don't want to censor either value. You can use a list if
#'  you want a different censoring for different records.
#' @param .aggregate The function for aggregation if the response values differ
#'  within the same unit level for the record. Use `NA` if you don't want to aggregate.
#'  By default, it will get the mean or mode depending on the encoding
#'  (numeric is mean, mode for character or factor), or if absent,
#'  based on returned encoding. It can be a named list where the names correspond to
#'  the record name and the values corresponding to a function.
#' @export
with_params <- function(..., .censor = NA, .aggregate = NULL) {
  params <- list2(...)
  list(params = params, censor = .censor, aggregate = .aggregate)
}

#' Simulate records
#'
#' @param .data
#' @param ... A name-value pair where the name should correspond to the names
#' used in the [simulate_process()]. The value should be returned from calling
#' [with_params()].
#' @param .seed An optional seed value.
#' @export
simulate_rcrds <- function(.data, ..., .seed = NULL) {
  dots <- list2(...)
  prov <- activate_provenance(.data)
  prov$save_seed(.seed, type = "simulate_rcrds")
  prnames <- names(dots)
  #srcrds_list <- map(prnames, function(x) prov$get_simulate(x)$rcrds)
  #srcrds <- unlist(srcrds_list)
  #if(any(duplicated(srcrds))) abort(paste0("You are trying to simulate ", .combine_words(srcrds[duplicated(srcrds)]), " in multiple processes."))

  # this works but it doesn't work when moved outside...
  simulate_env <- env(n = function(...) {
    loc <- eval_select(c(...), data = .data)
    if(length(loc) == 0) return(nrow(.data))
    v <- map_chr(1:nrow(.data), paste(.data[loc][i, ], collapse = "_"))
    length(unique(v))
  })

  for(aprocess in prnames) {
    process <- prov$get_simulate(aprocess)$process
    srcrds <- prov$get_simulate(aprocess)$rcrds
    y <- eval_tidy(do.call(process, dots[[aprocess]]$params),
                   data = .data, env = simulate_env)

    if(grepl("^[.]", aprocess)) {
      for(acol in colnames(y)) {
        .data[[acol]] <- get_rcrd_values(acol, prov, dots[[aprocess]]$aggregate,
                                         .data, y[[acol]], dots[[aprocess]]$censor)
      }
    } else {
      # there should be only one record if it is not a process name
      arcrd <- srcrds[1]
      .data[[arcrd]] <- get_rcrd_values(arcrd, prov, dots[[aprocess]]$aggregate,
                                        .data, unname(y), dots[[aprocess]]$censor)
    }
  }
  .data
}

get_rcrd_values <- function(rname, prov, aggfn, .data, y, censor) {
  vrcrds <- prov$get_validation("rcrds")
  uid <- prov$mapping_to_unit(id = prov$fct_id(name = rname))
  uname <- prov$fct_names(id = uid)
  agg_fn <- get_aggregate_function(aggfn,
                                   rname,
                                   get_record_type(vrcrds, rname),
                                   class(y))
  vals <- aggregate_values(y, .data[[uname]], agg_fn)
  if(rname %in% names(vrcrds)) {
    if(is.list(censor) && rname %in% names(censor)) {
      get_censored_value(vals, vrcrds[[rname]], censor[[rname]])
    } else {
      get_censored_value(vals, vrcrds[[rname]], censor)
    }
  } else {
    vals
  }
}

get_censored_value <- function(y, valid, censor) {
  type <- valid$record
  if(type=="numeric") {
    value <- valid$value
    switch(valid$operator,
           "greaterThan" = return_censor_value(y, y > value, censor),
           "greaterThanOrEqual" = return_censor_value(y, y >= value, censor),
           "equal" = return_censor_value(y, y == value, .censor),
           "between" = return_censor_value(y, y > value[1] & y < value[2], censor, value),
           "lessThanOrEqual" = return_censor_value(y, y <= value, censor),
           "lessThan" = return_censor_value(y, y < value, censor))
  } else if(type=="factor") {
    ind <- y %in% valid$values
    if(is_function(censor)) {
      y[!ind] <- censor(y[ind])
    } else {
      y[!ind] <- censor
    }
    y
  }
}

return_censor_value <- function(y, ind, censor, values = NULL, ind_censor = ind) {
  if(is_null(values)) {
    if(is_function(censor)) {
      y[!is.na(ind) & !ind] <- censor(y[!is.na(ind_censor) & ind_censor])
    } else if(!is.infinite(censor)) {
      y[!is.na(ind) & !ind] <- censor
    }
  } else {
    if(length(censor) == 2) {
      y <- return_censor_value(y, y > values[1], censor[[1]], ind_censor = ind)
      y <- return_censor_value(y, y < values[2], censor[[2]], ind_censor = ind)
    } else {
      y <- return_censor_value(y, y > values[1], censor, ind_censor = ind)
      y <- return_censor_value(y, y < values[2], censor, ind_censor = ind)
    }
  }
  y
}


get_record_type <- function(vrcrds, arcrd) {
  ifelse(arcrd %in% names(vrcrds), vrcrds[[arcrd]]$record, NULL)
}

# note: removes NA
get_mode <- function(x) {
  x <-  x[!is.na(x)]
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}


get_aggregate_function <- function(agg, rcrd_name = NULL, rcrd_type = NULL, return_type = NULL) {
  if(is.list(agg)) agg <- agg[[rcrd_name]]
  if(!is_function(agg) && !is.null(agg) && is.na(agg)) return(NA)
  if(!is_null(agg)) return(agg)
  if(!is.null(rcrd_type) && rcrd_type == "factor") return(get_mode)
  if(any(return_type %in% c("factor", "character"))) return(get_mode)
  return(function(x) mean(x, na.rm = TRUE))
}

aggregate_values <- function(y, group, fn) {
 if(is_function(fn)) {
   agg <- tapply(y, group, fn)
   as.vector(unname(agg[as.character(group)]))
 } else if(is.na(fn)) {
   y
 }
}

