
#' Simulation process
#'
#' This function to create and store functions to simulate the records.
#'
#'
#' @param .data An edibble table.
#' @param ... A name-value pair where the name should correspond to either the record name
#'   that you are simulating or a process name if the return object is a data frame with
#'   columns corresponding to the name of the records. The value must be a function with
#'   set default arguments. The return object of this function should be either a
#'   vector or a data frame with the column names corresponding to the record names.
#'   The size should correspond to the number of columns.
#'
#' @details
#' When creating a function, internally you can refer to any of the factors without referring
#' to the actual data. The data referred to is expected to be from the full data.
#' Like in tidyverse, syntax `.data` is reserved for the full data and `.env` can be
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
#' @export
simulate_process <- function(.data, ...) {
  not_edibble(.data)
  prov <- activate_provenance(.data)

  dots <- enexprs(..., .named = TRUE, .homonyms = "error", .check_assign = TRUE)
  nms <- names(dots)
  if(length(nms) == 0) return(.data)

  # anything starting with a "." is assumed to be a placeholder name
  process_nms <- nms[grepl("^[.]", nms)]
  rcrd_nms <- setdiff(nms, process_nms)

  # this works but it doesn't work when moved outside...
  simulate_env <- env(n = function(...) {
    dots <- list2(...)
    if(length(dots) == 0) return(nrow(.data))
    v <- do.call(paste, dots)
    length(unique(v))
  }, result_env = prov$get_simulate_result_env())

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

  if(length(process_nms)) {
    for(anm in process_nms) {
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
#'  you want a different censoring for different records where the name corresponds to
#'  the name of the record. If you want to apply a default value/function for censoring
#'  then use the name ".default". You can use a function instead of a value. The function
#'  may be specified by as a lambda function. The object `.lower` and `.upper` are
#'  special reserved values, corresponding to the limits given from valid values,
#'  that can be used within this function.
#' @param .aggregate The function for aggregation if the response values differ
#'  within the same unit level for the record. Use `NA` if you don't want to aggregate.
#'  By default, it will get the mean or mode depending on the encoding
#'  (numeric is mean, mode for character or factor), or if absent,
#'  based on returned encoding. It can be a named list where the names correspond to
#'  the record name and the values corresponding to a function.
#' @seealso [simulate_rcrds()]
#'
#' @export
with_params <- function(..., .censor = NA, .aggregate = NULL) {
  params <- list2(...)
  list(params = params, censor = .censor, aggregate = .aggregate)
}

#' Simulate records
#'
#' @param .data An edibble data
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

  srcrds_list <- map(prnames, function(x) prov$get_simulate(x)$rcrds)
  srcrds <- unlist(srcrds_list)
  if(any(duplicated(srcrds))) warn(paste0("You are trying to simulate ",
                                          .combine_words(srcrds[duplicated(srcrds)], fun = cli::col_blue),
                                          " in multiple processes. The values will be overwritten."))


  for(aprocess in prnames) {
    process <- prov$get_simulate(aprocess)$process
    if(is_null(process)) abort(paste0("The supplied process, ", cli::col_blue(aprocess), ", doesn't exist"))
    srcrds <- prov$get_simulate(aprocess)$rcrds
    body(process) <- patch_function(process, patch = sprintf("    list2env(setNames(list(mget(ls())), '%s'), envir = result_env)", aprocess),
                              position = length(as.list(body(process))) - 1)

    y <- eval_tidy(do.call(process, dots[[aprocess]]$params), data = .data)

    # now assign the values to the data, but apply aggregation then censorship
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
  return_edibble_with_graph(.data, prov)
}

get_rcrd_values <- function(rname, prov, aggfn, .data, y, censor) {
  vrcrds <- prov$get_validation("rcrds")
  uid <- prov$mapping_to_unit(id = prov$fct_id(name = rname))
  uname <- prov$fct_names(id = uid)
  agg_fn <- get_aggregate_function(aggfn,
                                   rname,
                                   get_record_type(vrcrds, rname),
                                   class(y))
  # TODO: this is a problem when we have nested_labels
  if(is_null(attr(.data[[uname]], "non-nested")))
    vals <- .data[[uname]]
  else vals <- attr(.data[[uname]], "non-nested")
  vals <- aggregate_values(y, vals, agg_fn)
  if(rname %in% names(vrcrds)) {
    if(is.list(censor) && rname %in% names(censor)) {
      get_censored_value(vals, vrcrds[[rname]], censor[[rname]])
    } else if(is.list(censor) && ".default" %in% names(censor)) {
      get_censored_value(vals, vrcrds[[rname]], censor[[".default"]])
    } else {
      get_censored_value(vals, vrcrds[[rname]], censor)
    }
  } else {
    vals
  }
}

get_censored_value <- function(y, valid, censor) {
  if(is_null(valid)) return(y)
  type <- valid$record
  if(type=="numeric") {
    value <- valid$value
    valid_env <- rlang::current_env()
    if(length(value) == 1) {
      valid_env$.upper <- valid_env$.lower <- value
    } else if(length(value) == 2) {
      valid_env$.lower <- value[1]
      valid_env$.upper <- value[2]
    }
    if(valid$operator!="between" & length(censor)!=1) {
      warn("There should be only one censor value. Only the first value used.")
      censor <- censor[1]
    }
    if(valid$operator=="between" & length(censor)!=2) {
      if(!is.na(censor)) warn("There should be only two censor values.")
      if(length(censor) > 2) censor <- censor[1:2]
      if(length(censor) == 1) censor <- c(censor, NA)
      if(length(censor) == 0) censor <- c(NA, NA)
    }
    switch(valid$operator,
           "greaterThan" = return_censor_value(y, y > value, censor, valid_env = valid_env),
           "greaterThanOrEqual" = return_censor_value(y, y >= value, censor, valid_env = valid_env),
           "equal" = return_censor_value(y, y == value, .censor, valid_env = valid_env),
           "between" = return_censor_value(y, y > value[1] & y < value[2], censor, value, valid_env = valid_env),
           "lessThanOrEqual" = return_censor_value(y, y <= value, censor, valid_env = valid_env),
           "lessThan" = return_censor_value(y, y < value, censor, valid_env = valid_env))
  } else if(type=="factor") {
    ind <- y %in% valid$values
    if(is_function(censor)) {
      y[!ind] <- censor(y[ind])
    } else {
      y[!ind] <- censor
    }
    y
  } else if(type=="integer") {
    valid$record <- "numeric"
    res <- get_censored_value(y, valid, censor)
    round(res)
  } else {
    y
  }
}

return_censor_value <- function(y, ind, censor, values = NULL, ind_censor = ind, valid_env = NULL) {
  if(is_null(values)) {
    if(is_formula(censor)) censor <- as_function(censor)
    if(is_function(censor)) {
      environment(censor) <- valid_env
      y[!is.na(ind) & !ind] <- censor(y[!is.na(ind_censor) & ind_censor])
    } else if(!is.infinite(censor)) {
      y[!is.na(ind) & !ind] <- censor
    }
  } else {
    if(length(censor) == 2) {
      y <- return_censor_value(y, y > values[1], censor[[1]], ind_censor = ind, valid_env = valid_env)
      y <- return_censor_value(y, y < values[2], censor[[2]], ind_censor = ind, valid_env = valid_env)
    } else {
      y <- return_censor_value(y, y > values[1], censor, ind_censor = ind, valid_env = valid_env)
      y <- return_censor_value(y, y < values[2], censor, ind_censor = ind, valid_env = valid_env)
    }
  }
  y
}


get_record_type <- function(vrcrds, arcrd) {
  ifelse(arcrd %in% names(vrcrds), vrcrds[[arcrd]]$record, NA)
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
  if(!is.na(rcrd_type) && is.null(rcrd_type) && rcrd_type == "factor") return(get_mode)
  if(any(return_type %in% c("factor", "character"))) return(get_mode)
  return(function(x) mean(x, na.rm = TRUE))
}

aggregate_values <- function(y, group, fn) {
 if(is_function(fn)) {
   agg <- tapply(y, group, fn)
   as.vector(unname(agg[as.character(group)]))
 } else if(is.na(fn) | is.null(fn)) {
   y
 }
}



#' A helper function to set variables that the record is dependent on.
#'
#' The other options give are characteristics of the record (not the independent variables).
#' Warning: none of the other options work at the moment!
#'
#' @param ... A series of factors in which the record is explicitly dependent upon (tidyselect campatible).
#' @param .missing A logical value indicating whether there should be some
#'   missing values. Default is FALSE. The missing values are introduced at random.
#'   It can also be numeric of between 0 and 1 giving the proportion of missing values.
#' @param .interaction Whether there should be treatment interaction effects.
#' @param .discrete Whether to make the response value discrete or not.
#' @param .linear Whether to include non-linear term or not. The value is always additive.
#' @param .dist The random distribution to use for numerical values
#'  (either "normal", "uniform", "exponential", "gamma", "beta", "cauchy", "chisq", "f", "t", "poisson", "weibull").
#'  The default choice is random out of these with higher chances of "normal".
#' @seealso [autofill_rcrds()]
#' @export
with_variables <- function(...,
                           .missing = FALSE,
                           .interaction = random_true_false(),
                           .discrete = FALSE,
                           .linear = random_true_false(),
                           .error_dist = NULL) {
  fcts <- enquos(...)
  list(vars = fcts,
       missing = .missing,
       interaction = .interaction,
       linear = .linear,
       discrete = .discrete,
       error_dist = .error_dist %||% sample(c("normal", "uniform", "t", "exponential", "gamma",
                                  "beta", "cauchy", "chisq", "f",
                                  "poisson", "weibull"), 1, prob = c(0.4, 0.2, 0.2, rep(0.025, 8))))
}



#' Autofill the records
#'
#' This function fills the values of the record factors by automatically
#' choosing a simulation process. It tries to be smart by ensuring to use
#' values that is within expectation.
#'
#' @param .data An edibble data.
#' @param ... If supplied, it is a name-value pair where the name should
#'   correspond to the record factor name and value is the f
#' @param .seed The seed number.
#' @export
autofill_rcrds <- function(.data, ..., .seed = NULL) {
  prov <- activate_provenance(.data)
  prov$save_seed(.seed, type = "autofill_rcrds")
  dots <- list2(...)
  tnames <- prov$trt_names()
  rnames <- prov$rcrd_names()
  unames <- prov$unit_names()
  r_to_u <- map_chr(rnames, function(name) {
    uids <- prov$mapping_to_unit(id = prov$fct_id(name = name))
    prov$fct_names(id = uids)
  })
  vrcrds <- prov$get_validation(type = "rcrds")
  deps <- map(seq_along(r_to_u), function(i) {
    uname <- r_to_u[i]
    rname <- rnames[i]
    default_opts <- with_variables()
    list(type = ifelse(rname %in% names(vrcrds), vrcrds[[rname]]$record, NA),
         rcrd_levels = if(rname %in% names(vrcrds)) vrcrds[[rname]]$values,
         unit = uname,
         trts = sample(tnames, size = sample(0:length(tnames), 1)),
         rcrds = character(),
         missing = default_opts$missing,
         interaction = default_opts$interaction,
         linear = default_opts$linear,
         discrete = default_opts$discrete,
         error_dist = default_opts$error_dist)
  })

  names(deps) <- rnames
  if(length(dots)) {
    for(rname in names(dots)) {
      depends <- dots[[rname]]
      fnames <- names(tidyselect::eval_select(expr(c(!!!depends$vars)), .data))
      deps[[rname]] <- list(type = deps[[rname]]$type,
                            rcrd_levels = deps[[rname]]$rcrd_levels,
                            unit = intersect(fnames, unames),
                            trts = intersect(fnames, tnames),
                            rcrds = setdiff(fnames, c(unames, tnames)),
                            missing = depends$missing,
                            interaction = depends$interaction,
                            linear = depends$linear,
                            discrete = depends$discrete,
                            error_dist = depends$error_dist)
    }
  }

  process_functions = character()
  processes = character()
  for(rname in names(deps)) {
    dep_fcts <- c(deps[[rname]]$unit, deps[[rname]]$trts, deps[[rname]]$rcrds)
    if(is.na(deps[[rname]]$type)) {
      code_list <- effects_code(dep_fcts, .data)
      code_adjust_y <- "y"
    } else if(deps[[rname]]$type %in% c("numeric", "integer")) {
      if(vrcrds[[rname]]$operator == "equal" | (vrcrds[[rname]]$operator == "between" && vrcrds[[rname]]$value[1] == vrcrds[[rname]]$value[2])) {
        code_list <- list(process_code = "", model_code = "")
        code_adjust_y <- sprintf("rep(%f, n())", deps[[rname]]$value)
      } else {
        valid_lower <- switch(vrcrds[[rname]]$operator,
                              "greaterThan" = vrcrds[[rname]]$value,
                              "greaterThanOrEqual" = vrcrds[[rname]]$value,
                              "equal" = vrcrds[[rname]]$value,
                              "between" = vrcrds[[rname]]$value[1],
                              "lessThanOrEqual" = NA,
                              "lessThan" = NA)
        valid_upper <- switch(vrcrds[[rname]]$operator,
                              "greaterThan" = NA,
                              "greaterThanOrEqual" = NA,
                              "equal" = vrcrds[[rname]]$value,
                              "between" = vrcrds[[rname]]$value[2],
                              "lessThanOrEqual" = vrcrds[[rname]]$value,
                              "lessThan" = vrcrds[[rname]]$value)
        code_list <- effects_code(dep_fcts, .data)
        code_adjust_y <- sprintf("edibble::rescale_values(y, lower = %f, upper = %f)", valid_lower, valid_upper)
      }
    } else if(deps[[rname]]$type=="factor") {
      nlvls <- length(deps[[rname]]$rcrd_levels)
      if(nlvls==1) {
        code_list <- list(process_code = "", model_code = "")
        code_adjust_y <- sprintf("rep(%s, n())", deps[[rname]]$rcrd_levels[1])
      } else {
        code_list <- effects_code(dep_fcts, .data, nlvls)
        code_adjust_y <- switch(as.character(nlvls),
                                "2" = sprintf("ifelse(1 / (1 + exp(-y)) > 0.5, '%s', '%s')", deps[[rname]]$rcrd_levels[1], deps[[rname]]$rcrd_levels[2]),
                                {
                                  c(sprintf("res <- as.data.frame(lapply(1:%d, function(i) 1 / (1 + exp(-y[[i]]))))", nlvls),
                                    sprintf("        c(%s)[apply(res, 1, which.max)]", paste0(paste0("'", deps[[rname]]$rcrd_levels, "'"), collapse = ", ")))
                                })
      }
    } else {
      # if response is other than numeric or factor, don't simulate for now
      # and go to the next record factor
      next
    }

    process_functions <- c(process_functions, sprintf('
      %s = function() {
        set.seed(%d)\n%s
        %s
        %s
      }', rname,
        random_seed_number(),
        paste0(code_list$process_code, collapse = "\n"),
        paste0(unlist(code_list$model_code), collapse = "\n        "),
        paste0(code_adjust_y, collapse = "\n")))

    processes <- c(processes, sprintf('%s = with_params()', rname))
  }


  simulate_processes <- sprintf("simulate_process(%s)", paste(process_functions, collapse = ",\n"))
  simulate_rcrds <- paste0('simulate_rcrds(', paste0(processes, collapse = ',\n                  '), ')')
  final_code <- parse(text = paste(".data %>%\n  ", simulate_processes, "%>%\n  ", simulate_rcrds))
  eval(final_code)
}



effects_code <- function(dep_fcts, .data, nlevels = 1) {
  code_list <- list(process_code = character(), model_code = character())
  if(nlevels <= 2) {
    for(fct in dep_fcts) {
      fct_class <- class(.data[[fct]])
      nfct <- length(unique(.data[[fct]]))
      if("numeric" %in% fct_class) {
        code_list$process_code <- c(code_list$process_code,
                                    sprintf('%s_degree <- sample(1:%d, 1)', fct, ifelse(nfct > 5, 5, nfct - 1)),
                                    paste(sprintf('%s_effects <- as.vector(poly(%s, %s_degree)', fct, fct, fct),
                                          "%*%",
                                          sprintf('rnorm(%s_degree, 0, 10))', fct)))
        code_list$model_code <- c(code_list$model_code, sprintf("%s_effects", fct))

        # logical not accounted for
      } else if(any(c("factor", "character") %in% fct_class)) {
        code_list$process_code <- c(code_list$process_code,
                                        sprintf('        %s_effects <- setNames(rnorm(%d, 0, 10), unique(%s))',
                                                fct, nfct, fct))
        code_list$model_code <- c(code_list$model_code, sprintf("%s_effects[%s]", fct, fct))
      }
    }
    # combine
    code_list$model_code <- paste0("y <- ", paste0(code_list$model_code, collapse = " + "))
  } else {
    for(fct in dep_fcts) {
      fct_class <- class(.data[[fct]])
      nfct <- length(unique(.data[[fct]]))
      if("numeric" %in% fct_class) {
        code_list$process_code <- c(code_list$process_code,
                                    sprintf('        %s_degree <- lapply(1:%d, function(i) sample(1:%d, 1))', fct, nlevels, nfct),
                                    sprintf('        %s_effects <- lapply(1:%d, function(i) as.vector(poly(%s, %s_degree[[i]]) %*% rnorm(%s_degree[[i]], 0, 10)))', fct, nlevels, fct, fct, fct))

        code_list$model_code <- c(code_list$model_code, sprintf("%s_effects[[i]]", fct))
      } else if(any(c("factor", "character") %in% fct_class)) {
        code_list$process_code <- c(code_list$process_code,
                                    sprintf('        %s_effects <- lapply(1:%d, function(i) setNames(rnorm(%d, 0, 10), unique(%s)))',
                                            fct, nlevels, nfct, fct))
        code_list$model_code <- c(code_list$model_code, sprintf("%s_effects[[i]][%s]", fct, fct))
      }
    }
    # combine
    code_list$model_code <- paste0(sprintf("y <- lapply(1:%d, function(i) ", nlevels), paste0(code_list$model_code, collapse = " + "), ")")
  }
  code_list
}


#' Examine the simulation process
#'
#' @param data An edibble data frame.
#' @param process The process name. Typically the name of the process. If unknown,
#'  leave this empty.
#' @export
examine_process <- function(data, process = NULL) {
  prov <- activate_provenance(data)
  res <- prov$get_simulate(process)
  if(is_null(res)) {
    warning("There is no simulation process stored.")
    NULL
  } else {
    if(!is_null(process)) {
      res <- setNames(list(res), process)
    }
    structure(res, class = c("sim_process", class(res)))
  }
}

#' @rdname examine_process
#' @export
examine_process_values <- function(data, process = NULL) {
  prov <- activate_provenance(data)
  res <- prov$get_simulate_result_env(process)
  if(is_null(res)) {
    warning("There is no simulation process stored.")
    NULL
  } else if(is.environment(res)) {
    abort(paste0("You need to specify a process name. The available process names are: ",
                .combine_words(ls(envir = res), fun = cli::col_blue), "."))
  } else {
    res
  }
}

#' @export
print.sim_process <- function(x, ...) {
  cat("simulate_process(\n")
  sep_fns <- ",\n\n"
  for(aname in names(x)) {
    if(aname == names(x)[length(x)]) sep_fns <- ""
    fn_text <- deparse(x[[aname]]$process)
    fn_text <- c(fn_text[1], paste0("   ", fn_text[2:length(fn_text)]))
    cat(paste("  ", cli::col_blue(aname), "=", paste0(fn_text, collapse = "\n"), sep_fns))
  }
  cat(")\n")
}


# thanks to https://stackoverflow.com/questions/38732663/how-to-insert-expression-into-the-body-of-a-function-in-r
patch_function <- function(fun, patch, position = 1) {
  # Deparse the body.
  # TODO: width.cutoff = 500 is a big upper limit
  # but this could be an issue if ever line is >500
  fun_body <- deparse(body(fun), width.cutoff = 500)
  if(length(fun_body) <= 1) {
    # TODO: what about when length == 0?
    patched_fun_body <- paste0(
      fun_body,
      collapse = "\n"
    )
  } else {
    # Append the patch to function body where intended.
    patched_fun_body <- paste0(
      c(fun_body[1:position], patch, fun_body[(position + 1):length(fun_body)]),
      collapse = "\n"
    )
  }
  # Parse and treat as an expression.
  as.expression(parse(text = patched_fun_body))
}
