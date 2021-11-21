
#' Set records for given unit
#'
#' @description
#' This function creates new nodes to edibble graph with the name
#' corresponding to either the intended response that will be measured or
#' a variable to be recorded.
#'
#' @inheritParams design-context
#' @param ... Name-value pair. The value should correspond to a single name of the
#'  unit defined in `set_units`. The name should be the name of the record variable.
#' @family user-facing functions
#' @export
set_rcrds <- function(.edibble, ...,
                      .name_repair = c("check_unique", "unique", "universal", "minimal")) {

  not_edibble(.edibble)
  .name_repair <- match.arg(.name_repair)
  units <- map_chr(enexprs(...), function(x) {
      if(is.character(x)) return(x)
      return(quo_text(x))
    })
  rcrds <- names(units)

  check_var_exists(.edibble, label = units, vclass = "edbl_unit")

  for(i in seq_along(units)) {
    rid <- fct_last_id(.edibble) + 1L
    uid <- fct_id(.edibble, unname(units)[i])
    .edibble$graph$nodes <- add_row(.edibble$graph$nodes,
                                     id = rid,
                                     label = rcrds[i],
                                     class = "edbl_rcrd")
    .edibble$graph$edges <- add_row(.edibble$graph$edges,
                                     from = uid,
                                     to = rid)
  }
  .edibble
}

#' @export
set_rcrds_of <- function(.edibble, ...) {
  unit2rcrd <- list2(...)
  units <- names(unit2rcrd)
  args <- list()
  for(aunit in units) {
    rcrds <- unit2rcrd[[aunit]]
    args <- c(args, structure(as.list(rep(aunit, length(rcrds))),
                              names = rcrds))
  }

  set_rcrds(.edibble, !!!args)
}

#' Set the expected values for recording variables
#'
#' @inheritParams design-context
#' @param ... Name-value pairs with the name belonging to the variable
#'  that are plan to be recorded from `set_rcrds()` and the values are
#'  the expected types and values set by helper functions, see `?expect-rcrds`.
#' @family user-facing functions
#' @export
expect_rcrds <- function(.edibble, ...) {
  not_edibble(.edibble)
  dots <- enquos(...)
  dots_nms <- names(dots)
  rules_named <- map(dots[dots_nms!=""], eval_tidy)
  rules_unnamed <- map(dots[dots_nms==""], validate_rcrd,
                       rnames = fct_label(.edibble, rcrd_ids(.edibble)))
  rules_unnamed <- setNames(rules_unnamed, map_chr(rules_unnamed, function(x) x$rcrd))
  .edibble$validation <- simplify_validation(c(rules_named, rules_unnamed))
  .edibble
}

simplify_validation <- function(x) {
  rcrd_nms <- names(x)
  rules <- list()
  for(arcrd in unique(rcrd_nms)) {
    xs <- x[which(rcrd_nms==arcrd)]
    rules[arcrd] <- xs[1]
    if(length(xs)!=1) {
      # the rules must match record type
      record_type <- unique(map_chr(xs, function(a) a$record))
      if(length(record_type)==1 && record_type=="numeric") {
        rng <- c(-Inf, Inf)
        inc <- c(TRUE, TRUE)
        for(ax in xs) {
          rng[1] <- switch(ax$operator,
                           "equal" = ,
                           "greaterThanOrEqual" = ,
                           "greaterThan" = max(ax$value, rng[1]),
                           "between" = max(ax$value[1], rng[1]),
                           rng[1])
          rng[2] <- switch(ax$operator,
                           "equal" = ,
                           "lessThanOrEqual" = ,
                           "lessThan" = min(ax$value, rng[2]),
                           "between" = min(ax$value[2], rng[2]),
                           rng[2])
          inc[1] <- switch(ax$operator,
                           "greaterThan" = FALSE,
                           TRUE)
          inc[2] <- switch(ax$operator,
                           "lessThan" = FALSE,
                           TRUE)
        }
        if(all(is.infinite(rng))) {
          # TODO: support just including type as numeric
          abort("No upper or lower range supplied. This is not supported yet.")
        } else if(is.infinite(rng[1])) {
          rules[[arcrd]]$operator <- ifelse(inc[1], "greaterThanOrEqual", "greaterThan")
          rules[[arcrd]]$value <- rng[2]
        } else if(is.infinite(rng[2])) {
          rules[[arcrd]]$operator <- ifelse(inc[1], "lessThanOrEqual", "lessThan")
          rules[[arcrd]]$value <- rng[1]
        } else {
          rules[[arcrd]]$operator <- "between"
          rules[[arcrd]]$value <- rng
        }

      } else {
        abort(sprintf("The record `%s` have record types: %s. Only one record type is allowed.",
                      arcrd, .combine_words(record_type)))
      }
    }
  }
  rules
}




validate_rcrd <- function(x, rnames = NULL) {
  l <- as.list(x[[2]])
  operator <- as.character(l[[1]])
  #browser()
  if(is.numeric(l[[2]])) {
    value <- l[[2]]
  } else {
    rcrd1 <- as.character(l[[2]])
  }
  if(is.numeric(l[[3]])) {
    value <- l[[3]]
  } else {
    rcrd2 <- as.character(l[[3]])
  }
  if(exists("rcrd1") && rcrd1 %in% rnames) {
    rcrd <- rcrd1
  } else if(exists("rcrd2") && rcrd2 %in% rnames){
    rcrd <- rcrd2
  }
  if(exists("value") & operator!="factor") {
    if(is.integer(value)) {
      return(c(to_be_integer(with_value(operator, value)), rcrd = rcrd))
    } else {
      return(c(to_be_numeric(with_value(operator, value)), rcrd = rcrd))
    }
  } else if(operator=="factor") {
    lvls <- eval(l[[3]], envir = attr(x, ".Environment"))
    return(c(to_be_factor(levels = lvls), rcrd = rcrd))
  }
}



#' @export
expect_vars <- function(.edibble, ...) {
  warn("This function defunct in favour of `expect_rcrds`. Please replace `expect_vars` with `expect_rcrds`")
  expect_rcrds(.edibble, ...)
}

has_record <- function(.design) {
  "edbl_rcrd" %in% .design$graph$nodes$class
}


#' Expected type of data entry
#'
#' @description
#' These functions should be used within `expect_vars` where variables that
#' are to be recorded are constraint to the expected values when exported
#' as an xlsx file by `export_design().` The functions to set a particular
#' value type (numeric, integer, date, time and character) are preceded by
#' "to_be_" where the corresponding restriction set by `with_value()`.
#'
#' @param value A vector of possible values for entry.
#' @param range,length A named list with two elements: "operator" and "value" as
#'  provided by helper `with_value()` that gives the possible range of values
#'  that the expected type can take.
#' @name expect-vars
#' @export
to_be_numeric <- function(range) {
  c(list(type = "decimal", record = "numeric"), range)
}

#' @rdname expect-vars
#' @export
to_be_integer <- function(range) {
  c(list(type = "whole", record = "integer"), range)
}

#' @rdname expect-vars
#' @export
to_be_date <- function(range) {
  c(list(type = "date", record = "date"), range)
}

#' @rdname expect-vars
#' @export
to_be_time <- function(range) {
  c(list(type = "time", record = "time"), range)
}

#' @rdname expect-vars
#' @export
to_be_character <- function(length) {
  c(list(type = "textLength", record = "text"), length)
}

#' @rdname expect-vars
#' @export
to_be_factor <- function(levels) {
  list(type = "list", record = "factor", values = levels)
}



#' Validation values
#'
#' This creates a list that is used later for creating data validation rules
#' when the data is exported.
#'
#' @param operator Operator to apply.
#' @param value An optional value related to operator
#' @param between,not_between An optional numerical vector of size two where the
#'  first entry is the minimum value and the second entry is the maximum value.
#'  For `between`, the value is valid if within the range of minimum and maximum
#'  value inclusive. For `not_between`, the value must lie outside of these values.
#' @return A list with two elements `operator` and `value`.
#' @export
with_value <- function(operator = c("=", "==", ">=", "<=", "<", ">", "!="),
                     value = NULL, between = NULL, not_between = NULL) {
  operator <- match.arg(operator)
  if(!is_null(between) & !is_null(not_between)) {
    abort("You cannot define `between` and `not_between` simultaneously.")
  }
  if(!is_null(between)) {
    return(list(operator = "between", value = between))
  }
  if(!is_null(not_between)) {
    return(list(operator = "notBetween", value = not_between))
  }
  list(operator = switch(operator,
                     "=" = "equal",
                     "==" = "equal",
                     ">=" = "greaterThanOrEqual",
                     ">" = "greaterThan",
                     "<=" = "lessThanOrEqual",
                     "<" = "lessThan",
                     "!=" = "notEqual"),
       value = value)
}


new_edibble_rcrd <- function(n, unit, class = NULL) {
  v <- rep("x", n)
  loc <- match(unique(unit), unit)
  v[loc] <- "■"
  x <- new_vctr(v, class = "edbl_rcrd")
  class(x) <- c(class, class(x))
  x
}

#' @importFrom pillar pillar_shaft new_pillar_shaft_simple style_subtle
#' @export
pillar_shaft.edbl_rcrd <- function(x, ...) {
  out <- as.character(x)
  out <- ifelse(out=="x", style_subtle("x"), out)
  new_pillar_shaft_simple(out, align = "right")
}

#' @export
as.character.edbl_rcrd <- function(x, ...) {
  out <- unclass(x)
  attributes(out) <- NULL
  out
}


#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.edbl_rcrd <- function(x, ...)  {
  "rcrd"
}

#' @importFrom vctrs vec_ptype_full
#' @export
vec_ptype_full.edbl_rcrd <- function(x, ...) "rcrd"

#' # TODO
#' # see scabbiness.R
#' #' Derive variables from other variables.
#' #'
#' #' @description
#' #' This is used to specify the excel formula for variables that
#' #' are derived based on other variables.
#' #'
#' #' @seealso See [calculate()], [calculate2()] and [pcalculate()] to
#' #' specify the excel formula.
#' derive_vars <- function(.edibble, ...) {
#'   dots <- enquos(...)
#'   dots_names <- names(dots)
#' }
#'
#' #' Specify the calculation to derive variables
#' #'
#' #' @description
#' #' This function specifies the excel formula that should
#' #' be stored when the design is exported with [export_design()].
#' #' The functions must be translatable to excel formula. Mappings
#' #' for some complex functions may not work.
#' #'
#' #' @param .x,.y Name of other variables in data.
#' #' @param .f A function, formula, or excel formula.
#' #' @param ... Arguments to be passed into `.f`.
#' #' @param .l A vector of variable names.
#' #' @return A special derivation class.
#' calculate <- function(.x, .f, ..., .group_by = NULL) {
#'   if(inherits(.f, "xlformula")) {
#'     xlf <- .f
#'   } else if(is.function(.f)) {
#'     xlf <- map_to_xlf(.f, ...) # TODO
#'   } else {
#'     xlf <- map_to_xlf(rlang::as_function(.f))
#'   }
#'   return(structure(list(vars = as_label(enexpr(.x)),
#'                         xlf = xlf,
#'                         group_by = .group_by),
#'                    class = "derivative"))
#' }
#'
#' calculate2 <- function(.x, .y, .f, ..., .group_by = NULL) {
#'   if(inherits(.f, "xlformula")) {
#'     xlf <- .f
#'   } else if(is.function(.f)) {
#'     xlf <- map_to_xlf(.f, ...) # TODO
#'   } else {
#'     xlf <- map_to_xlf(rlang::as_function(.f))
#'   }
#'   return(structure(list(vars = c(as_label(enexpr(.x)),
#'                                  as_label(enexpr(.y))),
#'                         xlf = xlf,
#'                         group_by = .group_by),
#'                    class = "derivative"))
#' }
#'
#' pcalculate <- function(.l, .f, ..., .group_by = NULL) {
#'   if(inherits(.f, "xlformula")) {
#'     xlf <- .f
#'   } else if(is.function(.f)) {
#'     xlf <- map_to_xlf(.f, ...) # TODO
#'   } else {
#'     xlf <- map_to_xlf(rlang::as_function(.f))
#'   }
#'   return(structure(list(vars = all.vars(enexprs(l)),
#'                         xlf = xlf,
#'                         group_by = .group_by),
#'                    class = "derivative"))
#' }
#'
#' xlf <- function(.f = NULL) {
#'   structure(.f, class = "xlformula")
#' }
