
#' Set records for given unit
#'
#' @description
#' This function creates new nodes to edibble graph with the name
#' corresponding to either the intended response that will be measured or
#' a variable to be recorded.
#'
#' @inheritParams set_units
#' @param ... Name-value pair. The value should correspond to a single name of the
#'  unit defined in `set_units`. The name should be the name of the record variable.
#' @family user-facing functions
#' @examples
#' takeout(menu_crd(t = 4, n = 10)) %>%
#'   set_rcrds(y = unit)
#'
#' takeout(menu_crd(t = 4, n = 10)) %>%
#'   set_rcrds_of(unit = "y")
#' @return An edibble design.
#' @export
set_rcrds <- function(.edibble, ...,
                      .name_repair = c("check_unique", "unique", "universal", "minimal"),
                      .record = TRUE) {

  not_edibble(.edibble)
  des <- edbl_design(.edibble)
  prov <- activate_provenance(des)
  if(.record) prov$record_step()
  .name_repair <- match.arg(.name_repair)

  units <- map(enexprs(...), function(x) {
      if(is.character(x)) return(x)
      if(is_symbol(x)) return(quo_text(x))
      return(eval(x))
    })

  rcrds <- names(units)

  prov$fct_exists(name = unlist(units), role = "edbl_unit")

  for(i in seq_along(units)) {
    prov$append_fct_nodes(name = rcrds[i], role = "edbl_rcrd")
    uid <- prov$fct_id(name = units[[i]])
    rid <- prov$fct_id(name = rcrds[i])
    prov$append_fct_edges(from = rid, to = uid, type = "record")
  }

  if(is_edibble_table(.edibble)) {
    rcrds <- prov$serve_rcrds(return = "value")
    for(arcrd in names(rcrds)) {
      if(arcrd %in% names(.edibble)) {
        uid <- prov$mapping_to_unit(id = prov$fct_id(name = arcrd))
        uname <- prov$fct_names(id = uid)
        uids <- prov$fct_id(name = .edibble[[uname]])
        .edibble[[arcrd]] <- new_edibble_rcrd(rep(NA_real_, nrow(.edibble)), uids)
      } else {
        .edibble[[arcrd]] <- rcrds[[arcrd]]
      }
    }
  }

  return_edibble_with_graph(.edibble, prov)
}

#' @rdname set_rcrds
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
#' @inheritParams set_units
#' @param ... Name-value pairs with the name belonging to the variable
#'  that are plan to be recorded from `set_rcrds()` and the values are
#'  the expected types and values set by helper functions, see `?expect-rcrds`.
#' @family user-facing functions
#' @examples
#' takeout(menu_crd(t = 4, n = 10)) %>%
#'   set_rcrds(y = unit) %>%
#'   expect_rcrds(y > 0)
#' @return An edibble design.
#' @export
expect_rcrds <- function(.edibble, ..., .record = TRUE) {
  not_edibble(.edibble)
  prov <- activate_provenance(.edibble)
  if(.record) prov$record_step()
  dots <- enquos(...)
  dots_nms <- names(dots)
  rules_named <- map(dots[dots_nms!=""], eval_tidy)
  rules_unnamed <- map(dots[dots_nms==""], validate_rcrd,
                       rnames = prov$rcrd_names())

  rules_unnamed <- stats::setNames(rules_unnamed, map_chr(rules_unnamed, function(x) x$rcrd))
  prov$set_validation(simplify_validation(c(rules_named, rules_unnamed)), type = "rcrds")
  return_edibble_with_graph(.edibble, prov)
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
      if(length(record_type)==1 && record_type %in% c("numeric", "integer")) {
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

validate_values <- function(x, env = parent.env()) {
  if(is.numeric(x)) {
    value <- x
  } else if(is.language(x)) {
    value <- tryCatch(eval(x, envir = env),
                      error = function(y) as.character(x))
  } else {
    abort("Don't know how to interpret the validation value.")
  }
  value
}

reverse_operator <- function(x) {
  switch(x,
         "<" = ">",
         "<=" = ">=",
         ">=" = "<=",
         ">" = "<")
}


validate_rcrd <- function(x, rnames = NULL) {
  l <- as.list(rlang::quo_get_expr(x))
  operator <- as.character(l[[1]])
  val1 <- validate_values(l[[2]], env = attr(x, ".Environment"))
  val2 <- validate_values(l[[3]], env = attr(x, ".Environment"))
  w <- ifelse(val1 %in% rnames,
              1,
              ifelse(val2 %in% rnames,
                     2,
                     abort("No record factor found.")))
  rcrd <- ifelse(w==1, val1, val2)
  value <- if(w==2) val1 else val2
  if(operator!="factor") {
    operator <- ifelse(w==1, operator, reverse_operator(operator))
    if(is.integer(value)) {
      return(c(to_be_integer(with_value(operator, value)), rcrd = rcrd))
    } else {
      return(c(to_be_numeric(with_value(operator, value)), rcrd = rcrd))
    }
  } else if(operator=="factor") {
    return(c(to_be_factor(levels = value), rcrd = rcrd))
  }
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
#' @param range,length A named list with two elements: "operator" and "value" as
#'  provided by helper `with_value()` that gives the possible range of values
#'  that the expected type can take.
#' @param levels A character vector with the factor levels.
#' @name expect-vars
#' @return A record type.
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

fill_symbol <- function() "o"
dup_symbol <- function() "x"


new_edibble_rcrd <- function(x, unit_values = NULL, class = NULL, ...) {
  res <- new_vctr(x, class = c("edbl_rcrd", "edbl_fct"),
                  #unit = unit_name %||% attr(x, "unit_name"),
                  unit_values = unit_values %||% attr(x, "unit_values"),
                  ...)
  class(res) <- c(class, class(res))
  res
}

#' @importFrom pillar pillar_shaft new_pillar_shaft_simple style_subtle
#' @export
pillar_shaft.edbl_rcrd <- function(x, ...) {
  if(all(is.na(x))) {
    uvals <- attr(x, "unit_values")[1:length(x)]
    n <- length(uvals)
    out <- rep(dup_symbol(), n)
    loc <- match(unique(uvals), uvals)
    out[loc] <- fill_symbol()
    new_pillar_shaft_simple(out, align = "right")
  } else {
    pillar::pillar_shaft(unclass(x))
  }
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

#' @export
as.numeric.edbl_rcrd <- function(x, ...) {
  out <- unclass(x)
  out
}
