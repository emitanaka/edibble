
return_edibble_with_graph <- function(edibble, prov) {
  des <- edbl_design(edibble)
  des$graph <- prov$get_graph()
  des$validation <- prov$get_validation()
  des$simulate <- prov$get_simulate()
  sim_res <- ls(envir = prov$get_simulate_result_env(), all.names = TRUE)
  if(length(sim_res)) {
    des$simulate_result <- mget(sim_res, prov$get_simulate_result_env())
  }
  if(is_edibble_table(edibble)) {
    attr(edibble, "design") <- des
    edibble
  } else {
    des
  }
}


eval_formula <- function(f, env) {
  lhs <- f_lhs(f)
  if(!is_formula(f) || is_null(lhs)) {
    abort("Input must be two-sided formula.")
  }
  env <- f_env(f) %||% env
  if(!is_symbol(lhs, name = ".")) {
    lhs <- eval_tidy(lhs, env = env)
  }
  list(lhs = lhs,
       rhs = eval_tidy(f_rhs(f), env = env))
}



decorate_vars <- function(x, decorate_units, decorate_trts, decorate_rcrds, classes) {
  edbl_classes <- c("edbl_unit", "edbl_trt", "edbl_rcrd")
  decorate_fns <- list(decorate_units, decorate_trts, decorate_rcrds)
  for(i in seq_along(edbl_classes)) {
    index <- which(classes==edbl_classes[i])
    if(length(index) > 0) {
      x[index] <- decorate_fns[[i]](x[index])
    }
  }
  x
}



#' Print intermediate experimental design to terminal
#'
#' @description
#' This function prints an `edbl_graph` object as a tree to terminal.
#' The variables are color coded (or decorated) with the given options.
#' Any ANSI coloring or styling are only visible in the console or terminal
#' outputs that support it. The print output is best used interactively since
#' any text styling are lost in text or R Markdown output. More details can
#' be found in `vignette("edbl-output", package = "edibble")`.
#'
#' @param x An edibble graph.
#' @param decorate_trts,decorate_units,decorate_rcrds,decorate_levels,decorate_title
#' A function applied to the name of treatment, unit, response factors or
#' design title. The function should return a string. Most often this wraps the name with
#' ANSI colored text.
#' @param title The title of the design.
#' @param ... Unused.
#'
#' @importFrom cli tree cli_li
#' @name formatting
#' @export
print.edbl_design <- function(x,
                              decorate_units  = edibble_decorate("units"),
                              decorate_trts   = edibble_decorate("trts"),
                              decorate_rcrds  = edibble_decorate("rcrds"),
                              decorate_levels = edibble_decorate("levels"),
                              decorate_title  = edibble_decorate("title"),
                              title = NULL, ...) {

  prov <- activate_provenance(x)
  title <- title %||% prov$get_title() %||% "An edibble design"
  fids <- prov$fct_nodes$id
  fnames <- prov$fct_names(id = fids)
  valids <- prov$get_validation(type = "rcrds")
  if(is_empty(fids)) {
    data <- data.frame(var = "root",
                       child = NA,
                       label = as.character(decorate_title(title)))
  } else {

    classes <- prov$fct_role()
    label_names <- decorate_vars(fnames,
                                 decorate_units,
                                 decorate_trts,
                                 decorate_rcrds,
                                 classes)
    var_nlevels <- lengths(prov$fct_levels(name = fnames, return = "id"))
    nvar <- length(fids)
    ll <- lapply(fids,
                 function(id) {
                   class <- prov$fct_role(id = id)
                   children <- prov$fct_id_child(id = id, role = "edbl_unit")
                   parents <- prov$fct_id_parent(id = id, role = c("edbl_trt", "edbl_rcrd"))
                   if(class=="edbl_unit") {
                     if(!is_empty(children)) return(children)
                     if(!is_empty(parents)) return(parents)
                   }
                   return(character())
                 })
    names(ll) <- as.character(fids)
    nodes_with_parents <- as.integer(unname(unlist(ll)))
    label_names_with_levels <- paste(label_names, map_chr(var_nlevels, decorate_levels))
    #browser()
    rcrd_names <- prov$rcrd_names()
    for(arcrd in rcrd_names) {
      ipos <- which(fnames == arcrd)
      label_names_with_levels[ipos] <- label_names[ipos]
      if(!is_null(valids[[arcrd]])) {
        label_names_with_levels[ipos] <- paste(label_names_with_levels[ipos],
                                               cli::col_grey(validation_interval(valids[[arcrd]])))
      }
    }
    data <- data.frame(var = c("root", fids),
                       child = I(c(list(setdiff(fids, nodes_with_parents)), ll)),
                       label = c(decorate_title(title),
                                 label_names_with_levels))
  }
  cat(tree(data, root = "root"), sep = "\n")

 #  validation_interval(valids[[i]])
}

validation_interval <- function(x) {
  rng <- c(-Inf, Inf)
  inc <- c("[", "]")
  if(x$record %in% c("numeric", "integer")) {
    rng[1] <- switch(x$operator,
                     "equal" = ,
                     "greaterThanOrEqual" = ,
                     "greaterThan" = x$value,
                     "between" = x$value[1],
                     rng[1])
    rng[2] <- switch(x$operator,
                     "equal" = ,
                     "lessThanOrEqual" = ,
                     "lessThan" = x$value,
                     "between" = x$value[2],
                     rng[2])
    inc[1] <- switch(x$operator,
                     "greaterThan" = "(",
                     "[")
    inc[2] <- switch(x$operator,
                     "lessThan" = ")",
                     "]")

    return(paste0(inc[1], rng[1], ", ", rng[2] ,inc[2]))
  } else if(x$record == "factor") {
    lvls <- x$values
    if(length(lvls) > 5) {
      lvls <- c(lvls[1:3], "...", lvls[length(lvls)])
    }
    return(paste0("[", paste0(lvls, collapse = ", "), "]"))
  }
}

perm <- function(x) {
  n <- length(x)
  if (n == 1) x
  else {
    X <- NULL
    for (i in 1:n) X <- rbind(X, cbind(x[i], perm(x[-i])))
    X
  }
}

#' @export
print.edbl_graph <- function(x, show = c("factors", "levels", "both"), ...) {
  show <- match.arg(show)
  if(show %in% c("factors", "both")) {
    cat(cli::col_green("factor nodes\n"))
    print(x$factors$nodes)
    cat(cli::col_green("factor edges\n"))
    print(x$factors$edges)
  }
  if(show %in% c("levels", "both")) {
    cat(cli::col_blue("level nodes\n"))
    print(x$levels$nodes)
    cat(cli::col_blue("level edges\n"))
    print(x$levels$edges)
  }
}

names.edbl_graph <- function(x) {
  x$factors$nodes$name
}

names.edbl_design <- function(x) {
  names(x$graph)
}


remove_nulls <- function(.x) {
  .x[!vapply(.x, is.null, logical(1))]
}

remove_empty_df <- function(.x) {
  .x[vapply(.x, function(x) nrow(x) > 0, logical(1))]
}



compact <- function(.x) {
  .x[!vapply(.x, is_empty, logical(1))]
}


# Find how many digits
ndigits <- function(x) {
  max(c(floor(log10(abs(x))) + 1, edibble_labels_opt("min_ndigits")))
}


rbind_ <- function(df1, df2) {
  if(nrow(df1) & nrow(df2)) {
    df1[setdiff(names(df2), names(df1))] <- NA
    df2[setdiff(names(df1), names(df2))] <- NA
    out <- rbind(df1, df2[names(df1)])
  } else if(nrow(df1)) {
    df1[setdiff(names(df2), names(df1))] <- NA
    out <- df1
  } else if(nrow(df2)) {
    df2[setdiff(names(df1), names(df2))] <- NA
    out <- df2
  } else {
    out <- cbind(df1, df2[setdiff(names(df2), names(df1))])
  }
  out[c(names(df1), setdiff(names(out), names(df1)))]
}


# number SI prefix --------------------------------------------------------

#' Numbers with SI prefix
#'
#' It's called SI prefix but the letter notation is added as a suffix.
#' The largest prefix is yotta $10^{24}$.
#'
#' @param x A numeric vector to format with SI prefix
#' @source https://en.wikipedia.org/wiki/Metric_prefix
#' @noRd
number_si_prefix <- function(x) {
  if(!all(x >= 1)) abort("The numeric vector should be a positive integer.")

  prefix <- c(1, 10^c(k = 3, M = 6, G = 9, T = 12, P = 15, E = 18, Z = 21, Y = 24))
  map_chr(x, function(n) {
    index <- max(which(n/prefix >= 1))
    scale <- 1 / prefix[index]
    symbol <- names(prefix)[index]
    nn <- round(n * scale)
    ifelse(nn == n * scale, paste0(nn, symbol), paste0("~", nn, symbol))
  })
}


# inspired by knitr::combine_words
.combine_words <- function(words, sep = ", ", and = " and ",
                           before = "", after = before, fun = function(x) x) {

  words <- fun(paste0(before, words, after))
  n <- length(words)
  if(n <= 1) {
    return(words)
  }

  words[n - 1] = paste0(words[n - 1], and, words[n])
  paste0(words[-n], collapse = sep)
}

#' Convert edibble table to normal data frame
#'
#' @param x An edibble table
#' @param levels_as Coerce the edibble factors to either "factor" or "character".
#' @param ignore_numeric Whether to coerce numeric factors or not. Default is TRUE,
#' @param ... Unused.
#'  i.e. don't coerce numeric factors.
#' @export
as.data.frame.edbl_table <- function(x,
                                     ...,
                                     levels_as = "factor",
                                     ignore_numeric = TRUE) {
  out <- lapply(x, function(.x) {
    if(is_unit(.x) | is_trt(.x)) {
      if(ignore_numeric) {
        if(is.numeric(.x)) return(as.numeric(.x))
      }
      switch(levels_as,
             "factor" = factor(as.character(.x), levels(.x)),
             "character" = as.character(.x))
    } else {
      .x
    }
  })
  as.data.frame(out)
}

append_recipe_code <- function(.design, new) {
  .design$recipe <- c(.design$recipe, new)
  prov <- activate_provenance(.design)
  prov$record_track_external(new)
  .design
}

add_edibble_code <- function(.edibble, code) {
    if(!isFALSE(.edibble)) {
      if(is_edibble_design(.edibble)) {
        append_recipe_code(.edibble, code)
      } else {
        des <- edbl_design(.edibble) %>%
          append_recipe_code(code)
        attr(.edibble, "design") <- des
        .edibble
      }
    }
}


#' Rescale a numerical vector
#'
#' Similar to [scales::rescale()] but it has a different
#' behaviour when only upper or lower bound is given.
#'
#' @param x A numerical vector.
#' @param lower The lower bound.
#' @param upper The upper bound.
#' @export
rescale_values <- function(x, lower = NA, upper = NA) {
  minx <- min(x, na.rm = TRUE)
  maxx <- max(x, na.rm = TRUE)
  if(is.na(lower) & is.na(upper)) return(x)
  if(is.na(lower) & !is.na(upper)) {
    shift <- maxx - upper
    if(shift < 0) return(x)
    return(x - shift + .Machine$double.xmin)
  }
  if(!is.na(lower) & is.na(upper)) {
    shift <- minx - lower
    if(shift >= 0) return(x)
    return(x - shift + .Machine$double.xmin)
  }
  ret <- (x - minx) / (maxx - minx) * (upper - lower)
  ret[ret == min(ret)] <- ret[ret == min(ret)] + (upper - lower) * .Machine$double.eps
  ret[ret == max(ret)] <- ret[ret == max(ret)] - (upper - lower) * .Machine$double.eps
  ret
}

#' @export
print.edbl_fct <- function(x, ...) {
  vctrs::obj_print(x)
  invisible(x)
}

#' @export
`+.edbl` <- function(e1, e2) {
  if(missing(e2)) {
    cli::cli_abort(c("Cannot use {.code +} with a single argument",
                     i = "Did you accidentally put {.code +} on a new line?"))
  }
  if(is_edibble_design(e1) | is_edibble_table(e1)) {
    prov1 <- activate_provenance(e1)
    if(is_edibble_design(e2)) {
      prov2 <- activate_provenance(e2)
      # add factor nodes and edges from e2
      fnodes2 <- prov2$fct_nodes
      if(nrow(fnodes2)) {
        prov1$append_fct_nodes(name = fnodes2$name,
                               role = fnodes2$role,
                               attrs = fnodes2$attrs)
      }
      fedges2 <- prov2$fct_edges
      if(nrow(fedges2)) {
        from1 <- prov1$fct_id(name = prov2$fct_names(id = fedges2$from))
        to1 <- prov1$fct_id(name = prov2$fct_names(id = fedges2$to))
        needs_group_id <- !is.na(fedges2$group)
        if(sum(needs_group_id)) {
          prov1$append_fct_edges(from = from1[needs_group_id],
                                 to = to1[needs_group_id],
                                 type = fedges2$type[needs_group_id],
                                 group = TRUE,
                                 # FIXME: this needs to be modified if attrs is data.frame!
                                 attrs = fedges2$attrs[needs_group_id])
        } else if(sum(!needs_group_id)) {
          prov1$append_fct_edges(from = from1[!needs_group_id],
                                 to = to1[!needs_group_id],
                                 type = fedges2$type[!needs_group_id],
                                 group = FALSE,
                                 # FIXME: this needs to be modified if attrs is data.frame!
                                 attrs = fedges2$attrs[!needs_group_id])
        }
      }
      # add level nodes and edges from e2
      missing_cols <- function(col, df) {
        if(col %in% colnames(df)) {
          ret <- lnode[[col]]
        } else {
          ret <- NULL
        }
        ret
      }

      lnodes2 <- prov2$lvl_nodes
      if(length(lnodes2)) {
        for(cfid in names(lnodes2)) {
          fname <- prov2$fct_names(id = as.numeric(cfid))
          fid <- prov1$fct_id(name = fname)
          lnode <- lnodes2[[cfid]]
          prov1$append_lvl_nodes(value = lnode$value,
                                 n = missing_cols("n", lnode),
                                 attrs = missing_cols("attrs", lnode),
                                 label = missing_cols("label", lnode),
                                 fid = fid)
        }
      }
      ledges2 <- lvl_edges(e2)
      if(!is.null(ledges2)) {
        for(lnode in ledges2) {
          from <- prov1$lvl_id(value = lnode$val_from, fid = prov1$fct_id(name = lnode$var_from[1]))
          to <- prov1$lvl_id(value = lnode$val_to, fid = prov1$fct_id(name = lnode$var_to[1]))
          prov1$append_lvl_edges(from = from,
                                 to = to,
                                 attrs = lnode$attrs)
        }
      }

      # TODO: add these components when adding design
      # validation,
      # anatomy,
      # simulate,
      # context
      e1 <- return_edibble_with_graph(e1, prov1)
      des2 <- edbl_design(e2)
      for(code in des2$recipe[-1]) e1 <- add_edibble_code(e1, code)
      e1
    } else if(inherits(e2, "edbl_fn")) {
      e2$.edibble <- e1
      eval(e2, envir = attr(e2, "env"))
    } else if(inherits(e2, "edbl_fns")) {
      ret <- e1 + e2[[1]]
      if(length(e2) == 1L) return(ret)
      e2add <- structure(e2[-1], class = c("edbl_fns", "edbl"))
      ret + e2add
    } else {
      cli::cli_abort(paste("Not sure how to do deal with {.code +} for object of class",
                           class(e2)[1]))
    }
  } else if(inherits(e1, "edbl_fn")) {
    if(inherits(e2, "edbl_fn")) {
      structure(list(e1, e2), class = c("edbl_fns", "edbl"))
    } else if(inherits(e2, "edbl_fns")) {
      structure(c(list(e1), e2), class = c("edbl_fns", "edbl"))
    } else {
      cli::cli_abort(paste("Not sure how to do deal with {.code +} for object of class",
                           class(e2)[1]))
    }
  } else if(inherits(e1, "edbl_fns")) {
    if(inherits(e2, "edbl_fn")) {
      structure(c(e1, list(e2)), class = c("edbl_fns", "edbl"))
    } else if(inherits(e2, "edbl_fns")) {
      structure(c(e1, e2), class = c("edbl_fns", "edbl"))
    } else {
      cli::cli_abort(paste("Not sure how to do deal with {.code +} for object of class",
                           class(e2)[1]))
    }
  } else {
    cli::cli_abort(paste("Not sure how to do deal with {.code +} for object of class",
                         class(e1)[1]))
  }
}

#' @export
rbind.edbl_table <- function(...) {
  dplyr::bind_rows(...)
}
