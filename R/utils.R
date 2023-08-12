






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
  title <- title %||% prov$get_title()
  fids <- prov$fct_nodes$id
  fnames <- prov$fct_names(id = fids)

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
                   children <- prov$fct_id_child(id = id)
                   if(class!="edbl_trt" & !is_empty(children)) {
                     prov$fct_names(id = children)
                   } else {
                     character()
                   }
                 })
    nodes_with_parents <- unname(unlist(ll))
    label_names_with_levels <- paste(label_names, map_chr(var_nlevels, decorate_levels))
    label_names_with_levels[classes=="edbl_rcrd"] <- label_names[classes=="edbl_rcrd"]

    data <- data.frame(var = c("root", fids),
                       child = I(c(list(setdiff(fids, nodes_with_parents)), ll)),
                       label = c(decorate_title(title),
                                 label_names_with_levels))
  }
  cat(tree(data, root = "root"), sep = "\n")
  if(!is_null(x$allotment)) {
    cat(decorate_title("Allotment:\n"))
    s <- as.character(c(x$allotment$trts, x$allotment$units))
    tilde_pos <- unlist(gregexpr("~", s))
    tilde_pos_max <- max(tilde_pos)
    pad <- map_chr(tilde_pos_max - tilde_pos, function(n) ifelse(n==0, "", paste0(rep(" ", n), collapse = "")))
    cli_li(items = paste0(" ", pad, s))
  }
  if(!is_null(x$assignment)) {
    cat(decorate_title("Assignment:"), paste0(x$assignment, collapse = ", "), "\n")
  }
  if(!is_null(x$validation)) {
    cat(decorate_title("Validation:\n"))
    rnames <- names(x$validation)
    items <- map_chr(seq_along(x$validation), function(i) {
      paste0(rnames[i], ": ", style_italic(x$validation[[i]]$record), " ",
             validation_interval(x$validation[[i]]))
    })
    cli_li(items = items)
  }
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
print.edbl_graph <- function(x, show_levels = FALSE, ...) {
  cat(cli::col_green("factor nodes\n"))
  print(x$factors$nodes)
  cat(cli::col_green("factor edges\n"))
  print(x$factors$edges)
  if(show_levels) {
    cat(cli::col_blue("level nodes\n"))
    print(x$levels$nodes)
    cat(cli::col_blue("level edges\n"))
    print(x$levels$edges)
  }
}

names.edbl_graph <- function(graph) {
  graph$factors$nodes$name
}

names.edbl_design <- function(design) {
  names(design$graph)
}


remove_nulls <- function(.x) {
  .x[!vapply(.x, is.null, logical(1))]
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
    out <- rbind(df1, df2)
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
    paste0(floor(n * scale), symbol)
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
#'  i.e. don't coerce numeric factors.
as.data.frame.edbl_table <- function(x,
                                     levels_as = "factor",
                                     ignore_numeric = TRUE) {
  out <- lapply(x, function(.x) {
    if(is_edibble_unit(.x) | is_edibble_trt(.x)) {
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
