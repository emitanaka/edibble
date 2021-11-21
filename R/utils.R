
#' Check if the design contains a particular variable label.
#'
#'
#' @param design An edibble design.
#' @param label A character vector of the variable labels.
#' @return A logical vector of the same size as `label` or a single logical value if class is provied.
#' @export
var_exists <- function(design, label = NULL) {
  if(is_null(label)) {
    abort("`label` must be supplied in `var_exists`.")
  }
  label %in% fct_label(design)
}

template_var_exists <- function(design, label = NULL, class = NULL, all_labels = fct_label(design)) {
  if(is_null(label)) {
    class %in% fct_class(design)
  } else {
    vexists <- label %in% all_labels
    if(!all(vexists)) {
      abort_missing_vars(label[!vexists])
    }
    vexists
  }
}

trts_exists <- function(design, label = NULL) {
  template_var_exists(design, label = label, class = "edbl_trt", all_labels = trt_labels(design))
}

units_exists <- function(design, label = NULL) {
  template_var_exists(design, label = label, class = "edbl_unit", all_labels = unit_labels(design))
}

rcrds_exists <- function(design, label = NULL) {
  template_var_exists(design, label = label, class = "edbl_rcrd", all_labels = rcrd_labels(design))
}

check_trt_exists <- function(design, label = NULL) {
  if(!trts_exists(design)) {
    abort("No treatment variables exists in the design.")
  }
}

check_rcrd_exists <- function(design, label = NULL) {
  if(!rcrds_exists(desgin)) {
    abort("No record variables exists in the design.")
  }
}

check_unit_exists <- function(design, label = NULL) {
  if(!units_exists(design)) {
    abort("No unit variables exists in the design.")
  }
}

check_var_exists <- function(design, label = NULL, vclass = NULL) {
  f_exists <- switch(vclass,
                     "edbl_trt" = trts_exists,
                     "edbl_unit" = units_exists,
                     "edbl_rcrd" = rcrds_exists)
  vexists <- f_exists(design, label)
  if(is_null(label) && any(!vexists)) {
    abort(sprintf("No variables with class `%s` exists.", vclass))
  }

  if(any(!vexists)) {
    abort_missing_vars(label[!vexists])
  }
}

abort_missing_vars <- function(missing_vars) {
  abort(sprintf("%s does not exist in the design.",
                .combine_words(paste0("`", missing_vars, "`"))))
}


decorate_vars <- function(x, decorate_units, decorate_trts, decorate_resp, classes) {
  edbl_classes <- c("edbl_unit", "edbl_trt", "edbl_resp")
  decorate_fns <- list(decorate_units, decorate_trts, decorate_resp)
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
#' @param decorate_trts,decorate_units,decorate_resp,decorate_levels,decorate_title
#' A function applied to the name of treatment, unit, response factors or
#' design title. The function should return a string. Most often this wraps the name with
#' ANSI colored text. Run [edibble_opt()] to see the list of default
#' values for the options.
#' @param title The title of the design.
#' @param ... Unused.
#'
#' @examples
#' # stylizing are only visible in terminal output that supports it
#' print(nclassics$split)
#' ## Split plot design
#' ## ├─mainplot (4 levels)
#' ## │ └─subplot (8 levels)
#' ## ├─subplot (8 levels)
#' ## ├─variety (2 levels)
#' ## └─irrigation (2 levels)
#' @importFrom cli tree cli_li
#' @export
print.edbl_design <- function(x,
                              decorate_units  = edibble_decorate("units"),
                              decorate_trts   = edibble_decorate("trts"),
                              decorate_resp   = edibble_decorate("resp"),
                              decorate_levels = edibble_decorate("levels"),
                              decorate_title  = edibble_decorate("title"),
                              title = NULL, ...) {
  title <- title %||% x$name
  fnames <- names(x)

  if(is_empty(fnames)) {
    data <- data.frame(var = "root",
                       child = NA,
                       label = as.character(decorate_title(title)))
  } else {

    classes <- fct_class(x)
    label_names <- decorate_vars(fnames,
                                 decorate_units,
                                 decorate_trts,
                                 decorate_resp,
                                 classes)
    var_nlevels <- lengths(fct_levels(x)[fnames])
    nvar <- length(fnames)
    ll <- lapply(fnames,
                 function(v) {
                   id <- fct_id(x, v)
                   class <- fct_class(x, id = id)
                   children <- fct_child(x, id = id)
                   if(class!="edbl_trt" & !is_empty(children)) {
                     fct_label(x, id = children)
                   } else {
                     character()
                   }
                 })
    nodes_with_parents <- unname(unlist(ll))

    data <- data.frame(var = c("root", fnames),
                       child = I(c(list(setdiff(fnames, nodes_with_parents)), ll)),
                       label = c(decorate_title(title),
                                 paste(label_names, map_chr(var_nlevels, decorate_levels))))
  }
  cat(tree(data, root = "root"), sep = "\n")
  if(!is_null(x$allotment)) {
    cat(decorate_title("Allotment:\n"))
    s <- as.character(x$allotment)
    tilde_pos <- unlist(gregexpr("~", s))
    tilde_pos_max <- max(tilde_pos)
    pad <- map_chr(tilde_pos_max - tilde_pos, function(n) ifelse(n==0, "", paste0(rep(" ", n), collapse = "")))
    cli_li(items = paste0(" ", pad, s))
  }
  if(!is_null(x$assignment)) {
    cat(decorate_title("Assignment:"), x$assignment, "\n")
  }
  if(!is_null(x$validation)) {
    cat(decorate_title("Validation:\n"))
    rnames <- names(x$validation)
    #browser()
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

#' @export
print.edbl_graph <- function(x, show_levels = FALSE, ...) {
  cat(cli::col_green("factor nodes\n"))
  print(x$nodes)
  cat(cli::col_green("factor edges\n"))
  print(x$edges)
  if(show_levels) {
    cat(cli::col_blue("level nodes\n"))
    print(x$levels$nodes)
    cat(cli::col_blue("level edges\n"))
    print(x$levels$edges)
  }
}

names.edbl_graph <- function(graph) {
  graph$nodes$label
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


#' Find how many digits
ndigits <- function(x) {
  max(c(floor(log10(abs(x))) + 1, edibble_labels_opt("min_ndigits")))
}


is_nested_unit <- function(design, uid) {
  unit_ids <- fct_nodes_filter(design, class == "edbl_unit")$id
  out <- fct_edges_filter(design, to %in% uid & from %in% unit_ids)
  nrow(out) > 0
}


# number SI prefix --------------------------------------------------------

#' Numbers with SI prefix
#'
#' It's called SI prefix but the letter notation is added as a suffix.
#' The largest prefix is yotta $10^{24}$.
#'
#' @param x A numeric vector to format with SI prefix
#' @source https://en.wikipedia.org/wiki/Metric_prefix
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
