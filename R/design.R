#' Start the edibble design
#'
#' @description
#' This function doesn't really do much besides create a new edibble design object.
#'
#' @param title Optional title of the experiment.
#' @param name Optional name of the experiment.
#' @inheritParams set_units
#' @param seed A seed number for reproducibility.
#' @param .data An edibble table.
#' @param provenance An environment setup in a manner to store methods and
#'   information to trace the origin of the design
#' @return An empty `edbl_design` object.
#' @examples
#' design("My design")
#' @seealso Add variables to this design with [set_units()], [set_trts()], and
#' [set_rcrds()].
#' @family user-facing functions
#' @export
design <- function(title = NULL, name = "edibble", .record = TRUE, seed = NULL, provenance = Provenance$new()) {
  if(.record) provenance$record_step()
  if(!is.null(title)) provenance$set_title(title)
  provenance$set_name(name)
  provenance$save_seed(seed)
  structure(list(graph = provenance$get_graph(),
                 provenance = provenance,
                 anatomy = NULL,
                 recipe = NULL,
                 simulate = NULL,
                 context = NULL),
            class = c("edbl_design", "edbl"))
}

#' @rdname design
#' @export
redesign <- function(.data, name = NULL, .record = TRUE, seed = NULL, provenance = provenance, ...) {
  des <- design(name = name, .record = .record, seed = seed, provenance = provenance)
  new_edibble(.data, ..., design = des)
}

# initialise graph structure -----------------------------------------------

empty_edibble_graph <- function() {
  fnodes <- tibble::tibble(id = integer(),
                           role = character(),
                           name = character(),
                           attrs = data.frame())
  lnodes <- structure(list(), class = c("edbl_lnodes", "list"))
  fedges <- tibble::tibble(from = integer(), to = integer(),
                            type = character(), group = integer(),
                            attrs = data.frame())
  ledges <-  tibble::tibble(from = integer(), to = integer(),
                            attrs = data.frame())
  new_edibble_graph(fnodes, lnodes, fedges, ledges)
}


as.list.edbl_lnodes <- function(x, ...) {
  class(x) <- unique(c(setdiff(class(x), "edbl_lnodes"), "list"))
  x
}

# I don't know if this is a good idea but the level nodes are stored
# as a list of nodes
# below replaces some common `extract` options to make it feel like
# a data.frame instead

#' Extract or replace parts of the level nodes
#'
#' The level nodes are stored as a named list of nodes where the name
#' corresponds to the id of the corresponding factor. This makes the
#' access of level nodes slightly awkward. For example, to extract the
#' id of the level nodes, you have to iterate over every list.
#'
#' @examples
#' crd <- takeout(menu_crd())
#'
#' @name extract-lvl-nodes
NULL

#' @rdname extract-lvl-nodes
#' @param x An edibble level nodes.
#' @param name The name to extract.
#' @export
"$.edbl_lnodes" <- function(x, name) {
  unname(unlist(lapply(unclass(x), function(.x) .x[[name]])))
}

#' @param i The index.
#' @param ... Unused
#' @rdname extract-lvl-nodes
#' @export
"[.edbl_lnodes" <- function(x, i, ...) {
  lx <- as.list(x)
  if(is.numeric(i)) {
    structure(lx[as.character(i)], class = class(x))
  } else {
    structure(lx[i], class = class(x))
  }
}

#' @param value The value to replace with.
#' @rdname extract-lvl-nodes
#' @export
"[<-.edbl_lnodes" <- function(x, i, ..., value) {
  lx <- as.list(x)
  browser()
  if(is.numeric(i)) {
    lx[as.character(i)] <- value
  } else {
    lx[i] <- value
  }
  invisible(structure(lx, class = class(x)))
}

#' @rdname extract-lvl-nodes
#' @keywords internal
#' @export
"[[.edbl_lnodes" <- function(x, i, ...) {
  lx <- as.list(x)
  if(is.numeric(i)) {
    lx[[as.character(i)]]
  } else {
    lx[[i]]
  }
}

#' @rdname extract-lvl-nodes
#' @export
"[[<-.edbl_lnodes" <- function(x, i, ..., value) {
  lx <- as.list(x)
  if(is.numeric(i)) {
    lx[[as.character(i)]] <- value
  } else {
    lx[[i]] <- value
  }
  invisible(structure(lx, class = class(x)))
}

new_edibble_graph <- function(fnodes = NULL, lnodes = NULL, fedges = NULL, ledges = NULL) {
  if(!is_null(lnodes) && !inherits(lnodes, "edbl_lnodes")) class(lnodes) <- c("edbl_lnodes", class(lnodes))
  structure(list(factors = list(nodes = fnodes,
                                edges = fedges),
                 levels = list(nodes = lnodes,
                               edges = ledges)),
            class = "edbl_graph")
}


