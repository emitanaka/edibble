#' Start the edibble design
#'
#' @description
#' This function doesn't really do much besides create a new edibble design object.
#'
#' @param name Optional name used as title for printing the design.
#' @inheritParams set_units
#' @param seed A seed number for reproducibility.
#' @param .data An edibble table.
#' @param kitchen An environment setup in a manner to manipulate, extract and query
#'   information on the design.
#' @return An empty `edbl_design` object.
#' @examples
#' design("My design")
#' @seealso Add variables to this design with [set_units()], [set_trts()], and
#' [set_rcrds()].
#' @family user-facing functions
#' @export
design <- function(name = NULL, .record = TRUE, seed = NULL, kitchen = Kitchen) {
  if(.record) record_step()
  save_seed(seed)
  structure(list(name = name,
                 graph = empty_edibble_graph(),
                 kitchen = kitchen),
            class = c("edbl_design", "edbl"))
}

#' @rdname design
#' @export
redesign <- function(.data, name = NULL, .record = TRUE, seed = NULL, kitchen = Kitchen, ...) {
  des <- design(name = name, .record = .record, seed = seed, kitchen = kitchen)
  new_edibble(.data, ..., design = des)
}

# initialise graph structure -----------------------------------------------

empty_edibble_graph <- function() {
  fnodes <- tibble::tibble(id = integer(),
                           role = character(),
                           name = character(),
                           attrs = list())
  lnodes <- list()
  fedges <-  tibble::tibble(from = integer(), to = integer(),
                            type = character(), group = integer(),
                            attrs = list())
  ledges <-  tibble::tibble(from = integer(), to = integer(),
                            attrs = list())
  structure(list(factors = list(nodes = fnodes,
                                edges = fedges),
                 levels = list(nodes = lnodes,
                               edges = ledges)),
            class = "edbl_graph")
}

new_lnode <- function(ids, vals, data) {
  tibble::tibble(id = ids,
                 value = vals,
                 attrs = data)
}
