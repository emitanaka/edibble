
# `is` functions ------------------------------------------------------------

#' Test and get edibble objects
#'
#' @description
#' The `is` functions tests if an object (or an object in its attribute)
#' inherits particular class and returns `TRUE` if it does, otherwise `FALSE`.
#'
#' * `is_edibble_design` checks if it inherits `edbl_design`.
#' * `is_edibble_graph` checks if it inherits `edbl_graph`.
#' * `is_edibble_table` checks if it inherits `edbl_table`
#' * `is_edibble` checks if the object inherits `edbl`.
#'  The search is quite simple, it checks if
#' the object is `edbl_design`, failing that it looks to see if the
#' attribute "design" of the object is `edbl_design`.
#' * `is_named_design` check if it inherits `NamedDesign`.
#'
#' The `get` functions extracts the requested edibble component (table, graph,
#' or design) from the object if possible.
#'
#' * `edbl_design` tries to get `edbl_design`.
#' * `edbl_table` tries to get `edbl_table` with no design attribute.
#' * `edbl_graph` tries to get `edbl_graph`.
#'
#' @param x An object.
#' @name design-helpers
#' @examples
#' is_edibble_design(takeout())
#' @return A logical value.
#' @export
is_edibble_design <- function(x) {
  inherits(x, "edbl_design")
}

#' @rdname design-helpers
#' @export
is_named_design <- function(x) {
  inherits(x, "named_design")
}

#' @rdname design-helpers
#' @export
is_edibble_table <- function(x) {
  inherits(x, "edbl_table")
}

#' @rdname design-helpers
#' @export
is_edibble_graph <- function(x) {
  inherits(x, "edbl_graph")
}

#' @rdname design-helpers
#' @export
is_edibble <- function(x) {
  inherits(x, "edbl")
}

#' @rdname design-helpers
#' @export
is_edibble_levels <- function(x) {
  inherits(x, "edbl_lvls")
}

#' @rdname design-helpers
#' @export
is_nest_levels <- function(x) {
  inherits(x, "nest_lvls")
}

#' @rdname design-helpers
#' @export
is_cross_levels <- function(x) {
  inherits(x, "cross_lvls")
}




# `get` functions ---------------------------------------------------------

#' @rdname design-helpers
#' @export
edbl_design <- function(x) {
  if(is_edibble_design(x)) {
    x
  } else if(is_edibble_table(x)) {
    attr(x, "design")
  } else {
    abort(sprintf("An edibble design is not available in %s.",
                  deparse(substitute(x))))
  }
}

#' @rdname design-helpers
#' @export
edbl_table <- function(x) {
  if(is_edibble_design(x)) {
    return(x$table)
  } else if(is_edibble_table(x)) {
    return(x)
  } else {
    abort(sprintf("Do not know how to get table from %s.",
                  deparse(substitute(x))))
  }
}



# `not` functions -----------------------------------------------------------

not_edibble <- function(x) {
  if (!is_edibble(x)) {
    abort(sprintf("%s is not an edibble.", deparse(substitute(x))))
  }
}


#' An edibble table constructor
#'
#' @description
#' This helps to construct a new edibble table which is a special type
#' of tibble.
#'
#' @param .data data frame or list of the same size.
#' @param ... Passed to `new_tibble`.
#' @param .design An edibble graph object.
#' @param .class Subclasses for edibble table. The default is NULL.
#' @param units The data columns that are units.
#' @param trts The data columns that are treatments.
#' @importFrom tibble new_tibble
#' @importFrom vctrs vec_size_common
#' @return An edibble table.
#' @export
new_edibble <- function(.data, ..., .design = NULL, .class = NULL) {
  new_tibble(.data, ..., nrow = vec_size_common(!!!.data),
             class = c(.class, "edbl_table", "edbl"), design = .design)
}


new_trackable <- function(internal_cmd = character(),
                          time_internal = Sys.time(),
                          time_zone_internal = character(),
                          external_cmd = NULL,
                          time_external = NULL,
                          time_zone_external = NULL) {
  new_tibble(tibble::tibble(internal_cmd = internal_cmd,
                            execution_time = time_internal,
                            time_zone = time_zone_internal),
             class = "trck_table",
             external_cmd = external_cmd,
             execution_time = time_external,
             time_zone = time_zone_external)
}

#' @export

tbl_sum.trck_table <- function(x) {
  c("A tracking table" = dim_desc(x),
    "External command" = attr(x, "external_cmd"),
    "Execution time" = paste(as.character(attr(x, "execution_time")),
                             as.character(attr(x, "time_zone"))))
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.edbl_table <- function(x) {
  c("An edibble" = dim_desc(x))
}

#' @export
print.edbl_table <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  prov <- activate_provenance(x)
  title <- prov$get_title()
  format_title <- style_subtle(paste("#", cli::style_bold(title)))
  if(!is.null(title)) cat(format_title, "\n")
  NextMethod()
}


#' @rdname new_edibble
#' @export
as_edibble <- function(.data, ...) {
  UseMethod("as_edibble")
}

#' @export
as_edibble.data.frame <- function(.data, title = NULL, name = "edibble", .record = TRUE, seed = NULL, provenance = Provenance$new(), units = NULL, trts = NULL, ...) {
  if(.record) provenance$record_step()
  des <- design(title = title, name = name, .record = FALSE, seed = seed, provenance = provenance)
  new_edibble(.data, ..., .design = des) %>%
    set_units({{units}}) %>%
    set_trts({{trts}})
}

# idk what's the point of this function
# should this be removed?
edibble <- function(.data, title = NULL, name = "edibble", .record = TRUE, seed = NULL, provenance = Provenance$new(), ...) {
  if(.record) provenance$record_step()
  des <- design(title = title, name = name, .record = FALSE, seed = seed, provenance = provenance)
  new_edibble(.data, ..., .design = des)
}



#' Restart the edibble design
#'
#' @description
#' This restarts the edibble design after initiating the design using
#' [edibble()].
# restart_design <- function(.data) {
#   not_edibble_table(.data)
#   attr(.data, "design")
# }

