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
  provenance$save_seed(seed, type = "design")
  structure(list(graph = provenance$get_graph(),
                 validation = list(rcrds = NULL),
                 provenance = provenance,
                 anatomy = NULL,
                 recipe = NULL,
                 simulate = list(),
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
  lnodes <- list()
  fedges <- tibble::tibble(from = integer(), to = integer(),
                            type = character(), group = integer(),
                            attrs = data.frame())
  ledges <-  tibble::tibble(from = integer(), to = integer(),
                            attrs = data.frame())
  new_edibble_graph(fnodes, lnodes, fedges, ledges)
}



new_edibble_graph <- function(fnodes = NULL, lnodes = NULL, fedges = NULL, ledges = NULL) {
  structure(list(factors = list(nodes = fnodes,
                                edges = fedges),
                 levels = list(nodes = lnodes,
                               edges = ledges)),
            class = "edbl_graph")
}


#' A baseline model for given experimental design
#'
#' This
#'
#' @param data An edibble data.
#' @param type The type of model expression to return.
#' @export
design_model <- function(data, type = c("anova", "lmer")) {
  warn("An appropriate model for analysis requires context and diagnosis.\nDon't take this suggested baseline model without appropriate thinking.")
  prov <- activate_provenance(data)
  des <- edbl_design(data)
  type <- match.arg(type)
  unit_str <- des$anatomy
  units <- attr(terms(unit_str), "term.labels")
  trt_str <- paste0(prov$trt_names(), collapse = "*")
  rnames <- prov$rcrd_names()
  map_to_units <- prov$mapping_to_unit(id = prov$fct_id(name = rnames))
  smallest_unit <- prov$fct_id_leaves(role = "edbl_unit")[1]
  rclass <- prov$rcrd_class(rnames)
  rname <- "y"
  if(length(rnames)) rname <- rnames[which(map_to_units == smallest_unit)][1]
  if("numeric" %in% rclass) rname <- rnames[!is.na(rclass) & rclass=="numeric" & map_to_units == smallest_unit][1]
  res <- switch(type,
         anova = {
           sprintf("aov(%s ~ %s + Error(%s), data = .)\n",
                            rname, trt_str, paste0(units[-length(units)], collapse = " + "))
         },
         lmer = {
           rstr <- paste0(paste0("(1|", units[-length(units)], ")"), collapse = " + ")
           sprintf("lme4::lmer(%s ~ %s + %s, data = .)\n",
                   rname, trt_str, rstr)
         })
  cat(res)
  invisible(parse(text = res))
}
